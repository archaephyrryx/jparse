{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Streaming.Gates (generate) where

import Prelude hiding (unzip)

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString           as B
import qualified Data.ByteString.Streaming as BS

import Data.ByteString.Streaming.Sources

import JParse.Channels
import JParse.Global

import Control.Concurrent.Async

type IsGated = Bool
type IsZipped = Bool

-- supplies closures with an output channel to act as a final 'gate' mechanism
--
-- Allows generation of a monadic ByteString to be handled in a separate thread
-- from input processing in order to isolate throughput bottlenecks.
produce :: (ChanBounded B.ByteString -> IO (BS.ByteString IO ())) -> IO (BS.ByteString IO ())
produce mf = do
  outgate <- newChanBounded uBound_work
  mf outgate

-- | Generates a monadic 'BS.ByteString' in the 'IO' monad from different types of input source
-- and format.
--
-- 'Gating' tends to yield marginal performance improvements for higher thread-counts,
-- and especially when the source is zlib-compressed data retrieved over http(s). When parallel
-- capabilities are limited or the input is not zlib-compressed http data, gating may instead
-- inflate memory usage with zero (or possibly negative) change in performance.
generate :: IsGated  -- ^ Indicates whether 'gating' is desired
         -> IsZipped -- ^ Indicates whether input stream is zlib-compressed
         -> Maybe String -- ^ Optional http(s) URL to retrieve data from, otherwise reading stdin
         -> IO (BS.ByteString IO ()) -- ^ Output monadic 'BS.ByteString' consisting of raw JSON data
generate _     False  Nothing   = return $ getStdin
generate False True   Nothing   = return $ unzip getStdin
generate True  True   Nothing   = produce $ \outgate -> do
  async $ writeBS outgate $ unzip getStdin
  return $ readBS outgate
generate False zipped (Just !u) = produce $ \outgate -> do
  link =<< async (runResourceT $ writeBS outgate . condUnzip zipped =<< getHttp u)
  return $ readBS outgate
generate True  False  (Just !u) = produce $ \outgate -> do
  link =<< async (runResourceT $ writeBS outgate =<< getHttp u)
  return $ readBS outgate
generate True  True   (Just !u) = produce $ \outgate -> do
  gate  <- newChanBounded uBound_gate
  link =<< async (runResourceT $ writeBS gate =<< getHttp u)
  link =<< async (writeBS outgate $ unzip $ readBS gate)
  return $ readBS outgate
