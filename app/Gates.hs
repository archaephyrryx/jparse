{-# LANGUAGE BangPatterns #-}

module Gates (IsGated, IsZipped, generate) where

import Prelude hiding (unzip)

import Control.Monad.Trans.Reader (ReaderT(..), asks)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString      as B
import qualified Streaming.ByteString as BS

import Sources

import JParse.Channels
import JParse.Global

import Control.Concurrent.Async

-- | Semantically transparent alias for 'Bool' when used to indicate \'gated\' parameter
type IsGated = Bool

-- | Semantically transparent alias for 'Bool' when used to indicate \'zipped\' parameter
type IsZipped = Bool

-- supplies closures with an output channel to act as a final 'gate' mechanism
--
-- Allows generation of a 'BS.ByteStream' to be handled in a separate thread
-- from input processing in order to isolate throughput bottlenecks.
produce :: (Int -> BoundedChan B.ByteString -> IO (BS.ByteStream IO ()))
        -> ReaderT GlobalConf IO (BS.ByteStream IO ())
produce mf = do
  uBound_work <- asks workLimit
  lift $ newBoundedChan uBound_work >>= mf uBound_work

-- | Generates a monadic 'BS.ByteString' in the 'IO' monad from different types of input source
-- and format.
--
-- \'Gating\' tends to yield marginal performance improvements for higher thread-counts,
-- and especially when the source is zlib-compressed data retrieved over http(s). When parallel
-- capabilities are limited or the input is not zlib-compressed http data, gating may instead
-- inflate memory usage with zero (or possibly negative) change in performance.
generate :: IsGated  -- ^ Indicates whether \'gating\' is desired (Alias for 'Bool')
         -> IsZipped -- ^ Indicates whether input stream is zlib-compressed (Alias for 'Bool')
         -> Maybe String -- ^ Optional http(s) URL to retrieve data from, otherwise reading stdin
         -> ReaderT GlobalConf IO (BS.ByteStream IO ()) -- ^ 'GlobalConf' 'ReaderT' around output monadic 'BS.ByteString' consisting of raw JSON data
generate _     False  Nothing   = return $ getStdin
generate False True   Nothing   = return $ unzip getStdin
generate True  True   Nothing   = produce $ \_ outgate -> do
  link =<< async (writeBS outgate $ unzip getStdin)
  return (readBS outgate)
generate False zipped (Just !u) = produce $ \_ outgate -> do
  link =<< async (runResourceT $ writeBS outgate . condUnzip zipped =<< getHttp u)
  return $ readBS outgate
generate True  False  (Just !u) = produce $ \_ outgate -> do
  link =<< async (runResourceT $ writeBS outgate =<< getHttp u)
  return $ readBS outgate
generate True  True   (Just !u) = produce $ \uBound_work outgate -> do
  gate  <- newBoundedChan uBound_work
  link =<< async (runResourceT $ writeBS gate =<< getHttp u)
  link =<< async (writeBS outgate $ unzip $ readBS gate)
  return $ readBS outgate
