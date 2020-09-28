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

produce :: (ChanBounded B.ByteString -> IO (BS.ByteString IO ())) -> IO (BS.ByteString IO ())
produce mf = do
  outgate <- newChanBounded uBound_work
  mf outgate

generate :: IsGated -> IsZipped -> Maybe String -> IO (BS.ByteString IO ())
generate _     False  Nothing   = return $ getStdin
generate False True   Nothing   = return $ unzip getStdin
-- generate _  zipped Nothing   = return $ condUnzip zipped getStdin
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
