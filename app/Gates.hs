{-# LANGUAGE BangPatterns #-}

module Gates where

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Morph (MFunctor(..))

import qualified Data.ByteString           as B
import qualified Data.ByteString.Streaming as BS

import Sources

import JParse.Channels
import JParse.Global

import Control.Concurrent.Async

type IsGated = Bool
type IsZipped = Bool

produce :: (ChanBounded B.ByteString -> IO (BS.ByteString IO ())) -> IO (BS.ByteString IO ())
produce mf = do
  outgate <- newChanBounded uBound_gate
  mf outgate

generate :: IsGated -> IsZipped -> Maybe String
         -> (ChanBounded B.ByteString -> IO (BS.ByteString IO ()))
generate _     zipped Nothing   = \outgate -> do
  async $ writeBS outgate $ condUnzip zipped getStdin
  return $ readBS outgate
generate False zipped (Just !u) = \outgate -> do
  link =<< async (runResourceT $ writeBS outgate . condUnzip zipped =<< getHttp u)
  return $ readBS outgate
generate True  False  (Just !u) = \outgate -> do
  link =<< async (runResourceT $ writeBS outgate =<< getHttp u)
  return $ readBS outgate
generate True  True   (Just !u) = \outgate -> do
  gate  <- newChanBounded uBound_gate
  link =<< async (runResourceT $ writeBS gate =<< getHttp u)
  link =<< async (writeBS outgate $ Sources.unzip $ readBS gate)
  return $ readBS outgate
