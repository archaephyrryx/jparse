{-# LANGUAGE BangPatterns #-}

module Gates where

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Morph (MFunctor(..))

import qualified Data.ByteString.Streaming as BS

import Sources

import JParse.Channels
import JParse.Global

import Control.Concurrent.Async

type IsGated = Bool
type IsZipped = Bool

produce :: BS.ByteString IO () -> BS.ByteString IO ()
produce mbs = do
  outgate <- liftIO $ newChanBounded uBound_gate
  liftIO $ async $ writeBS outgate mbs
  readBS outgate


generate :: IsGated -> IsZipped -> Maybe String -> BS.ByteString IO ()
generate _     zipped Nothing   = condUnzip getStdin zipped
generate False zipped (Just !u) = hoist runResourceT $ condUnzip (getHttp u) zipped
generate True  False  (Just !u) = hoist runResourceT $ getHttp u
generate True  True   (Just !u) = do
  gate <- liftIO $ newChanBounded uBound_gate
  httpThread <- liftIO $ async $ runResourceT $ writeBS gate $ getHttp u
  liftIO $ link httpThread
  Sources.unzip $ readBS gate
