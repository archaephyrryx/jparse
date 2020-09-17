{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module JParse.Driver.Internal where

import Control.Concurrent.STM.TVar

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D
import           Data.ByteString.Builder (Builder)

import Streaming
import qualified Data.ByteString.Streaming as BS

import JParse.Channels
import JParse.Global

-- | set of common synchronization values for concurrent linemode
data ZEnv
   = ZEnv
     { nworkers :: Int -- ^ number of workers (runtime)
     , nw :: TVar Int -- ^ number of unterminated worker threads
     , input :: ChanBounded L.ByteString -- ^ channel for unparsed input
     , output :: ChanBounded (Maybe B.ByteString) -- ^ channel for parsed output
     }

newZEnv :: IO ZEnv
newZEnv = do
  nworkers <- nWorkers
  nw <- newTVarIO nworkers
  input <- newChanBounded uBound_work
  output <- newChanBounded uBound_work
  return ZEnv{..}

toStricts :: Monad m => Stream (BS.ByteString m) m r -> Stream (Of B.ByteString) m r
toStricts = mapped _toStrict
  where
    _toStrict :: Monad m => BS.ByteString m r -> m (Of B.ByteString r)
    _toStrict mbs = do
      (lbs :> ret) <- BS.toLazy mbs
      return (L.toStrict lbs :> ret)
    {-# INLINE _toStrict #-}
{-# INLINE toStricts #-}
