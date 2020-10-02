{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module JParse.Zepto.Internal where

import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D

import Data.ByteString.Streaming.Internal (ByteString(..))
import qualified Data.ByteString.Streaming as BS

import Streaming
import qualified Streaming.Prelude as S
import Streaming.Internal (Stream(..))

import JParse.Global
import JParse.Channels

-- Concurrency mode
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- | set of common synchronization values for concurrent linemode
data ZEnv f a
   = ZEnv
     { nworkers :: Int -- ^ number of workers (runtime)
     , nw       :: TVar Int -- ^ number of unterminated worker threads
     , input    :: ChanBounded L.ByteString -- ^ channel for unparsed input
     , output   :: ChanBounded (f a) -- ^ channel for parsed output
     }

-- | Generate a new 'ZEnv' object in the 'IO' monad
newZEnv :: IO (ZEnv f a)
newZEnv = do
  nworkers <- nWorkers
  nw <- newTVarIO nworkers
  input <- newChanBounded uBound_work
  output <- newChanBounded uBound_work
  return ZEnv{..}
