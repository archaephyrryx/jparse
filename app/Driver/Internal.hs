{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Driver.Internal where

import Data.Semigroup ((<>))

import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D

import Data.Either (isLeft, fromLeft)

import qualified Conduit as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit ((.|))

import Data.ByteString.Streaming.Internal (ByteString(..))
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import Streaming
import qualified Streaming.Prelude as S
import Streaming.Internal (Stream(..))

import qualified Streaming.Zip as Zip

import JParse
import Parse

import qualified Parse.ReadAlt as Alt

import qualified Parse.ReadZepto as Zep
import qualified Parse.Parser.Zepto as Z

import qualified Parse.ReadStream as ZepS
import qualified Parse.Parser.ZeptoStream as ZS

import Final
import Streams
import Global


import Data.Vector (Vector)
import qualified Data.Vector as V

-- Concurrency mode
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (replicateM_, unless, when)
import System.IO (stdout)
import System.Environment

import qualified Control.Concurrent.BoundedChan as BC
import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan)

{-
data ChanBounded a =
     ChanBounded { chan :: Chan a
                 , tlim :: TVar Int
                 , nlim :: !Int
                 }
-}
type ChanBounded a = BoundedChan a

newChanBounded :: Int -> IO (ChanBounded a)
newChanBounded = newBoundedChan
{-
newChanBounded nlim = do
    chan <- newChan
    tlim <- newTVarIO 0
    return ChanBounded{..}
-}
{-# INLINE newChanBounded #-}

writeChanBounded :: ChanBounded a -> a -> IO ()
writeChanBounded = BC.writeChan
{-
writeChanBounded ChanBounded{..} val = do
  atomically $ do
    level <- readTVar tlim
    when (level >= nlim) retry
  atomically $ modifyTVar tlim succ
  writeChan chan val
-}
{-# INLINE writeChanBounded #-}

readChanBounded :: ChanBounded a -> IO a
readChanBounded = BC.readChan
{-
readChanBounded ChanBounded{..} = do
  atomically $ modifyTVar tlim pred
  readChan chan
-}
{-# INLINE readChanBounded #-}


-- | number of worker threads to run Zepto parsing in parallel
nWorkers :: IO Int
nWorkers = getNumCapabilities
{-# INLINE nWorkers #-}

-- | Upper bound on number of unprocessed items in an input flow-control gate
uBound_gate :: Int
uBound_gate = 64
{-# INLINE uBound_gate #-}

-- | Upper bound on number of unprocessed items in produce-consumer channels
uBound_work :: Int
uBound_work = 128
{-# INLINE uBound_work #-}

-- | Conversion between Chan and Stream
feedChan :: (MonadIO m, Monoid a) => Chan a -> Stream (Of a) m r -> m ()
feedChan chan = go
  where
    go stream =
      S.uncons stream >>= \case
        Just (x, rest) -> liftIO (writeChan chan x) >> go rest
        Nothing -> liftIO (writeChan chan mempty)
{-# INLINE feedChan #-}

drainChan :: (MonadIO m, Monoid a, Eq a) => Chan a -> Stream (Of a) m ()
drainChan chan = go
  where
    go = do
      x <- liftIO $ readChan chan
      if x == mempty then Return () else Step (x :> go)
{-# INLINE drainChan #-}

feedChanBounded :: (MonadIO m, Monoid a) => ChanBounded a -> Stream (Of a) m r -> m ()
feedChanBounded cb = go
  where
    go stream =
      S.uncons stream >>= \case
        Just (x, rest) -> liftIO (writeChanBounded cb x) >> go rest
        Nothing -> liftIO $ BC.writeChan cb mempty
{-# INLINE feedChanBounded #-}

drainChanBounded :: (MonadIO m, Monoid a, Eq a) => ChanBounded a -> Stream (Of a) m ()
drainChanBounded cb = go
  where
    go = do
      x <- liftIO $ readChanBounded cb
      if x == mempty then Return () else Step (x :> go)
{-# INLINE drainChanBounded #-}

-- | Abstraction around list or vector of elements
type Bundle a = Vector a

foldrBundle :: (a -> b -> b) -> b -> Bundle a -> b
foldrBundle = V.foldr
{-# INLINE foldrBundle #-}

nullBundle :: Bundle a -> Bool
nullBundle = V.null
{-# INLINE nullBundle #-}

-- | set of common synchronization values for concurrent linemode
data ZEnv
   = ZEnv
     { nworkers :: Int -- ^ number of workers (runtime)
     , nw :: TVar Int -- ^ number of unterminated worker threads
     , input :: ChanBounded (Bundle L.ByteString) -- ^ channel for unparsed input
     , output :: ChanBounded (Maybe B.ByteString) -- ^ channel for parsed output
     }

newZEnv :: IO ZEnv
newZEnv = do
  nworkers <- nWorkers
  nw <- newTVarIO nworkers
  input <- newChanBounded uBound_work
  output <- newChanBounded uBound_work
  return ZEnv{..}

-- Builder->Strict ByteString conversion
build :: Builder -> B.ByteString
build = L.toStrict . D.toLazyByteStringWith buildStrat L.empty
  where
    buildStrat = D.untrimmedStrategy 2048 4096
    {-# INLINE buildStrat #-}
{-# INLINE build #-}

toStricts :: Monad m => Stream (BS.ByteString m) m r -> Stream (Of B.ByteString) m r
toStricts = S.mapped _toStrict
  where
    _toStrict :: Monad m => BS.ByteString m r -> m (Of B.ByteString r)
    _toStrict mbs = do
      (lbs :> ret) <- BS.toLazy mbs
      return (L.toStrict lbs :> ret)
    {-# INLINE _toStrict #-}
{-# INLINE toStricts #-}
