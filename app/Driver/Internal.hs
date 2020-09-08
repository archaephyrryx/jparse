{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Driver.Internal where

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

import Global

import Data.Vector (Vector)
import qualified Data.Vector as V

-- Concurrency mode
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import qualified Control.Concurrent.BoundedChan as BC
import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan)

type ChanBounded a = BoundedChan a

newChanBounded :: Int -> IO (ChanBounded a)
newChanBounded = newBoundedChan
{-# INLINE newChanBounded #-}

writeChanBounded :: ChanBounded a -> a -> IO ()
writeChanBounded = BC.writeChan
{-# INLINE writeChanBounded #-}

readChanBounded :: ChanBounded a -> IO a
readChanBounded = BC.readChan
{-# INLINE readChanBounded #-}

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

writeBS :: MonadIO m => ChanBounded B.ByteString -> BS.ByteString m r -> m ()
writeBS cb = go
  where
    go = \case
      Empty _ -> liftIO $ writeChanBounded cb B.empty
      Chunk b mb -> (liftIO $ writeChanBounded cb b) >> go mb
      Go m -> m >>= go
{-# INLINE writeBS #-}

readBS :: MonadIO m => ChanBounded B.ByteString -> BS.ByteString m ()
readBS cb = go
  where
    go = Go $ do
      b <- liftIO $ readChanBounded cb
      if B.null b
        then return (Empty ())
        else return $ Chunk b go
{-# INLINE readBS #-}

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
