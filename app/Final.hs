{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Final (toVector, toVectorIO, toVectorsIO, toVectorsLBS) where

import Data.Monoid (Monoid(..))
import Data.Semigroup ((<>))

import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString (ByteString)

import Data.Either (isLeft, fromLeft)

import Streaming
import qualified Streaming.Prelude as S
import Streaming.Internal (Stream(..))
import Data.Vector (Vector)
import Data.Functor ((<$>))
import qualified Data.Vector as V

import Control.Monad.Trans.State.Strict
import Data.IORef

unconsFinal :: Monad m => Stream (Of a) m r -> StateT r m (Maybe (a, Stream (Of a) m r))
unconsFinal = loop
  where
    loop stream = case stream of
      Return r -> put r >> pure Nothing
      Effect m -> lift m >>= loop
      Step (a :> rest) -> pure $ Just (a, rest)
{-# INLINE unconsFinal #-}

unconsFinalIO :: MonadIO m => IORef r -> Stream (Of a) m r -> m (Maybe (a, Stream (Of a) m r))
unconsFinalIO ret = loop
  where
    loop stream = case stream of
      Return r -> liftIO (writeIORef ret r) >> pure Nothing
      Effect m -> m >>= loop
      Step (a :> rest) -> pure $ Just (a, rest)
{-# INLINE unconsFinalIO #-}

{-
unconsEvery :: Monad m => Stream (Of a) m r -> StateT (Stream (Of a) m r) m (Maybe (a, Stream (Of a) m r))
unconsEvery = loop
  where
    loop stream = case stream of
      Return r -> put stream >> pure Nothing
      Effect m -> lift m >>= loop
      Step (a :> rest) -> put rest >> pure $ Just (a, rest)
{-# INLINE unconsEvery #-}
-}

unconsEveryIO :: MonadIO m => IORef (Stream (Of a) m r) -> Stream (Of a) m r -> m (Maybe (a, Stream (Of a) m r))
unconsEveryIO ret = loop
  where
    loop stream = case stream of
      Return r -> liftIO (writeIORef ret stream) >> pure Nothing
      Effect m -> m >>= loop
      Step (a :> rest) -> liftIO (writeIORef ret rest) >> pure (Just (a, rest))
{-# INLINE unconsEveryIO #-}

toVector :: Monad m => Int -> Stream (Of a) m r -> m (Of (Vector a) r)
toVector size st = evalStateT go (error "toVector: no state to get")
  where
    go = do
      vec <- V.unfoldrNM (size+1) unconsFinal st
      ret <- get
      return (vec :> ret)
{-# INLINE toVector #-}

toVectorIO :: MonadIO m => Int -> Stream (Of a) m r -> m (Of (Vector a) r)
toVectorIO size st = do
  ref <- liftIO $ newIORef $ error "toVectorIO: return value never written"
  vec <- V.unfoldrNM (size+1) (unconsFinalIO ref) st
  ret <- liftIO $ readIORef ref
  return (vec :> ret)
{-# INLINE toVectorIO #-}

{- XXX: unimplemented due to hoisting
toVectors :: Monad m => Int -> Stream (Of a) m r -> Stream (Of (Vector a)) m r
toVectors size st = evalStateT go (error "toVector: no state to get")
  where
    go = do
      vec <- V.unfoldrNM size unconsFinal st
      ret <- get
      return (vec :> ret)
{-# INLINE toVectors #-}
-}

toVectorsIO :: MonadIO m => Int -> Stream (Of a) m r -> Stream (Of (Vector a)) m r
toVectorsIO size st = liftIO (newIORef st) >>= go
  where
    go ref = liftIO (readIORef ref) >>= \case
      (Return r) -> Return r
      stream -> do
        vec <- lift $ V.unfoldrNM size (unconsEveryIO ref) stream
        S.yield vec
        go ref
{-# INLINE toVectorsIO #-}

-- XXX: figure out which version of vectorLines is better (both seem to have similar performance)
toVectorsLBS :: MonadIO m => Int -> Stream (Of L.ByteString) m r -> Stream (Of (Vector L.ByteString)) m r
toVectorsLBS size = S.map vectorLines
  where
    vectorLines :: L.ByteString -> Vector L.ByteString
    vectorLines lb = V.fromListN size $ L8.lines lb

    vectorLines' :: L.ByteString -> Vector L.ByteString
    vectorLines' lb = V.unfoldrN size getLine lb
      where
        getLine lb | L.null lb = Nothing
                   | otherwise = case L.elemIndex 0xa lb of
                      Nothing  -> Just (lb, L.empty)
                      Just !ix -> Just (L.take ix lb, L.drop (ix+1) lb)
        {-# INLINE getLine #-}
    {-# INLINE vectorLines #-}
    {-# INLINE vectorLines' #-}
{-# INLINE toVectorsLBS #-}
