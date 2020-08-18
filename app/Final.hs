{-# LANGUAGE LambdaCase #-}
module Final (toVector, toVectorIO, toVectorsIO) where

import Data.Monoid (Monoid(..))
import Data.Semigroup ((<>))

import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
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
