{-# LANGUAGE PatternSynonyms #-}
module Final (toVector) where

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

unconsFinal :: Monad m => Stream (Of a) m r -> StateT r m (Maybe (a, Stream (Of a) m r))
unconsFinal = loop
  where
    loop stream = case stream of
      Return r -> put r >> pure Nothing
      Effect m -> lift m >>= loop
      Step (a :> rest) -> pure $ Just (a, rest)
{-# INLINE unconsFinal #-}

toVector :: Monad m
         => Int
         -> Stream (Of a) m r
         -> m (Of (Vector a) r)
toVector size st = evalStateT go (error "toVector: no state to get")
  where
    go = do
      vec <- V.unfoldrNM (size+1) unconsFinal st
      ret <- get
      return (vec :> ret)
{-# INLINE toVector #-}
