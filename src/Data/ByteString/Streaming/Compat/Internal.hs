{-# LANGUAGE CPP #-}

module Data.ByteString.Streaming.Compat.Internal
  ( module MBS
  , unconsChunk
  ) where

import qualified Data.ByteString as B
#if MIN_VERSION_streaming_bytestring(0,1,7)
import Streaming.ByteString as MBS hiding (unconsChunk)
import qualified Streaming.ByteString as BS
#else
import Data.ByteString.Streaming as MBS hiding (unconsChunk)
import qualified Data.ByteString.Streaming as BS
#endif

#if MIN_VERSION_streaming_bytestring(0,2,0)
unconsChunk :: Monad m => BS.ByteStream m r -> m (Either r (B.ByteString, BS.ByteStream m r))
unconsChunk = BS.unconsChunk
#elif MIN_VERSION_streaming_bytestring(0,1,7)
unconsChunk :: Monad m => BS.ByteStream m r -> m (Either r (B.ByteString, BS.ByteStream m r))
unconsChunk = BS.nextChunk
#else
unconsChunk :: Monad m => BS.ByteString m r -> m (Either r (B.ByteString, BS.ByteString m r))
unconsChunk = BS.nextChunk
#endif
{-# INLINE unconsChunk #-}
