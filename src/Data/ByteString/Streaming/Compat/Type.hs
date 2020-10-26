{-# LANGUAGE CPP #-}

module Data.ByteString.Streaming.Compat.Type
  (
#if MIN_VERSION_streaming_bytestring(0,1,7)
    ByteStream(..)
#else
    ByteStream
  , ByteString(..)
#endif
  ) where

#if MIN_VERSION_streaming_bytestring(0,1,7)
import Streaming.ByteString.Internal (ByteStream(..))
#else
import Data.ByteString.Streaming.Internal (ByteString(..))

type ByteStream = ByteString
#endif
