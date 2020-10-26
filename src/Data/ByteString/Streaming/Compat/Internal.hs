{-# LANGUAGE CPP #-}

module Data.ByteString.Streaming.Compat.Internal
  ( module BS
  ) where

#if MIN_VERSION_streaming_bytestring(0,1,7)
import Streaming.ByteString as BS
#else
import Data.ByteString.Streaming as BS
#endif
