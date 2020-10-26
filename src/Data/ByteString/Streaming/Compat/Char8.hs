{-# LANGUAGE CPP #-}

module Data.ByteString.Streaming.Compat.Char8
  ( module BS8
  ) where

#if MIN_VERSION_streaming_bytestring(0,1,7)
import Streaming.ByteString.Char8 as BS8
#else
import Data.ByteString.Streaming.Char8 as BS8
#endif
