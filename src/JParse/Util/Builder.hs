{-# LANGUAGE BangPatterns #-}

-- | Convenient shorthand helper-functions for converting from 'D.Builder' to 'B.ByteString'
--   when the approximate size-range of the result is known in advance.
module JParse.Util.Builder where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder.Extra as D

import Data.ByteString.Builder (Builder)

-- | Converts to 'B.ByteString' for length most likely in range 0-31
buildNano :: Builder -> B.ByteString
buildNano = build 8 16
{-# INLINE buildNano #-}

-- | Converts to 'B.ByteString' for length most likely in range 32-127
buildMicro :: Builder -> B.ByteString
buildMicro = build 32 64
{-# INLINE buildMicro #-}

-- | Converts to 'B.ByteString' for length most likely in range 128-511
buildShort :: Builder -> B.ByteString
buildShort = build 128 256
{-# INLINE buildShort #-}

-- | Converts to 'B.ByteString' for length most likely in range 512-2047
buildMedium :: Builder -> B.ByteString
buildMedium = build 512 1024
{-# INLINE buildMedium #-}

-- | Converts to 'B.ByteString' for length most likely in range 2048-8191
buildLong :: Builder -> B.ByteString
buildLong = build 2048 4096
{-# INLINE buildLong #-}

-- | Converts to 'B.ByteString' for length most likely in range 8192-32767
buildMega :: Builder -> B.ByteString
buildMega = build 8192 16384
{-# INLINE buildMega #-}

-- | Generic function to covert from 'D.Builder' to 'B.ByteString' using
-- specified initial and subsequent buffer sizes
build :: Int -- ^ Initial buffer size
      -> Int -- ^ Subsequent buffer size
      -> Builder -> B.ByteString
build !i !j = L.toStrict . D.toLazyByteStringWith buildStrat L.empty
  where
    buildStrat = D.untrimmedStrategy i j
    {-# INLINE buildStrat #-}
{-# INLINE build #-}
