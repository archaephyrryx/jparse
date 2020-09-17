{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Build where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D

import Data.ByteString.Builder (Builder)

buildNano :: Builder -> B.ByteString
buildNano = build 8 16
{-# INLINE buildNano #-}

buildMicro :: Builder -> B.ByteString
buildMicro = build 32 64
{-# INLINE buildMicro #-}

buildShort :: Builder -> B.ByteString
buildShort = build 128 256
{-# INLINE buildShort #-}

buildLong :: Builder -> B.ByteString
buildLong = build 2048 4096
{-# INLINE buildLong #-}

build :: Int -> Int -> Builder -> B.ByteString
build !i !j = L.toStrict . D.toLazyByteStringWith buildStrat L.empty
  where
    buildStrat = D.untrimmedStrategy i j
    {-# INLINE buildStrat #-}
{-# INLINE build #-}
