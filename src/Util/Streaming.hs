{-# LANGUAGE BangPatterns #-}

{-|
Module      : Util.Streaming
Description : Helper functions for 'BS.ByteStream' conversion
Copyright   : (c) Peter Duchovni, 2020
License     : BSD-3
Maintainer  : caufeminecraft+github@gmail.com

This module contains assorted functions for converting from monadic 'BS.ByteStream' input
to non-monadic ByteString output.
-}

module Util.Streaming where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming.Compat as BS
import qualified Data.ByteString.Streaming.Compat.Char8 as BS8

import           Streaming

-- | Convert a 'BS.ByteString' into a 'Stream' of (lazy) 'L.ByteString'
-- containing batches of @n@ lines where @n@ is the value of the first argument
lazyLineSplit :: MonadIO m => Int -> BS.ByteStream m () -> Stream (Of L.ByteString) m ()
lazyLineSplit !nLines = mappedPost BS.toLazy . BS8.lineSplit nLines
{-# INLINE lazyLineSplit #-}

-- | Convert each 'BS.ByteStream' inside of a 'Stream' into a strict 'B.ByteString'
toStricts :: Monad m => Stream (BS.ByteStream m) m r -> Stream (Of B.ByteString) m r
toStricts = mappedPost _toStrict
  where
    _toStrict :: Monad m => BS.ByteStream m r -> m (Of B.ByteString r)
    _toStrict mbs = do
      (lbs :> ret) <- BS.toLazy mbs
      return $! (L.toStrict lbs :> ret)
    {-# INLINE _toStrict #-}
{-# INLINE toStricts #-}
