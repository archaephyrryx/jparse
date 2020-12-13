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

import qualified Data.ByteString.Streaming.Compat as BS
import qualified Data.ByteString.Streaming.Compat.Char8 as BS8

import           Data.ByteString.Lazy (ByteString)
import           Streaming

-- | Convert a 'BS.ByteString' into a 'Stream' of lazy 'ByteString'
-- containing batches of @n@ lines where @n@ is the value of the first argument
lazyLineSplit :: MonadIO m => Int -> BS.ByteStream m () -> Stream (Of ByteString) m ()
lazyLineSplit !nLines = mappedPost BS.toLazy . BS8.lineSplit nLines
{-# INLINE lazyLineSplit #-}
