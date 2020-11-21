module Util.Streaming where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming.Compat as BS
import qualified Data.ByteString.Streaming.Compat.Char8 as BS8

import           Streaming

-- | Convert a (monadic) 'BS.ByteString' into a 'Stream' of (lazy) 'L.ByteString'
-- containing batches of lines whose cardinality is the global constant 'nLines'
lazyLineSplit :: MonadIO m => Int -> BS.ByteStream m () -> Stream (Of L.ByteString) m ()
lazyLineSplit nLines = mappedPost BS.toLazy . BS8.lineSplit nLines
{-# INLINE lazyLineSplit #-}
