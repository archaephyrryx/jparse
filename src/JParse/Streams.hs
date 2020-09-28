module JParse.Streams where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import           Streaming

import           JParse.Global

type MBStream m r = Stream (BS.ByteString m) m r
type LBStream m r = Stream (Of L.ByteString) m r

-- | Convert a (monadic) 'BS.ByteString' into a 'Stream' of (lazy) 'L.ByteString'
-- containing batches of lines whose cardinality is the global constant 'nLines'
lazyLineSplit :: MonadIO m => BS.ByteString m () -> LBStream m ()
lazyLineSplit = lazy . lineSplit
  where
    lazy :: MonadIO m => MBStream m r -> LBStream m r
    lazy = mapped BS.toLazy
    {-# INLINE lazy #-}
    lineSplit :: MonadIO m => BS.ByteString m r -> MBStream m r
    lineSplit = BS8.lineSplit nLines
    {-# INLINE lineSplit #-}
{-# INLINE lazyLineSplit #-}
