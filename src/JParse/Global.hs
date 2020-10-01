-- | Global constants used as parameters for various core functions in
-- stream-parsing pipeline. With the exception of 'nWorkers', these values
-- are arbitrary and cannot be guaranteed optimal for any particular input
module JParse.Global where

import Control.Concurrent (getNumCapabilities)

-- | Number of lines per individual sub-stream in line-mode.
nLines :: Int
nLines = 1024
{-# INLINE nLines #-}

-- | Number of worker threads to run line-mode (Zepto) parsing in parallel.
--
-- Determined at runtime based on RTS options and machine-reported threading capabilities
nWorkers :: IO Int
nWorkers = getNumCapabilities
{-# INLINE nWorkers #-}

-- | Upper bound on number of unprocessed items in an input flow-control gate.
uBound_gate :: Int
uBound_gate = 64
{-# INLINE uBound_gate #-}

-- | Upper bound on number of unprocessed items in produce-consumer channels.
uBound_work :: Int
uBound_work = 128
{-# INLINE uBound_work #-}
