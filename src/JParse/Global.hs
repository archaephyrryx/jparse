module JParse.Global where

import Control.Concurrent (getNumCapabilities)

--
-- | number of lines per individual sub-stream in line-mode
nLines :: Int
nLines = 1024
{-# INLINE nLines #-}

-- | number of worker threads to run Zepto parsing in parallel
nWorkers :: IO Int
nWorkers = getNumCapabilities
{-# INLINE nWorkers #-}

-- | Upper bound on number of unprocessed items in an input flow-control gate
uBound_gate :: Int
uBound_gate = 64
{-# INLINE uBound_gate #-}

-- | Upper bound on number of unprocessed items in produce-consumer channels
uBound_work :: Int
uBound_work = 128
{-# INLINE uBound_work #-}
