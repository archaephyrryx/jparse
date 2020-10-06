-- | Global constants used as parameters for various core functions in
-- stream-parsing pipeline. With the exception of 'nWorkers', these values
-- are arbitrary and cannot be guaranteed optimal for any particular input
module JParse.Global
  ( GlobalConf(..)
  , defaultGlobalConf
  , resetWorkerCount
  , withConf
  ) where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Concurrent (getNumCapabilities)

-- | Set of global constants used to configure the behavior of various library
-- functions.
--
-- If 'wkThreads' is set to @0@, it is automatically reset to the value of 'getNumCapabilities'
data GlobalConf
   = GlobalConf
   { batchSize :: Int -- ^ Number of lines per batch in line-mode.
   , wkThreads :: Int -- ^ Number of worker threads to spawn in line-mode.
   , gateLimit :: Int -- ^ Upper bound on number of unprocessed items in an input flow-control gate
   , workLimit :: Int -- ^ Upper bound on number of unprocessed items in a producer-consumer channel
   }

-- | Default set of values for GlobalConf
--
-- The values themselves are somewhat arbitrary but should yield
-- decent non-optimal performance on most machines.
defaultGlobalConf :: GlobalConf
defaultGlobalConf =
  GlobalConf { batchSize = 1024
             , wkThreads = 0 -- zero-value is reset to getNumCapabilities
             , gateLimit = 256
             , workLimit = 512
             }

-- | Re-evaluate a non-positive number of worker threads based on value returned by 'getNumCapabilities'
resetWorkerCount :: GlobalConf -> IO GlobalConf
resetWorkerCount gconf
  | wkThreads gconf > 0 = pure gconf
  | otherwise = do
      nwk <- getNumCapabilities
      pure $ gconf { wkThreads = nwk }
{-# INLINE resetWorkerCount #-}

-- | Use a particular configuration to run a 'ReaderT'
withConf :: Monad m => GlobalConf -> (ReaderT GlobalConf m a) -> m a
withConf gconf rf = runReaderT rf gconf
{-# INLINE withConf #-}
