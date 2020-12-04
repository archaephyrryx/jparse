{-| Global configuration for tuning parallel performance of line-mode stream-parsing.

In most cases, the values used for 'defaultGlobalConf' should be acceptable.
-}
module JParse.Global
  ( GlobalConf(..)
  , defaultGlobalConf
  , resetWorkerCount
  , withConf
  ) where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Concurrent (getNumCapabilities)

-- | Parallel performance tuning parameters.
--
-- A 'wkThreads' value of @0@ or lower is interpreted as a sentinel value for \"maximum thread-count\";
-- as such values would normally be invalid, the function 'resetWorkerCount' should be used to detect
-- and replace non-positive worker-counts with the return value of 'getNumCapabilities'.
data GlobalConf
   = GlobalConf
   { batchSize :: Int -- ^ Number of lines per batch in line-mode.
   , wkThreads :: Int -- ^ Number of worker threads to spawn in line-mode.
   , workLimit :: Int -- ^ Upper bound on number of unprocessed items in a producer-consumer channel
   }

-- | Default set of values for 'GlobalConf'
defaultGlobalConf :: GlobalConf
defaultGlobalConf =
  GlobalConf { batchSize = 1024
             , wkThreads = 0 -- zero-value is reset to getNumCapabilities
             , workLimit = 512
             }

-- | Replace a non-positive value of 'wkThreads' with the value returned by 'getNumCapabilities'
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
