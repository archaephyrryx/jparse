{-| This module defines the 'GlobalConf' data-type, which encapsulates a number of
\'global\' constants that are used to tune the behavior of various pipeline elements.

'GlobalConf' is currently used in two ways:
  * As an explicit parameter for 'lineParseStream' and its variants in "JParse.Zepto"
  * As the internal state parameter of a 'ReaderT' in the functions 'generate' and 'newZEnv'

In the latter case, the function 'withConf' can be used to pass in an explicit 'GlobalConf' parameter
and yield a computation in the internal monad of the 'ReaderT'

For most purposes, the values used for 'defaultGlobalConf' should be close-to-optimal,
though it may be possible to significantly improve performance by hand-tuning the values
for processing specific inputs on a given machine.
-}
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
-- A 'wkThreads' value of @0@ or lower is interpreted as a sentinel value for \"maximum thread-count\";
-- as such values would normally be invalid, the function 'resetWorkerCount' should be used to detect
-- and replace non-positive worker-counts with the return value of 'getNumCapabilities'.
data GlobalConf
   = GlobalConf
   { batchSize :: Int -- ^ Number of lines per batch in line-mode.
   , wkThreads :: Int -- ^ Number of worker threads to spawn in line-mode.
   , gateLimit :: Int -- ^ Upper bound on number of unprocessed items in an input flow-control gate
   , workLimit :: Int -- ^ Upper bound on number of unprocessed items in a producer-consumer channel
   }

-- | Default set of values for 'GlobalConf'
defaultGlobalConf :: GlobalConf
defaultGlobalConf =
  GlobalConf { batchSize = 1024
             , wkThreads = 0 -- zero-value is reset to getNumCapabilities
             , gateLimit = 256
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
--
-- Equivalent to a monomorphized @flip 'runReaderT'@
withConf :: Monad m => GlobalConf -> (ReaderT GlobalConf m a) -> m a
withConf gconf rf = runReaderT rf gconf
{-# INLINE withConf #-}
