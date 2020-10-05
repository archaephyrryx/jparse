{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module JParse.Driver.Internal where

import Control.Concurrent.STM.TVar
import Control.Monad.Trans.Reader (ReaderT(..), ask)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D
import           Data.ByteString.Builder (Builder)

import Streaming
import qualified Data.ByteString.Streaming as BS

import JParse.Channels
import JParse.Global

-- | set of common synchronization values for concurrent linemode
data ZEnv
   = ZEnv
     { nworkers :: Int -- ^ number of workers (runtime)
     , nw :: TVar Int -- ^ number of unterminated worker threads
     , input :: ChanBounded L.ByteString -- ^ channel for unparsed input
     , output :: ChanBounded (Maybe B.ByteString) -- ^ channel for parsed output
     }

-- | Generate a new 'ZEnv' object in the 'IO' monad
newZEnv :: ReaderT GlobalConf IO ZEnv
newZEnv = do
  GlobalConf{..} <- (lift . resetWorkerCount =<< ask)
  let nworkers = wkThreads
  nw     <- lift $ newTVarIO wkThreads
  input  <- lift $ newChanBounded workLimit
  output <- lift $ newChanBounded workLimit
  return ZEnv{..}

-- | Manifest each monadic 'BS.ByteString' in a 'Stream' as a strict 'B.ByteString'
toStricts :: Monad m => Stream (BS.ByteString m) m r -> Stream (Of B.ByteString) m r
toStricts = mapped _toStrict
  where
    _toStrict :: Monad m => BS.ByteString m r -> m (Of B.ByteString r)
    _toStrict mbs = do
      (lbs :> ret) <- BS.toLazy mbs
      return $! (L.toStrict lbs :> ret)
    {-# INLINE _toStrict #-}
{-# INLINE toStricts #-}
