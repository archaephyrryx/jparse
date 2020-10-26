{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module JParse.Zepto.Internal where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Concurrent.STM

import qualified Data.ByteString.Lazy as L

import JParse.Global
import JParse.Channels
import JParse.Pipeline

import qualified Data.Nullable as N

-- | set of common synchronization values for concurrent linemode
data ZEnv f a
   = ZEnv
     { nworkers :: Int -- ^ number of workers (runtime)
     , nw       :: TVar Int -- ^ number of unterminated worker threads
     , input    :: ChanBounded L.ByteString -- ^ channel for unparsed input
     , output   :: ChanBounded (f a) -- ^ channel for parsed output
     }

-- | Generate a new 'ZEnv' object in the 'IO' monad
newZEnv :: ReaderT GlobalConf IO (ZEnv f a)
newZEnv = do
  GlobalConf{..} <- (lift . resetWorkerCount =<< ask)
  let nworkers = wkThreads
  nw     <- lift $ newTVarIO wkThreads
  input  <- lift $ newChanBounded workLimit
  output <- lift $ newChanBounded workLimit
  return ZEnv{..}

-- | Creates @n@ worker threads to read from an input channel and perform an IO computation
-- over multi-line lazy bytestrings until an empty 'L.ByteString' is encountered.
--
-- The IO computation in question should, in practice, be a partially-applied function over
-- a 'Z.Parser' and the 'output' @'ChanBounded' (t b)@ inside of the argument 'ZEnv', that
-- runs the parser over each line and writes the processed results to the output channel.
dispatchZEnv :: ZEnv t b -> (L.ByteString -> IO ()) -> IO ()
dispatchZEnv ZEnv{..} = dispatch nworkers input nw
{-# INLINE dispatchZEnv #-}

-- | Creates a thread that writes a sentinel value of @'N.null'@ to the output channel
-- to indicate end-of-output after waiting for all worker threads to signal inactivity.
detectZEnv :: N.Nullable (t b) => ZEnv t b -> IO ()
detectZEnv ZEnv{..} = detect output nw
{-# INLINE detectZEnv #-}
