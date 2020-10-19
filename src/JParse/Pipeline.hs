module JParse.Pipeline where

import Streaming

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming as BS

import qualified Data.Nullable as N

import JParse.Helper
import JParse.Channels

-- Concurrency mode
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM_, unless)

-- | Spawns a thread that writes the contents of a 'Stream' of lazy 'L.ByteString' to a 'ChanBounded'
-- of the same type.
writeBatches :: ChanBounded L.ByteString
             -> Stream (Of L.ByteString) IO ()
             -> IO ()
writeBatches inp str = void $ async $ feedChanBounded inp str
{-# INLINE writeBatches #-}

-- | Creates @n@ worker threads to read from an input channel and perform an IO computation
-- over multi-line lazy bytestrings until an empty 'L.ByteString' is encountered.
--
-- The IO computation in question should commonly be a partially-applied function over
-- an \"output\"" @'ChanBounded' (t b)@ inside of a 'ZEnv', that applies some transformation
-- to each line and writes the processed results to the output channel.
dispatch :: Int
         -> ChanBounded L.ByteString
         -> TVar Int
         -> (L.ByteString -> IO ()) -> IO ()
dispatch nworkers input nw f = replicateM_ nworkers $ async $ worker
  where
    worker :: IO ()
    worker = do
      lbs <- readChanBounded input
      if L.null lbs
        then do
          atomically $ modifyTVar nw pred
          writeChanBounded input lbs
        else f lbs >> worker
{-# INLINE dispatch #-}

-- | Creates a thread that writes a sentinel value of @'N.null'@ to the output channel
-- to indicate end-of-output after waiting for all worker threads to signal inactivity.
detect :: N.Nullable w => ChanBounded w -> TVar Int -> IO ()
detect output nw = void . async $ monitor
  where
    monitor :: IO ()
    monitor = do
      atomically $ do
        n <- readTVar nw
        unless (n == 0) retry
      writeChanBounded output N.null
    {-# INLINE monitor #-}
{-# INLINE detect #-}
