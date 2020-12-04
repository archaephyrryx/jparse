{- | Semi-internal module defining common pipeline elements used for parallel 'Stream' creation.

The common use-case of these functions is


@
example :: 'N.Nullable' a => Stream (Of a) IO () -> Stream (Of b) IO ()
example str = do
    output <- liftIO $ 'newBoundedChan' n
    liftIO $ do
        input <- 'newBoundedChan' n
        sem <- 'newTVarIO' m
        'writeBatches' input str
        'dispatch' m input sem (doWork output)
        'detect' output sem
    'drainChanMaybe' output

doWork :: 'BoundedChan' (Maybe b) -> a -> IO ()
doWork output x = 'writeChan' output $! Just $ someFunctionOf x
@

where @doWork@ is a function that processes a batch of work and writes the result to a bounded channel.

Under this model of operation, it is important that the @'TVar' 'Int'@ parameter passed in to 'dispatch'
and 'detect' are the same, and that the initial value is equal to the total number of worker threads
created in the internal do block. This is because the 'TVar' in question represents a shared semaphor
whose value is decremented by 1 every time a worker thread created by 'dispatch' terminates upon reaching
an end-of-input marker, and upon reaching @0@ causes 'detect' to write an end-of-output marker to the
associated output channel. Under most cases, this means that the number of workers specified in a call to
dispatch should equal the initial value of this 'TVar'. However, it is possible to construct a scenario in
which 'dispatch' is called multiple times over different inputs, which are demultiplexed into a single output
channel, in which case the initial value should equal the sum of worker counts across all such calls to
'dispatch'.

@
example' :: ('N.Nullable' a, 'N.Nullable' b) => Stream (Of a) IO () -> Stream (Of b) IO () -> Stream (Of c) IO ()
example' strA strB = do
    output <- liftIO $ 'newBoundedChan' n
    liftIO $ do
        inputA <- 'newBoundedChan' m
        inputB <- 'newBoundedChan' m
        sem <- 'newTVarIO' (na+nb)
        'writeBatches' inputA strA
        'writeBatches' inputB strB
        'dispatch' na inputA sem (doWorkA output)
        'dispatch' nb inputB sem (doWorkB output)
        'detect' output sem
    'drainChanMaybe' output

doWorkA :: BoundedChan (Maybe c) -> a -> IO ()
doWorkA output x = writeChan output $! Just $ functionA x

doWorkB :: BoundedChan (Maybe c) -> b -> IO ()
doWorkB output y = writeChan output $! Just $ functionB y
@
-}
module JParse.Pipeline (writeBatches, dispatch, detect) where

import qualified Data.ByteString.Lazy as L

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM_, unless)
import Streaming

import Data.Nullable

import JParse.Channels

-- | Spawns a thread that writes the contents of a 'Stream' to a 'BoundedChan'
-- with the same internal type.
writeBatches :: Nullable v
             => BoundedChan v
             -> Stream (Of v) IO ()
             -> IO ()
writeBatches inp str = void $ async $ feedChan inp str
{-# INLINE writeBatches #-}
{-# SPECIALIZE INLINE writeBatches :: BoundedChan L.ByteString -> Stream (Of L.ByteString) IO () -> IO () #-}

-- | Creates @n@ worker threads to read from an input channel and perform an IO computation
-- over channel elements until a 'N.null' element is encountered.
--
-- When used internally, the \"work\"-function is a closure around
-- an \"output\"" @'BoundedChan' w@, that applies a transformation
-- to each line of a multi-line batched 'L.ByteString' and writes
-- the processed results to the output channel.
--
-- When used externally, the IO computation over each batch is not required to write to an output channel,
-- but is nevertheless responsible for ensuring that processed batches are not \'lost\' to the ether. When
-- used alongside 'detect', it is implicitly assumed that writes to an output channel are occurring, whether
-- this is handled by the dispatched workers or an intervening custom pipeline element.
dispatch :: Nullable v
         => Int -- ^ number of worker threads to spawn
         -> BoundedChan v -- ^ input channel
         -> TVar Int -- ^ counter for number of unterminated worker threads
         -> (v -> IO ()) -- ^ \"work\" function operating over batches
         -> IO ()
dispatch nworkers input nw f = replicateM_ nworkers $ async $ worker
  where
    worker :: IO ()
    worker = do
      item <- readChan input
      if isNull item
        then do
          atomically $ modifyTVar nw pred
          writeChan input item
        else f item >> worker
{-# INLINE dispatch #-}
{-# SPECIALIZE INLINE dispatch :: Int -> BoundedChan L.ByteString -> TVar Int -> (L.ByteString -> IO ()) -> IO () #-}


-- | Creates a thread that writes a sentinel value of 'N.null' to the output channel
-- to indicate end-of-output after waiting for all worker threads to signal inactivity.
detect :: Nullable w => BoundedChan w -> TVar Int -> IO ()
detect output nw = void . async $ monitor
  where
    monitor :: IO ()
    monitor = do
      atomically $ do
        n <- readTVar nw
        unless (n == 0) retry
      writeChan output nullValue
    {-# INLINE monitor #-}
{-# INLINE detect #-}
