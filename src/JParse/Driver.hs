{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Line-mode concurrent stream-parser that writes successful parse-results to
--   stdout without any post-processing.
module JParse.Driver (streamZepto) where

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as D
import qualified Parse.Parser.Zepto as Z

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM_, unless)
import Data.ByteString (ByteString)
import Data.ByteString.Build (buildLong)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Split (unconsLine)
import JParse.Channels
import JParse.Driver.Internal
import JParse.Helper
import JParse.Streams (lazyLineSplit)
import JParse.Global

-- | Runs a parser in parallel threads over batches of lines from input 'BS.ByteString'
-- and writes each finished batch to stdout.
streamZepto :: Z.Parser (Maybe Builder) -> BS.ByteString IO () -> IO ()
streamZepto z mbs = do
  ZEnv{..} <- withConf defaultGlobalConf newZEnv
  async $ feedChanBounded input $ lazyLineSplit (batchSize defaultGlobalConf) mbs
  replicateM_ nworkers $ async $ worker input output nw z
  done <- async $ collector output
  monitor output nw
  wait done

-- | Wait until all worker threads have encountered end-of-input marker
-- and write end-of-output marker to output channel
monitor :: ChanBounded (Maybe a) -> TVar Int -> IO ()
monitor output nw = do
  atomically $ do
    n <- readTVar nw
    unless (n == 0) retry
  writeChanBounded output Nothing

-- | Continually read from output channel and print
-- values to stdout, terminating when end-of-output
-- marker is encountered.
collector :: ChanBounded (Maybe ByteString) -> IO ()
collector output = go
  where
    go = readChanBounded output >>= \case
           Just bs -> B.putStr bs >> go
           Nothing -> return ()

-- | Dispatch non-empty batches of lines to 'labor' function
-- and decrements semaphore 'TVar' when end-of-input marker is
-- encountered (replaces end-of-input marker in channel to signal
-- other waiting worker threads)
worker :: ChanBounded L.ByteString
       -> ChanBounded (Maybe ByteString)
       -> TVar Int
       -> Z.Parser (Maybe Builder)
       -> IO ()
worker input output nw z = go
  where
    go = do
      lbs <- readChanBounded input
      if L.null lbs
         then do
           atomically $ modifyTVar nw pred
           writeChanBounded input lbs
         else labor output z lbs >> go

-- | Performs a 'refold' of lazy 'L.ByteString' containing a batch of multiple lines,
-- manifests resultant 'D.Builder' as a strict 'B.ByteString' which is then written to
-- the output channel
labor :: ChanBounded (Maybe ByteString)
      -> Z.Parser (Maybe Builder)
      -> L.ByteString
      -> IO ()
labor output z lbs = do
  let bld = refold unconsLine (accZepto z) (mempty :: Builder) lbs
      !bs = buildLong bld
  writeChanBounded output (Just bs)

-- | Prepends result of successful parses to accumulator 'D.Builder' with
-- intervening newline (@\\n@) character
accZepto :: Z.Parser (Maybe Builder)
         -> L.ByteString
         -> Builder
         -> Builder
accZepto z bs bld =
  case Z.parse z (L.toStrict bs) of
    Right (Just x) -> x <> D.word8 0x0a <> bld
    _ -> bld
