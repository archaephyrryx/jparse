{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-| 
Module      : JParse.Zepto
Description : Concurrent stream-parsing in Line-Mode
Copyright   : (c) Peter Duchovni, 2020
License     : BSD-3
Maintainer  : caufeminecraft+github@gmail.com

Line-Mode stream-parsers to be used when JSON input is strictly one-per-line
and when the input lines are short enough to be reasonably read into memory.

The top-level functions 'lineParseStream', 'lineParseFold', and 'lineParseFoldIO' are stream-parsers
that process JSON data (formatted appropriately for Line-Mode) obtained from a monadic
'Data.ByteString.Streaming.ByteString', returning a 'Stream' of values, with 'lineParseFold'
and its IO variant offering more robust control of post-processing. Each functions performs its computations in parallel,
processing \"batches\" of JSON data according to the provided parser, with optional fold-and-convert
parameters.

The parser-library these functions are implemented in terms of is "Parse.Parser.Zepto",
which defines a set of efficient non-backtracking parser-combinators. In order to actually
perform the desired bulk-extraction of values associated with a query key,
the parser combinator to be passed in to any of these functions should be

@
'JParse.Internal.strToZepto' (key :: String)
@

This parser sacrifices the majority of JSON validation in order to be
as efficient as possible when processing valid JSON data, and may process
malformed JSON as if it were valid.

In order to obtain complete JSON objects, every
literal newline character (@\\n@) in the input JSON stream is treated as an
end-of-object marker, as newlines can only ever occur outside of JSON keys and
values. When Line-Mode parsing is possible, however, it allows for concurrent
processing of large batches of lines taken from the input stream, and vastly
outperforms equivalent operations using other JSON-stream parsers.

-}

module JParse.Zepto (lineParseStream, lineParseFold, lineParseFoldIO) where

import Streaming
import qualified Streaming.Prelude as S

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming as BS

import           Data.ByteString.Split (unconsLine)

import qualified Parse.Parser.Zepto as Z

import JParse.Helper
import JParse.Channels
import JParse.Zepto.Internal

import JParse.Streams (lazyLineSplit)
import JParse.Global

-- Concurrency mode
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM_, unless)

import qualified Data.Nullable as N


-- | Parses a monadic bytestring that holds exactly one JSON object per line, returning a 'Stream' of
-- lists of the successful parse results for each individual batch.
lineParseStream :: Z.Parser (Maybe a)
                -> BS.ByteString IO ()
                -> Stream (Of [a]) IO ()
lineParseStream parser mbs = parseLines parser $ lazyLineSplit (batchSize defaultGlobalConf) mbs
{-# INLINE lineParseStream #-}

-- | Parses a monadic ByteString that is already pre-processed to raw JSON format
--   and processes output with specified accumulation and finalization functions
--   per-batch
lineParseFold :: Z.Parser (Maybe a) -- ^ Parser to run over each line
              -> (a -> x -> x) -- ^ Accumulation function
              -> x -- ^ Initial value of accumulator
              -> (x -> b) -- ^ Finalization function to run over final accumulator value per-batch
              -> BS.ByteString IO () -- ^ Input JSON-stream
              -> Stream (Of b) IO ()
lineParseFold parser f z g mbs = parseLinesFold parser f z g $ lazyLineSplit (batchSize defaultGlobalConf) mbs
{-# INLINE lineParseFold #-}

-- | Parses a monadic ByteString that is already pre-processed to raw JSON format
--   and processes output with specified accumulation and monadic finalization functions
--   per-batch
lineParseFoldIO :: Z.Parser (Maybe a) -- ^ Parser to run over each line
                -> (a -> x -> x) -- ^ Accumulation function
                -> x -- ^ Initial value of accumulator
                -> (x -> IO b) -- ^ Monadic finalization function to run over final accumulator value per-batch
                -> BS.ByteString IO () -- ^ Input JSON-stream
                -> Stream (Of b) IO ()
lineParseFoldIO parser f z g mbs = parseLinesFoldIO parser f z g $ lazyLineSplit (batchSize defaultGlobalConf) mbs
{-# INLINE lineParseFoldIO #-}


parseLines :: Z.Parser (Maybe a)
           -> Stream (Of L.ByteString) IO ()
           -> Stream (Of [a]) IO ()
parseLines z str = do
  ZEnv{..} <- liftIO (withConf defaultGlobalConf newZEnv :: IO (ZEnv [] a))
  liftIO $ do
    async $ feedChanBounded input str
    replicateM_ nworkers $ async $ worker input output nw z
    async $ monitor output nw
  drainChanBounded output
{-# INLINE parseLines #-}

parseLinesFold :: Z.Parser (Maybe a)
               -> (a -> x -> x) -> x -> (x -> b)
               -> Stream (Of L.ByteString) IO ()
               -> Stream (Of b) IO ()
parseLinesFold parser f z g str = do
  ZEnv{..} <- liftIO (withConf defaultGlobalConf newZEnv :: IO (ZEnv Maybe b))
  liftIO $ do
    async $ feedChanBounded input str
    replicateM_ nworkers $ async $ workerFold input output nw parser f z g
    async $ monitor output nw
  drainChanBoundedMaybe output
{-# INLINE parseLinesFold #-}

parseLinesFoldIO :: Z.Parser (Maybe a)
                 -> (a -> x -> x) -> x -> (x -> IO b)
                 -> Stream (Of L.ByteString) IO ()
                 -> Stream (Of b) IO ()
parseLinesFoldIO parser f z g str = do
  ZEnv{..} <- liftIO (withConf defaultGlobalConf newZEnv :: IO (ZEnv Maybe b))
  liftIO $ do
    async $ feedChanBounded input str
    replicateM_ nworkers $ async $ workerFoldIO input output nw parser f z g
    async $ monitor output nw
  drainChanBoundedMaybe output
{-# INLINE parseLinesFoldIO #-}

worker :: ChanBounded L.ByteString
       -> ChanBounded [a]
       -> TVar Int
       -> Z.Parser (Maybe a)
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
{-# INLINE worker #-}

workerFold :: ChanBounded L.ByteString
           -> ChanBounded (Maybe b)
           -> TVar Int
           -> Z.Parser (Maybe a)
           -> (a -> x -> x) -> x -> (x -> b)
           -> IO ()
workerFold input output nw parser f z g = go
  where
    go = do
      lbs <- readChanBounded input
      if L.null lbs
         then do
           atomically $ modifyTVar nw pred
           writeChanBounded input lbs
         else laborFold output parser f z g lbs >> go
{-# INLINE workerFold #-}

workerFoldIO :: ChanBounded L.ByteString
             -> ChanBounded (Maybe b)
             -> TVar Int
             -> Z.Parser (Maybe a)
             -> (a -> x -> x) -> x -> (x -> IO b)
             -> IO ()
workerFoldIO input output nw parser f z g = go
  where
    go = do
      lbs <- readChanBounded input
      if L.null lbs
         then do
           atomically $ modifyTVar nw pred
           writeChanBounded input lbs
         else laborFoldIO output parser f z g lbs >> go
{-# INLINE workerFoldIO #-}

labor :: ChanBounded ([a])
      -> Z.Parser (Maybe a)
      -> L.ByteString
      -> IO ()
labor output z lbs = do
  let xs = refold unconsLine (accZepto z) [] lbs
  unless (null xs) $
    writeChanBounded output xs
{-# INLINE labor #-}

laborFold :: ChanBounded (Maybe b)
          -> Z.Parser (Maybe a)
          -> (a -> x -> x) -> x -> (x -> b)
          -> L.ByteString
          -> IO ()
laborFold output parser f z g lbs = do
  let xs = refold unconsLine (accZeptoFold parser f) z lbs
  let !ys = Just $! g xs
  writeChanBounded output ys
{-# INLINE laborFold #-}

laborFoldIO :: ChanBounded (Maybe b)
            -> Z.Parser (Maybe a)
            -> (a -> x -> x) -> x -> (x -> IO b)
            -> L.ByteString
            -> IO ()
laborFoldIO output parser f z g lbs = do
  let xs = refold unconsLine (accZeptoFold parser f) z lbs
  !ys <- g xs
  writeChanBounded output $! Just ys
{-# INLINE laborFoldIO #-}

accZepto :: Z.Parser (Maybe a) -> L.ByteString -> [a] -> [a]
accZepto z bs acc =
  case Z.parse z (L.toStrict bs) of
    Right (Just x) -> x : acc
    _ -> acc
{-# INLINE accZepto #-}

accZeptoFold :: Z.Parser (Maybe a) -> (a -> x -> x) -> L.ByteString -> x -> x
accZeptoFold parser f bs acc =
  case Z.parse parser (L.toStrict bs) of
    Right (Just x) -> f x acc
    _ -> acc
{-# INLINE accZeptoFold #-}

monitor :: N.Nullable w => ChanBounded w -> TVar Int -> IO ()
monitor output nw = do
  atomically $ do
    n <- readTVar nw
    unless (n == 0) retry
  writeChanBounded output N.null
{-# INLINE monitor #-}
