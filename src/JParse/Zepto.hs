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
lineParseStream :: GlobalConf -- ^ Set of global constants for behavior tuning
                -> Z.Parser (Maybe a)-- ^ Parser to run over each line
                -> BS.ByteString IO () -- ^ Input JSON-stream
                -> Stream (Of [a]) IO ()
lineParseStream conf parser mbs = parseLines conf parser $ lazyLineSplit (batchSize conf) mbs
{-# INLINE lineParseStream #-}

-- | Parses a monadic bytestring that holds exactly one JSON object per line,
-- returning a 'Stream' of values of specified accumulation and finalization
-- functions per-batch
lineParseFold :: GlobalConf -- ^ Set of global constants for behavior tuning
              -> Z.Parser (Maybe a) -- ^ Parser to run over each line
              -> (a -> x -> x) -- ^ Accumulation function
              -> x -- ^ Initial value of accumulator
              -> (x -> b) -- ^ Finalization function to run over final accumulator value per-batch
              -> BS.ByteString IO () -- ^ Input JSON-stream
              -> Stream (Of b) IO ()
lineParseFold conf parser f z g mbs = parseLinesFold conf parser f z g $ lazyLineSplit (batchSize conf) mbs
{-# INLINE lineParseFold #-}

-- | Parses a monadic bytestring that holds exactly one JSON object per line,
-- returning a 'Stream' of values of specified accumulation and monadic finalization
-- functions per-batch. The accumulator value may also be monadic in certain cases.
lineParseFoldIO :: GlobalConf -- ^ Set of global constants for behavior tuning
                -> Z.Parser (Maybe a) -- ^ Parser to run over each line
                -> (a -> x -> x) -- ^ Accumulation function
                -> x -- ^ Initial value of accumulator
                -> (x -> IO b) -- ^ Monadic finalization function to run over final accumulator value per-batch
                -> BS.ByteString IO () -- ^ Input JSON-stream
                -> Stream (Of b) IO ()
lineParseFoldIO conf parser f z g mbs = parseLinesFoldIO conf parser f z g $ lazyLineSplit (batchSize conf) mbs
{-# INLINE lineParseFoldIO #-}


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
-- The IO computation in question should, in practice, be a partially-applied function over
-- a 'Z.Parser' and the 'output' @'ChanBounded' (t b)@ inside of the argument 'ZEnv' that
-- runs the parser over each line and writes the processed results to the output channel.
dispatch :: ZEnv t b -> (L.ByteString -> IO ()) -> IO ()
dispatch ZEnv{..} f = replicateM_ nworkers $ async $ worker
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
detect :: N.Nullable (t b) => ZEnv t b -> IO ()
detect ZEnv{..} = void $ async $ monitor output nw
  where
    monitor :: N.Nullable w => ChanBounded w -> TVar Int -> IO ()
    monitor output nw = do
      atomically $ do
        n <- readTVar nw
        unless (n == 0) retry
      writeChanBounded output N.null
    {-# INLINE monitor #-}
{-# INLINE detect #-}


-- | Applies a 'Z.Parser' to each line in a 'Stream' of 'L.ByteString' \"batches\" of JSON data,
-- returning a 'Stream' consisting of lists of the successful parse results for each batch.
parseLines :: GlobalConf
           -> Z.Parser (Maybe a)
           -> Stream (Of L.ByteString) IO ()
           -> Stream (Of [a]) IO ()
parseLines conf z str = do
  env@(ZEnv{..}) <- liftIO $ withConf conf newZEnv
  liftIO $ do
    writeBatches input str
    dispatch env (labor output z)
    detect env
  drainChanBounded output
{-# INLINE parseLines #-}

-- | Applies a 'Z.Parser' to each line in a 'Stream' of 'L.ByteString' \"batches\" of JSON data,
-- returning a 'Stream' consisting of finalized values accumulated using a right-associative fold.
parseLinesFold :: GlobalConf
               -> Z.Parser (Maybe a)
               -> (a -> x -> x) -> x -> (x -> b)
               -> Stream (Of L.ByteString) IO ()
               -> Stream (Of b) IO ()
parseLinesFold conf parser f z g str = do
  env@(ZEnv{..})  <- liftIO $ withConf conf newZEnv
  liftIO $ do
    writeBatches input str
    dispatch env (laborFold output parser f z g)
    detect env
  drainChanBoundedMaybe output
{-# INLINE parseLinesFold #-}

-- | Applies a 'Z.Parser' to each line in a 'Stream' of 'L.ByteString' \"batches\" of JSON data,
-- returning a 'Stream' consisting of monadically finalized values accumulated using a (possibly monadic)
-- right-associative fold.
parseLinesFoldIO :: GlobalConf
                 -> Z.Parser (Maybe a)
                 -> (a -> x -> x) -> x -> (x -> IO b)
                 -> Stream (Of L.ByteString) IO ()
                 -> Stream (Of b) IO ()
parseLinesFoldIO conf parser f z g str = do
  env@(ZEnv{..}) <- liftIO $ withConf conf newZEnv
  liftIO $ do
    writeBatches input str
    dispatch env (laborFoldIO output parser f z g)
    detect env
  drainChanBoundedMaybe output
{-# INLINE parseLinesFoldIO #-}

-- | Function passed to 'dispatch' in 'parseLinesStream'
--
-- Accumulates a list of unprocessed parse results,
-- writing to the provided output channel when the list is non-empty.
labor :: ChanBounded ([a])
      -> Z.Parser (Maybe a)
      -> L.ByteString
      -> IO ()
labor output z lbs = do
  let xs = refold unconsLine (accZepto z) [] lbs
  unless (null xs) $
    writeChanBounded output xs
{-# INLINE labor #-}

-- | Function passed to 'dispatch' in 'parseLinesFold'
--
-- Performs a right-associative fold over each parse result, finalizing the value
-- and writing it to the provided output channel.
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

-- | Function passed to 'dispatch' in 'parseLinesFoldIO'
--
-- Performs a right-associative fold over each parse result, finalizing the value in the IO monad
-- and writing the pure result to the provided output channel.
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

-- | List-specific parse-result right-fold function
--
-- Computes the result of the provided parser when run over provided 'L.ByteString',
-- prepending it to the accumulator when the parse succeeded and otherwise preserving
-- the original accumulator value
accZepto :: Z.Parser (Maybe a) -> L.ByteString -> [a] -> [a]
accZepto z bs acc =
  case Z.parse z (L.toStrict bs) of
    Right (Just x) -> x : acc
    _ -> acc
{-# INLINE accZepto #-}

-- | General parse-result right-fold function
--
-- Computes the result of the provided parser when run over provided 'L.ByteString',
-- combining it with the accumulator when the parse succeeded and otherwise preserving
-- the original accumulator value
accZeptoFold :: Z.Parser (Maybe a) -> (a -> x -> x) -> L.ByteString -> x -> x
accZeptoFold parser f bs acc =
  case Z.parse parser (L.toStrict bs) of
    Right (Just x) -> f x acc
    _ -> acc
{-# INLINE accZeptoFold #-}

