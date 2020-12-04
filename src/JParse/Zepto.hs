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
that process JSON data (formatted appropriately for Line-Mode) obtained from a
'BS.ByteStream', returning a 'Stream' of values, with 'lineParseFold'
and its IO variant offering more robust control of post-processing. Each function performs its computations in parallel,
processing \"batches\" of JSON data according to the provided parser, with optional fold-and-extract
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

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming.Compat as BS

import Control.Monad (unless)
import Streaming

import qualified Parse.Parser.Zepto as Z

import JParse.Channels
import JParse.Global
import JParse.Pipeline
import JParse.Zepto.Internal

import Util.Helper

import Util.ByteString.Split (unconsLine)
import Util.Streaming (lazyLineSplit)

-- | Parses a monadic bytestring that holds exactly one JSON object per line, returning a 'Stream' of
-- lists of the successful parse results for each individual batch.
lineParseStream :: GlobalConf -- ^ Set of global constants for behavior tuning
                -> Z.Parser (Maybe a)-- ^ Parser to run over each line
                -> BS.ByteStream IO () -- ^ Input JSON-stream
                -> Stream (Of [a]) IO ()
lineParseStream conf parser mbs = parseLines conf parser $ lazyLineSplit (batchSize conf) mbs
{-# INLINE lineParseStream #-}

-- | Parses a monadic bytestring that holds exactly one JSON object per line,
-- returning a 'Stream' of values of specified accumulation and extraction
-- functions per-batch
lineParseFold :: GlobalConf -- ^ Set of global constants for behavior tuning
              -> Z.Parser (Maybe a) -- ^ Parser to run over each line
              -> (a -> x -> x) -- ^ Accumulation function
              -> x -- ^ Initial value of accumulator
              -> (x -> b) -- ^ Extraction function to run over final accumulator value per-batch
              -> BS.ByteStream IO () -- ^ Input JSON-stream
              -> Stream (Of b) IO ()
lineParseFold conf parser f z g mbs = parseLinesFold conf parser f z g $ lazyLineSplit (batchSize conf) mbs
{-# INLINE lineParseFold #-}

-- | Parses a monadic bytestring that holds exactly one JSON object per line,
-- returning a 'Stream' of values of specified accumulation and monadic extraction
-- functions per-batch. The accumulator value may also be monadic in certain cases.
lineParseFoldIO :: GlobalConf -- ^ Set of global constants for behavior tuning
                -> Z.Parser (Maybe a) -- ^ Parser to run over each line
                -> (a -> x -> x) -- ^ Accumulation function
                -> x -- ^ Initial value of accumulator
                -> (x -> IO b) -- ^ Monadic extraction function to run over final accumulator value per-batch
                -> BS.ByteStream IO () -- ^ Input JSON-stream
                -> Stream (Of b) IO ()
lineParseFoldIO conf parser f z g mbs = parseLinesFoldIO conf parser f z g $ lazyLineSplit (batchSize conf) mbs
{-# INLINE lineParseFoldIO #-}

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
    dispatchZEnv env (labor output z)
    detectZEnv env
  drainChan output
{-# INLINE parseLines #-}



-- | Applies a 'Z.Parser' to each line in a 'Stream' of 'L.ByteString' \"batches\" of JSON data,
-- returning a 'Stream' consisting of extracted values accumulated using a right-associative fold.
parseLinesFold :: GlobalConf
               -> Z.Parser (Maybe a)
               -> (a -> x -> x) -> x -> (x -> b)
               -> Stream (Of L.ByteString) IO ()
               -> Stream (Of b) IO ()
parseLinesFold conf parser f z g str = do
  env@(ZEnv{..})  <- liftIO $ withConf conf newZEnv
  liftIO $ do
    writeBatches input str
    dispatchZEnv env (laborFold output parser f z g)
    detectZEnv env
  drainChanMaybe output
{-# INLINE parseLinesFold #-}

-- | Applies a 'Z.Parser' to each line in a 'Stream' of 'L.ByteString' \"batches\" of JSON data,
-- returning a 'Stream' consisting of monadically extracted values accumulated using a (possibly monadic)
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
    dispatchZEnv env (laborFoldIO output parser f z g)
    detectZEnv env
  drainChanMaybe output
{-# INLINE parseLinesFoldIO #-}

-- | Function passed to 'dispatch' in 'parseLinesStream'
--
-- Accumulates a list of unprocessed parse results,
-- writing to the provided output channel when the list is non-empty.
labor :: BoundedChan ([a])
      -> Z.Parser (Maybe a)
      -> L.ByteString
      -> IO ()
labor output z lbs = do
  let xs = refold unconsLine (accZepto z) [] lbs
  unless (null xs) $ writeChan output xs
{-# INLINE labor #-}

-- | Function passed to 'dispatch' in 'parseLinesFold'
--
-- Performs a right-associative fold over each parse result, extracting the value
-- and writing it to the provided output channel.
laborFold :: BoundedChan (Maybe b)
          -> Z.Parser (Maybe a)
          -> (a -> x -> x) -> x -> (x -> b)
          -> L.ByteString
          -> IO ()
laborFold output parser f z g lbs = do
  let xs = refold unconsLine (accZeptoFold parser f) z lbs
      !ys = g xs
  writeChan output $! Just ys
{-# INLINE laborFold #-}

-- | Function passed to 'dispatch' in 'parseLinesFoldIO'
--
-- Performs a right-associative fold over each parse result, extracting the value in the IO monad
-- and writing the pure result to the provided output channel.
laborFoldIO :: BoundedChan (Maybe b)
            -> Z.Parser (Maybe a)
            -> (a -> x -> x) -> x -> (x -> IO b)
            -> L.ByteString
            -> IO ()
laborFoldIO output parser f z g lbs = do
  let xs = refold unconsLine (accZeptoFold parser f) z lbs
  !ys <- g xs
  writeChan output $! Just ys
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

