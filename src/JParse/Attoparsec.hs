{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : JParse.Attoparsec
Description : Single-threaded stream-parsing in Block-Mode
Copyright   : (c) Peter Duchovni, 2020
License     : BSD-3
Maintainer  : caufeminecraft+github@gmail.com

This module provides \"Block-Mode\" stream-parsers to be used when JSON input
is not strictly one-per-line (i.e. individual objects span multiple lines or
multiple objects appear on the same line), or when the input lines may be too
long to be reasonably read into memory.

The top-level functions 'mapParses', 'runParses', and 'runParsed' are all \"drivers\" for
stream-parsers, and perform IO computations rather than returning their output streams.
In the case of 'mapParses', the computation is a fold-and-finalize operation specified
by the caller, while 'runParses' and 'runParsed' both print the output of the result-'Streaming.Stream'
to stdout.

The parser-library these functions are implemented in terms of is "Parse.Parser.Attoparsec",
which renames and re-exports a limited set of parser-combinators from the "Data.Attoparsec.ByteString"
module. In order to actually perform the desired bulk-extraction of values associated with a query key,
the parser combinator to be passed in to any of these functions should be one of

@
'JParse.Internal.strToAtto' (key :: String)
@

or

@
'JParse.Internal.strToAtto'' (key :: String)
@

The two top-level parser combinators suggested above respectively prioritize
JSON validation over efficiency, and vice versa. If the JSON data is known
to be valid, the latter is preferable.

-}
module JParse.Attoparsec
  ( module JParse.Attoparsec.Streaming
  , putLnBuilderS
  , mapParses
  , runParses
  , runParsed
  ) where


import qualified Data.Attoparsec.ByteString as A
import qualified Streaming.Prelude as S

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Builder (Builder)
import Streaming (Stream, Of)

import qualified Data.ByteString.Streaming.Compat as BS

import JParse.Attoparsec.Streaming
import JParse.Attoparsec.Common

-- | Prints each 'D.Builder' in a 'Stream' to stdout with trailing newlines
putLnBuilderS :: MonadIO m => Stream (Of Builder) m () -> m ()
putLnBuilderS = S.mapM_ putLnBuilder
{-# INLINE putLnBuilderS #-}

-- | Computes a right-associative 'S.fold_' over the 'Stream' returned by 'blockParseStream'
-- using the provided accumulation function, initial value, and extraction function.
mapParses :: A.Parser (Maybe Builder) -- ^ Parser to be run
          -> (Builder -> x -> x) -- ^ Accumulation function
          -> x -- ^ Initial value of accumulator
          -> (x -> a) -- ^ Finalization function to run over final accumulator value
          -> BS.ByteStream IO () -- ^ Input monadic 'BS.ByteString'
          -> IO a -- ^ Extracted result
mapParses parser f z g src =
  let str = blockParseStream (A.parse parser) src
   in S.fold_ (flip f) z g $ str

-- | Runs 'parseS' using a given parser over arbitrary upstream
-- and outputs the results using 'putLnBuilderS'
runParses :: A.Parser (Maybe Builder)
          -> BS.ByteStream IO ()
          -> IO ()
runParses parser src = putLnBuilderS $ blockParseStream (A.parse parser) src
{-# INLINE runParses #-}

-- | Runs 'blockParsed' using a given parser over arbitrary upstream
-- and outputs the results using 'putLnBuilderS'
runParsed :: A.Parser (Maybe Builder)
          -> BS.ByteStream IO ()
          -> IO ()
runParsed parser src = putLnBuilderS $ blockParsed parser src
{-# INLINE runParsed #-}
