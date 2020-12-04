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
  ( blockParseStream
  , blockParseFold
  , blockParseFoldIO
  ) where


import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Streaming.Prelude as S

import Control.Monad (join)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Builder (Builder)
import Streaming (Stream, Of)

import qualified Data.ByteString.Streaming.Compat as BS

import JParse.Attoparsec.Internal (trim, parseS)

-- | Run 'parseS' using a given parser over arbitrary upstream
-- and return stream of unwrapped 'Just' results
blockParseStream :: (MonadIO m, MonadFail m)
                 => A.Parser (Maybe a) -- ^ parse function
                 -> BS.ByteStream m () -- ^ input monadic bytestring
                 -> Stream (Of a) m () -- ^ Stream of unwrapped @Just@ values
blockParseStream parser = go
  where
    go mbs =
      lift (BS.unconsChunk mbs) >>= \case
        Right (!bs, rest) -> do
          let !bs' = trim bs
          if B.null bs'
            then go rest
            else do
              src <- lift (BS.unconsChunk rest)
              parseS src (A.parse parser) bs'
        _ -> pure ()
    {-# INLINABLE go #-}
{-# INLINE blockParseStream #-}

-- | Computes a right-associative 'S.fold_' over the 'Stream' returned by 'blockParseStream'
-- using the provided accumulation function, initial value, and extraction function.
blockParseFold :: A.Parser (Maybe a) -- ^ Parser to be run
               -> (a -> x -> x) -- ^ Accumulation function
               -> x -- ^ Initial value of accumulator
               -> (x -> b) -- ^ Finalization function to run over final accumulator value
               -> BS.ByteStream IO () -- ^ Input monadic 'BS.ByteStream'
               -> IO b -- ^ Extracted result
blockParseFold parser f z g src =
  let str = blockParseStream parser src
   in S.fold_ (flip f) z g $ str
{-# INLINE blockParseFold #-}

-- | Computes a right-associative 'S.fold_' over the 'Stream' returned by 'blockParseStream'
-- using the provided accumulation function, initial value, and extraction function.
blockParseFoldIO :: A.Parser (Maybe a) -- ^ Parser to be run
                 -> (a -> x -> x) -- ^ Accumulation function
                 -> x -- ^ Initial value of accumulator
                 -> (x -> IO b) -- ^ Finalization function to run over final accumulator value
                 -> BS.ByteStream IO () -- ^ Input monadic 'BS.ByteStream'
                 -> IO b -- ^ Extracted result
blockParseFoldIO parser f z g src =
  let str = blockParseStream parser src
   in join $ S.fold_ (flip f) z g $ str
{-# INLINE blockParseFoldIO #-}
