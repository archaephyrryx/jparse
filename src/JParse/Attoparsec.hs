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

\"Block-Mode\" stream-parsers to be used when JSON input
is not strictly one-per-line (i.e. individual objects span multiple lines or
multiple objects appear on the same line), or when the input lines may be too
long to be reasonably read into memory.

'blockParseStream' is the most basic of these, and returns a stream of values
corresponding to the output of the provided parser over a 'BS.ByteStream' containing
a stream of JSON objects.
For the purposes of this library, the parser used should be

@
'JParse.Internal.strToAtto' (key :: String)
@

or

@
'JParse.Internal.strToAtto'' (key :: String)
@

where @key@ is the __query-key__ whose corresponding value is to be extracted per-object.


The two top-level parser combinators suggested above respectively prioritize
JSON validation over efficiency, and vice versa. If the JSON data is known
to be valid, the latter is preferable.


The functions 'blockParseFold' and 'blockParseFoldM' are offered for convenience, and each
perform a stream-fold over the output of 'blockParseStream', and each take a fold function,
initial accumulator value, and extraction function to be run over the final result of the accumulator.
In the case of 'blockParseFoldM', the extraction function returns a result in the internal monad of the
input 'BS.ByteStream'. The parser parameter for these functions should be the same as for 'blockParseStream'.

-}
module JParse.Attoparsec
  ( blockParseStream
  , blockParseFold
  , blockParseFoldM
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

-- | Iteratively extract values from a 'BS.ByteStream' using a given parser, returning stream of unwrapped 'Just' values
blockParseStream :: MonadFail m
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
blockParseFold :: MonadFail m
               => A.Parser (Maybe a) -- ^ Parser to be run
               -> (a -> x -> x) -- ^ Accumulation function
               -> x -- ^ Initial value of accumulator
               -> (x -> b) -- ^ Extraction function to run over final accumulator value
               -> BS.ByteStream m () -- ^ Input monadic 'BS.ByteStream'
               -> m b -- ^ Extracted result
blockParseFold parser f z g src =
  let str = blockParseStream parser src
   in S.fold_ (flip f) z g $ str
{-# INLINE blockParseFold #-}

-- | Computes a right-associative 'S.fold_' over the 'Stream' returned by 'blockParseStream'
-- using the provided accumulation function, initial value, and monadic extraction function.
blockParseFoldM :: MonadFail m
                => A.Parser (Maybe a) -- ^ Parser to be run
                -> (a -> x -> x) -- ^ Accumulation function
                -> x -- ^ Initial value of accumulator
                -> (x -> m b) -- ^ Monadic extraction function to run over final accumulator value
                -> BS.ByteStream m () -- ^ Input monadic 'BS.ByteStream'
                -> m b -- ^ Extracted result
blockParseFoldM parser f z g src =
  let str = blockParseStream parser src
   in join $ S.fold_ (flip f) z g $ str
{-# INLINE blockParseFoldM #-}
