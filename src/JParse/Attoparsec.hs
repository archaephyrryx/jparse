{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JParse.Attoparsec
  ( module JParse.Attoparsec.Streaming
  , putLnBuilderS
  , mapParses
  , runParses
  , runParsed
  ) where

import JParse.Attoparsec.Streaming
import JParse.Attoparsec.Common

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Streaming as BS
import qualified Streaming.Prelude as S

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Builder (Builder)
import Streaming (Stream, Of)

-- | Prints each 'D.Builder' in a 'Stream' to stdout with trailing newlines
putLnBuilderS :: MonadIO m => Stream (Of Builder) m () -> m ()
putLnBuilderS = S.mapM_ putLnBuilder
{-# INLINE putLnBuilderS #-}

-- | Computes a right-associative 'S.fold_' over the 'Stream' returned by 'blockParseStream'
-- using the provided accumulation function, initial value, and finalization function.
mapParses :: A.Parser (Maybe Builder) -- ^ Parser to be run
          -> (Builder -> x -> x) -- ^ Accumulation function
          -> x -- ^ Initial value
          -> (x -> a) -- ^ Finalization function
          -> BS.ByteString IO () -- ^ Input monadic 'BS.ByteString'
          -> IO a -- ^ Finalized result
mapParses parser f z g src =
  let str = blockParseStream (A.parse parser) src
   in S.fold_ (flip f) z g $ str

-- | Runs 'parseS' using a given parser over arbitrary upstream
-- and outputs the results using 'putLnBuilderS'
runParses :: A.Parser (Maybe Builder)
          -> BS.ByteString IO ()
          -> IO ()
runParses parser src = putLnBuilderS $ blockParseStream (A.parse parser) src
{-# INLINE runParses #-}

-- | Runs 'blockParsed' using a given parser over arbitrary upstream
-- and outputs the results using 'putLnBuilderS'
runParsed :: A.Parser (Maybe Builder)
          -> BS.ByteString IO ()
          -> IO ()
runParsed parser src = putLnBuilderS $ blockParsed parser src
{-# INLINE runParsed #-}
