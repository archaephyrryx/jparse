{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JParse.Attoparsec
  ( module JParse.Attoparsec.Conduit
  , module JParse.Attoparsec.Streaming
  , putLnBuilderS
  , runParses
  , runParsed
  ) where

import JParse.Attoparsec.Conduit
import JParse.Attoparsec.Streaming
import JParse.Attoparsec.Common

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Streaming as BS
import qualified Streaming.Prelude as S

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Builder (Builder)
import Streaming (Stream, Of)

putLnBuilderS :: MonadIO m => Stream (Of Builder) m () -> m ()
putLnBuilderS = S.mapM_ putLnBuilder
{-# INLINE putLnBuilderS #-}

-- | Run 'parseS' using a given parser over arbitrary upstream
-- and output the results using 'putLnBuilderS'
runParses :: (MonadIO m, MonadFail m)
          => (B.ByteString -> A.Result (Maybe Builder))
          -> BS.ByteString m ()
          -> m ()
runParses parser src = putLnBuilderS $ blockParseStream parser src
{-# INLINE runParses #-}

-- | Run 'blockParsed' using a given parser over arbitrary upstream
-- and output the results using 'putLnBuilderS'
runParsed :: A.Parser (Maybe Builder)
          -> BS.ByteString IO ()
          -> IO ()
runParsed parser src = putLnBuilderS $ blockParsed parser src
{-# INLINE runParsed #-}
