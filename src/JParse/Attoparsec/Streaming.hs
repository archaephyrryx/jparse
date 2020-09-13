{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JParse.Attoparsec.Streaming where

import qualified Streaming.Prelude as S
import           Streaming

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8
import           Data.ByteString.Streaming.Internal (ByteString(..))

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (lift)

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isSpace_w8)

import qualified Data.ByteString as B

import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)

import           Data.Maybe (isNothing)

import           System.IO (stdout)

import Data.Void (Void)

import JParse.Attoparsec.Common


putLnBuilderS :: MonadIO m => Stream (Of (Maybe Builder)) m () -> m ()
putLnBuilderS = S.mapM_ putLnBuilder
{-# INLINE putLnBuilderS #-}

-- | Run 'parseC' using a given parser over arbitrary upstream
-- and output the results using 'putLnBuilderC'
runParses :: (MonadIO m, MonadFail m)
          => (B.ByteString -> A.Result (Maybe Builder))
          -> BS.ByteString m ()
          -> m ()
runParses = runParseWithS putLnBuilderS
{-# INLINE runParses #-}

-- | Run 'parseC' using a given parser over arbitrary upstream
-- and output the results using arbitrary function
runParseWithS :: (MonadIO m, MonadFail m)
              => (Stream (Of (Maybe Builder)) m () -> m ()) -- ^ sink on Maybe Builder values
              -> (B.ByteString -> A.Result (Maybe Builder)) -- ^ parse function
              -> BS.ByteString m () -- ^ input conduit
              -> m ()
runParseWithS act parser src = act $ runParseS $ src
  where
    {-# INLINABLE runParseS #-}
    runParseS mbs = do
      lift (BS.unconsChunk mbs) >>= \case
        Just (!bs, rest) -> do
          let !bs' = trim bs
          if B.null bs'
            then runParseS rest
            else do
              lift (BS.unconsChunk rest) >>= \src -> parseS src False parser (parser bs')
        _       -> pure ()
{-# INLINE runParseWithS #-}

-- | Conduit that feeds upstream ByteStrings into a Parser and yields Maybe Builders from successful parses
--
--   Recurses until end of output is reached and retrieves additonal ByteString
--   output from upstream each pass, until parser yields Done or Fail result.
--
--   Designed around 'seekInObj', which has the property that as soon as a positive or
--   negative result has been decided for each JSON object encountered, the remainder of
--   that JSON object is skipped.

parseS :: (MonadIO m, MonadFail m)
       => Maybe (B.ByteString, BS.ByteString m ()) -- ^ unconsChunk of source monadic bytestring
       -> Bool -- ^ have we already passed empty to a continuation
       -> (B.ByteString -> A.Result (Maybe Builder)) -- ^ parser
       -> A.Result (Maybe Builder) -- ^ most recent result
       -> Stream (Of (Maybe Builder)) m () -- ^ output stream of parsed results
parseS src fed parser res =
  case res of
    A.Done leftover result -> do
      S.yield result
      let more = trim leftover
      if not $ B.null more 
        then parseS src False parser $! parser more
        else case src of
          Nothing           -> pure ()
          Just (bs, rest) -> do
            let bs' = trim bs
            src' <- lift $ BS.unconsChunk rest
            parseS src' False parser $! parser bs'
    A.Partial cont ->
      case src of
        Nothing | fed       -> pure ()
                | otherwise -> parseS Nothing True parser $! cont B.empty
        Just (bs, rest)     -> do
          let more = trim bs
          src' <- lift $ BS.unconsChunk rest
          if not $ B.null more
            then parseS src' False parser $! cont more
            else parseS src' False parser $! res
    A.Fail i ctx e -> fail $ show (i, ctx, e)