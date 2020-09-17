{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JParse.Attoparsec.Conduit where

import qualified Conduit as C
import           Conduit ((.|))
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isSpace_w8)
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import           System.IO (stdout)

import Data.Void (Void)

import JParse.Attoparsec.Common
import JParse.Helper (doJust)

putLnBuilderC :: MonadIO m => C.ConduitT Builder Void m ()
putLnBuilderC = C.mapM_C putLnBuilder
{-# INLINE putLnBuilderC #-}

-- | Run 'parseC' using a given parser over arbitrary upstream
-- and output the results using 'putLnBuilderC'
runParsec :: (ByteString -> A.Result (Maybe Builder))
          -> C.ConduitT () ByteString IO ()
          -> IO ()
runParsec = runParseWithC putLnBuilderC
{-# INLINE runParsec #-}

-- | Run 'parseC' using a given parser over arbitrary upstream
-- and output the results using arbitrary function
runParseWithC :: (MonadIO m, MonadFail m)
              => C.ConduitT Builder Void m a -- ^ sink on Maybe Builder values
              -> (ByteString -> A.Result (Maybe Builder)) -- ^ parse function
              -> C.ConduitT () ByteString m () -- ^ input conduit
              -> m a
runParseWithC mc parser source = C.runConduit $ source .| runParseC .| mc
  where
    {-# INLINABLE runParseC #-}
    runParseC = C.await >>= \case
      Just bs ->
        let bs' = trim bs
            streamEmpty = False
            cleanState = True
         in if B.null bs'
               then runParseC
               else parseC streamEmpty cleanState parser (parser bs')
      _       -> pure ()
{-# INLINE runParseWithC #-}

-- | Conduit that feeds upstream ByteStrings into a Parser and yields Maybe Builders from successful parses
--
--   Recurses until end of output is reached and retrieves additonal ByteString
--   output from upstream each pass, until parser yields Done or Fail result.
--
--   Designed around 'seekInObj', which has the property that as soon as a positive or
--   negative result has been decided for each JSON object encountered, the remainder of
--   that JSON object is skipped.
parseC :: (MonadIO m, MonadFail m)
       => Bool -- ^ has upstream been fully consumed
       -> Bool -- ^ is the parse-state clean (not mid-parse)
       -> (ByteString -> A.Result (Maybe Builder)) -- ^ primary parser
       -> A.Result (Maybe Builder) -- ^ most recent parse result
       -> C.ConduitT ByteString Builder m ()
parseC atEnd clean parser res =
  case res of
    A.Done leftover result -> do
      let clean' = not atEnd -- we are mid-parse if upstream is exhausted
      doJust C.yield result
      if | more <- trim leftover
         , not $ B.null more -> parseC atEnd clean' parser $! parser more -- recurse on non-empty leftover bytestring
         | atEnd -> pure () -- upstream fully consumed and no leftover input
         | otherwise -> C.await >>= \case
            Nothing -> pure ()
            Just bs | !bs' <- trim bs
                    -> parseC False (B.null bs') parser $! (parser $! bs') -- recurse over upstream
    A.Partial cont
      | atEnd && clean -> pure ()
      | atEnd -> parseC atEnd clean parser $! cont B.empty -- continue on empty bytestring when mid-parse
      | otherwise
      -> C.await >>= \case
            Nothing -> parseC True clean parser $! res -- mark end-of-input and retry
            Just bs | more <- trim bs
                    , not $ B.null more
                    -> parseC False False parser $! cont more -- continue on upstream ByteString
                    | otherwise
                    -> parseC False clean parser $! res -- retry upstream when output only whitespace
    A.Fail i ctx e -> fail $ show (i, ctx, e)

