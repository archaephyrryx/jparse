{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JParse.Attoparsec.Internal (trim, parseS) where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import qualified Data.ByteString as B
import qualified Streaming.Prelude as S

import Data.ByteString.Internal (isSpaceWord8)

import           Streaming
import           Streaming.Internal (Stream(..))

import qualified Data.ByteString.Streaming.Compat as BS

import Util.Helper (cond, doJust)

-- | Strip leading whitespace from a ByteString
trim :: B.ByteString -> B.ByteString
trim !bs = B.dropWhile isSpaceWord8 bs
{-# INLINE trim #-}

-- | Extract strict ByteStrings from a source 'ByteStream'
-- and feed them into a Parser, yielding results of type @Maybe a@
-- from successful parses
--
-- Recurses until end of pure input is reached and retrieves additonal ByteString
-- chunks from the source each pass, until parser yields Done or Fail result.
--
-- Designed around 'strToAtto', which has the property that as soon as a positive or
-- negative result has been decided for each JSON object encountered, the remainder of
-- that JSON object is skipped.
parseS :: (MonadIO m, MonadFail m)
       => Either () (B.ByteString, BS.ByteStream m ()) -- ^ 'unconsChunk' of source 'BS.ByteStream'
       -> (B.ByteString -> A.Result (Maybe a)) -- ^ parser
       -> B.ByteString -- ^ initial chunk
       -> Stream (Of a) m () -- ^ output stream of parsed results
parseS unSrc parser !bs = loop unSrc $ parser bs
  where
    loop src res = case res of
      A.Done leftover result -> do
        doJust S.yield result
        let more = trim leftover
        if not $ B.null more
          then loop src $! parser more
          else case src of
            Left _ -> pure ()
            Right (bs', rest) -> do
              src' <- lift $ BS.unconsChunk rest
              loop src' $! parser $! trim bs'
      A.Partial cont ->
        case src of
          Left _ -> loop (Left ()) $! cont B.empty
          Right (bs', rest) -> do
            src' <- lift $ BS.unconsChunk rest
            loop src' $ cond B.null (const res) cont $! trim bs'
      A.Fail i ctx e -> fail $ show (i, ctx, e)
{-# INLINE parseS #-}
