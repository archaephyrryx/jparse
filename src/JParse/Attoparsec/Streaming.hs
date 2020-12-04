{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module JParse.Attoparsec.Streaming where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Streaming as AS
import qualified Data.ByteString as B
import qualified Streaming.Prelude as S

import           Streaming
import           Streaming.Internal (Stream(..))

import qualified Data.ByteString.Streaming.Compat as BS

import JParse.Attoparsec.Common
import Util.Helper (cond, doJust)

-- | Run 'parseS' using a given parser over arbitrary upstream
-- and return stream of unwrapped 'Just' results
blockParseStream :: (MonadIO m, MonadFail m)
                 => (B.ByteString -> A.Result (Maybe a)) -- ^ parse function
                 -> BS.ByteStream m () -- ^ input monadic bytestring
                 -> Stream (Of a) m () -- ^ Stream of unwrapped @Just@ values
blockParseStream parser src = runParseS $ src
  where
    {-# INLINABLE runParseS #-}
    runParseS mbs = do
      lift (BS.unconsChunk mbs) >>= \case
        Right (!bs, rest) -> do
          let !bs' = trim bs
          if B.null bs'
            then runParseS rest
            else do
              src' <- lift (BS.unconsChunk rest)
              parseS src' parser bs'
        _       -> pure ()
{-# INLINE blockParseStream #-}

-- | Run 'AS.parsed' using a given parser over arbitrary upstream and
-- return stream of unwrapped 'Just' results
blockParsed :: A.Parser (Maybe a) -> BS.ByteStream IO r -> Stream (Of a) IO ()
blockParsed parser src = loop $ AS.parsed parser src
  where
    loop str = case str of
      Return (Right _)   -> Return ()
      Effect m           -> Effect (fmap loop m)
      Step (ma :> snext) -> case ma of
        Nothing -> loop snext
        Just a  -> Step (a :> loop snext)
      Return (Left (msg,_)) -> fail $ show msg
{-# INLINE blockParsed #-}


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
