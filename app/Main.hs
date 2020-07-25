{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Conduit as C
import           Conduit ((.|))
import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isDigit_w8, isSpace_w8, skipSpace)
import qualified Data.Attoparsec.Text as T (char)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as S8
import           Data.Word (Word8)
import qualified System.Environment as Sys
import           System.IO (stdout)

import Data.Function (fix)
import Data.Void (Void)

import Parse

-- | query key function: extracts a query bytestring from command line argument list
--   default if no arguments found is "name" for historical reasons
qkey :: [String] -> ByteString
qkey = \case
    a:_ -> T.encodeUtf8 . T.pack $ a
    _   -> "name"
{-# INLINE qkey #-}


main :: IO ()
main = do
   key <- qkey <$> Sys.getArgs
   let ckey = mapClass $! key
       parser = A.parse (seekInObj ckey)
   C.runConduit $ C.stdinC .| fix \retry -> do
       fmap trim <$> C.await >>= \case
           Just bs | B.null bs -> retry
                   | streamEmpty <- False
                   , cleanState  <- True
                   -> parseC streamEmpty cleanState parser (parser bs)
           _       -> pure ()





-- | print a Builder to stdout with a trailing newline
putLnBuilder :: Maybe Builder -> IO ()
putLnBuilder Nothing = pure ()
putLnBuilder (Just b) = D.hPutBuilder stdout (b <> D.word8 0xa)

-- | Run a parser as a conduit sink that prints each parsed result on a separate line.
--   Recurses until end of output is reached and retrieves additonal ByteString
--   output from upstream each pass, until parser yields Done or Fail result.
parseC :: Bool -- ^ has conduit been fully consumed
       -> Bool -- ^ is the parse-state clean (not mid-parse)
       -> (ByteString -> A.Result (Maybe Builder)) -- ^ primary parser
       -> A.Result (Maybe Builder) -- ^ most recent parse result
       -> C.ConduitT ByteString Void IO ()
parseC atEnd clean parser res =
  case res of
    A.Done leftover result -> do
      let clean' = not atEnd -- at clean state only if upstream is exhausted
      C.liftIO (putLnBuilder result)
      if | more <- trim leftover
         , not $ B.null more -> parseC atEnd clean parser $! parser more -- recurse on non-empty leftover bytestring
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

-- | Strip leading whitespace from a ByteString
trim :: ByteString -> ByteString
trim !bs = B.dropWhile A.isSpace_w8 bs
{-# INLINE trim #-}

-- | Extract bytestring-valued key from a JSON object
--
--   Argument is a list of ParseClass values corresponding to
--   query key, obtained most likely through `mapClass`
seekInObj :: [ParseClass] -> A.Parser (Maybe Builder)
seekInObj cs = do
    symbol LBrace
    A.anyWord8 >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValue cs
        _    -> mzero

-- | Extract bytestring-valued key from a sequence of key-value pairs inside a
-- JSON object, then consume and discard the tail of the object throug the
-- closing brace.  The starting position is immediately after the initial open
-- quote character.
--
getStringValue :: [ParseClass] -> A.Parser (Maybe Builder)
getStringValue ckey = do
    this <- parseMatch ckey
    if this
       then do symbol Colon <* A.word8 Quote
               Just <$> parseToEndQ <* skipRestObj
       else do symbol Colon *> skipValue
               token >>= \case
                  Comma -> A.word8 Quote *> getStringValue ckey
                  RBrace -> pure Nothing
                  _ -> mzero
