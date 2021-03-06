{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module JParse.Internal (strToZepto, strToAtto, strToAtto') where

import           Control.Monad (mzero)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)

import Parse.ASCII.ByteLiterals

import           Parse.ASCII.Attoparsec (symbol, token)
import qualified Parse.ASCII.Zepto as Z (symbol, token)

import           Parse.JSON.Match.Internal (ParseClass, mapClass)
import           Parse.JSON.Match.Attoparsec   (parseMatch, parseMatchFast)
import qualified Parse.JSON.Match.Zepto as Zep (parseMatch)

import           Parse.JSON.Read.Attoparsec.Exact        (parseToEndQ, skipRestObj, skipValue)
import qualified Parse.JSON.Read.Attoparsec.Fast as Fast (parseToEndQ, skipRestObj, skipValue)
import qualified Parse.JSON.Read.Zepto           as Zep  (parseToEndQ, skipRestObj, skipValue)

import qualified Parse.Parser.Attoparsec as A (Parser, pop, word8)
import qualified Parse.Parser.Zepto as Z      (Parser, pop, word8)

-- | \"Block-Mode\" 'A.Parser' that attempts to return a 'Builder' consisting of the contents
-- of the string-value associated with the first JSON key matching a query string.
--
-- Performs maximal validation with least optimal performance using "Parse.Parser.Attoparsec".
strToAtto :: String -- ^ query-key to extract value of
          -> A.Parser (Maybe Builder) -- ^ JSON-object parser for specified query-key
strToAtto !str =
  let !key = qkey str
      !ckey = mapClass key
   in seekInObj ckey
{-# INLINE strToAtto #-}

-- | \"Block-Mode\" 'A.Parser' that attempts to return a 'Builder' consisting of the contents
-- of the string-value associated with the first JSON key matching a query string.
--
-- Performs partial validation with more optimal performance using "Parse.Parser.Attoparsec".
strToAtto' :: String -> A.Parser (Maybe Builder)
strToAtto' !str =
  let !key = qkey str
      !ckey = mapClass key
   in seekInObj' ckey
{-# INLINE strToAtto' #-}

-- | \"Line-Mode\" 'Z.Parser' that attempts to return a 'Builder' consisting of the contents
-- of the string-value associated with the first JSON key matching a query string.
--
-- Performs minimal validation with most optimal performance, using "Parse.Parser.Zepto".
strToZepto :: String -> Z.Parser (Maybe Builder)
strToZepto !str =
  let !key = qkey str
      !ckey = mapClass key
   in seekInObjZepto ckey
{-# INLINE strToZepto #-}

-- | query key function: performs UTF-8 'ByteString' encoding on a
-- 'String'-valued query-key.
qkey :: String -> ByteString
qkey = T.encodeUtf8 . T.pack
{-# INLINE qkey #-}

-- | Extract bytestring-valued key from a JSON object
--
--   Argument is a list of ParseClass values corresponding to
--   query key, obtained most likely through `mapClass`
seekInObj :: [ParseClass] -> A.Parser (Maybe Builder)
seekInObj cs = do
    symbol LBrace
    A.pop >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValue cs
        _    -> mzero
{-# INLINE seekInObj #-}

-- | version using 'getStringValue\''
seekInObj' :: [ParseClass] -> A.Parser (Maybe Builder)
seekInObj' cs = do
    symbol LBrace
    A.pop >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValue' cs
        _    -> mzero
{-# INLINE seekInObj' #-}

-- | version using 'getStringValueZepto'
seekInObjZepto :: [ParseClass] -> Z.Parser (Maybe Builder)
seekInObjZepto cs = do
    Z.symbol LBrace
    Z.pop >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValueZepto cs
        _    -> mzero
{-# INLINE seekInObjZepto #-}

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
{-# INLINE getStringValue #-}

-- | version using 'parseMatchFast' and "Parse.JSON.Read.Attoparsec.Fast" variant functions
getStringValue' :: [ParseClass] -> A.Parser (Maybe Builder)
getStringValue' ckey = do
    this <- parseMatchFast ckey
    if this
       then do symbol Colon <* A.word8 Quote
               Just <$> Fast.parseToEndQ <* Fast.skipRestObj
       else do symbol Colon *> Fast.skipValue
               token >>= \case
                  Comma -> A.word8 Quote *> getStringValue' ckey
                  RBrace -> pure Nothing
                  _ -> mzero
{-# INLINE getStringValue' #-}

-- | version using 'Zep.parseMatch' and Parse.ReadZepto variant functions
getStringValueZepto :: [ParseClass] -> Z.Parser (Maybe Builder)
getStringValueZepto ckey = do
    this <- Zep.parseMatch ckey
    if this
       then do Z.symbol Colon <* Z.word8 Quote
               Just <$> Zep.parseToEndQ <* Zep.skipRestObj
       else do Z.symbol Colon *> Zep.skipValue
               Z.token >>= \case
                  Comma -> Z.word8 Quote *> getStringValueZepto ckey
                  RBrace -> pure Nothing
                  _ -> mzero
{-# INLINE getStringValueZepto #-}
