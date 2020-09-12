{-# LANGUAGE LambdaCase #-}

module JParse.Internal where

import           Control.Monad (mzero)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (Builder)

import Parse
import qualified Parse.ReadAlt as R

import qualified Parse.MatchZepto as Zep
import qualified Parse.ReadZepto as Zep
import qualified Parse.Parser.Zepto as Z
import qualified Parse.Parser as Z

-- | query key function: performs UTF-8 'ByteString' encoding
-- on a query-key read from the command line as a 'String'.
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
    A.anyWord8 >>= \case
        RBrace -> pure Nothing
        Quote -> getStringValue cs
        _    -> mzero
{-# INLINE seekInObj #-}

-- | version using 'getStringValue\''
seekInObj' :: [ParseClass] -> A.Parser (Maybe Builder)
seekInObj' cs = do
    symbol LBrace
    A.anyWord8 >>= \case
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

-- | version using 'parseMatchAlt' and Parse.ReadAlt variant functions
getStringValue' :: [ParseClass] -> A.Parser (Maybe Builder)
getStringValue' ckey = do
    this <- parseMatchAlt ckey
    if this
       then do symbol Colon <* A.word8 Quote
               Just <$> R.parseToEndQ <* R.skipRestObj
       else do symbol Colon *> R.skipValue
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
