{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Parse.ReadStream where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)

import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import           Data.Word (Word8)

import qualified Streaming as S
import qualified Streaming.Prelude as SP


import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import           Parse.Symbol hiding (token, symbol)
import           Parse.Read.Internal

import qualified Parse.Parser.Stream as Z
import qualified Parse.Parser.ZeptoStream as Z
import           Parse.Parser.Stream (token, symbol)

-- * String-centric parsers

-- | parseToEndQ : parses the payload of a JSON-formatted string
--   silently consumes end-quote character and any trailing whitespace
--   implicitly requires that the first doublequote has been consumed
--   any other pre-consumed characters will be omitted from the parser result
parseToEndQ :: Monad m => Z.Parser m Builder
parseToEndQ = parseQBuilder <* Z.skipSpace
  where
    parseQBuilder = go
      where
        go = do
          bs <- Z.takeWhile isSimple
          bs' <- lift $ BS.toLazy_ bs
          let bld = D.lazyByteString bs'
          Z.pop >>= \case
              Quote  -> pure bld
              _      -> do
                e <- parseEscaped
                ((bld <> e) <>) <$> go
    {-# INLINE parseQBuilder #-}
{-# INLINE parseToEndQ #-}

-- | skipToEndQ : efficiently skips the remainder of a JSON-formatted string
--   can be called partway into string parsing provided that no characters
--   are orphaned from associated preceding unescaped backslashes
--
--   does not check for EOF or invalid backslash escapes
skipToEndQ :: Monad m => Z.Parser m ()
skipToEndQ = skipQUnit >> Z.skipSpace
  where
    skipQUnit :: Monad m => Z.Parser m ()
    skipQUnit = go
      where
        go = do
          Z.skipWhile isSimple
          Z.pop >>= \case
            Quote  -> pure ()
            _      -> Z.pop >> go
    {-# INLINE skipQUnit #-}
{-# INLINE skipToEndQ #-}

-- | basic parser that interprets escaped characters (except backslash)
parseEscaped :: Monad m => Z.Parser m Builder
parseEscaped =
  Z.pop >>= \case
    e | escAtom e -> pure $ D.word8 e
    Hex_u -> do
      q <- parseHex
      pure $ D.word8 Hex_u <> D.byteString q
{-# INLINE parseEscaped #-}

-- | parses uXXXX hexcodes (without initial u)
parseHex :: Monad m => Z.Parser m ByteString
parseHex = do
  q <- Z.take 4 >>= S.lift . BS.toStrict_
  if B.all isHexChar q
     then pure q
     else mzero
{-# INLINE parseHex #-}

-- | universal parser that skips over arbitrary-type JSON values
--   does not perform any sanity validation
--
--   consumes leading word8 of value before calling type-specific parsers
skipValue :: Monad m => Z.Parser m ()
skipValue =
    Z.pop >>= \case
        Quote -> skipToEndQ       -- leading '"' implies string
        Minus -> skipNumber True  -- numbers that begin in '-' must have at least one digit
        LBracket -> skipArray
        LBrace -> skipObject
        Lit_n -> _null  *> Z.skipSpace
        Lit_t -> _true  *> Z.skipSpace
        Lit_f -> _false *> Z.skipSpace
        w | isDigit w -> skipNumber False
        _  -> mzero
    where
        -- character sequence required for JSON literals (null, true, false)
        -- is truncated at head and therefore less transparent than named constants
        _null  = Z.string "ull"
        _true  = Z.string "rue"
        _false = Z.string "alse"
        {-# INLINE _null #-}
        {-# INLINE _true #-}
        {-# INLINE _false #-}

-- | skipArray : skips over contents of JSON-formatted array value,
--               ignoring internal whitespace
skipArray :: Monad m => Z.Parser m ()
skipArray = do
    Z.skipSpace
    Z.peek >>= \case
        Just RBracket -> Z.pop *> Z.skipSpace
        _   -> skipVals -- pattern-match does not distinguish between non-] character and EOF
  where
    skipVals = go
      where
        go = do
          skipValue
          (Z.pop <* Z.skipSpace) >>= \case
            Comma -> go
            RBracket -> pure ()
            _    -> mzero
    {-# INLINE skipVals #-}
{-# INLINE skipArray #-}

-- | skipNumber : numbers contain no special characters and can be skipped
--   efficiently without validation.
skipNumber :: Monad m => Bool -> Z.Parser m ()
skipNumber wantDigit = Z.skipWhile nonTerminal >> Z.skipSpace
  where
    nonTerminal :: Word8 -> Bool
    nonTerminal Comma = False
    nonTerminal RBracket = False
    nonTerminal RBrace = False
    nonTerminal w = not $ isSpace w
    {-# INLINE nonTerminal #-}

-- | efficiently skips to end of current object without validating sanity of contents
skipRestObj :: Monad m => Z.Parser m ()
skipRestObj = do
    Z.skipWhile $ not . isSpecial
    Z.pop >>= \case
        RBrace   -> pure ()
        Quote    -> skipToEndQ >> skipRestObj
        LBracket -> skipRestArr >> skipRestObj
        LBrace   -> skipRestObj >> skipRestObj
        _    -> mzero

-- | efficiently skips to end of current array without validating sanity of contents
skipRestArr :: Monad m => Z.Parser m ()
skipRestArr = do
    Z.skipWhile $ not . isSpecial
    Z.pop >>= \case
        RBracket -> pure ()
        Quote    -> skipToEndQ >> skipRestArr
        LBracket -> skipRestArr >> skipRestArr
        LBrace   -> skipRestObj >> skipRestArr
        _    -> mzero

-- | generic fast-skip to matching terminal symbol
skipQuick :: Monad m => Word8 -> Z.Parser m ()
skipQuick end = do
    Z.skipWhile $ not . isSpecial
    Z.pop >>= \w ->
        if w == end
        then pure ()
        else case w of
            Quote    -> skipToEndQ >> skipQuick end
            LBracket -> skipQuick RBracket >> skipQuick end
            LBrace   -> skipQuick RBrace >> skipQuick end
            _    -> mzero

-- | Skip any trailing list of object keys and values and final close-brace
-- starting at the initial comma.
--
skipTail :: Monad m => Z.Parser m ()
skipTail = Z.pop >>= \case
    Comma  -> Z.skipSpace >> Z.word8 Quote *> skipKeyVals
    RBrace -> pure ()
    _    -> mzero

-- | Starting just after the double-quote of the first key, Skip key-value
-- pairs to the end of the object, including final close-brace.
--
skipKeyVals :: Monad m => Z.Parser m ()
skipKeyVals = do
    skipToEndQ >> symbol Colon >> skipValue
    skipTail

-- | Skip object including final close-brace and trailing whitespace.
--
skipObject :: Monad m => Z.Parser m ()
skipObject = Z.skipSpace >> Z.pop >>= \case
    RBrace -> Z.skipSpace
    Quote  -> skipKeyVals >> Z.skipSpace
    _    -> mzero
