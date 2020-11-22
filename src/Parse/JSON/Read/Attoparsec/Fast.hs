{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}


module Parse.JSON.Read.Attoparsec.Fast where

import           Control.Monad (mzero, when)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isDigit_w8, isSpace_w8, skipSpace)
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import           Data.Word (Word8)

import           Parse.Symbol
import           Parse.JSON.Read.Internal
import           Parse.JSON.Read.Attoparsec.Common

-- | parseToEndQ : parses the payload of a JSON-formatted string
--   silently consumes end-quote character and any trailing whitespace
--   implicitly requires that the first doublequote has been consumed
--   any other pre-consumed characters will be omitted from the parser result
parseToEndQ :: A.Parser Builder
parseToEndQ = parseQBuilder <* A.skipSpace
    where
        parseQBuilder :: A.Parser Builder
        parseQBuilder = do
           sim <- D.byteString <$> A.takeWhile isSimple
           A.anyWord8 >>= \case
              Quote  -> pure sim
              _      -> do
                e <- parseEscaped
                ((sim <> e) <>) <$> parseQBuilder
{-# INLINE parseToEndQ #-}

-- | skipToEndQ : efficiently skips the remainder of a JSON-formatted string
--   can be called partway into string parsing provided that no characters
--   are orphaned from associated preceding unescaped backslashes
--
--   does not check for EOF or invalid backslash escapes
skipToEndQ :: A.Parser ()
skipToEndQ = skipQUnit >> A.skipSpace
  where
    skipQUnit :: A.Parser ()
    skipQUnit = go
      where
        go = do
          A.skipWhile isSimple
          A.anyWord8 >>= \case
            Quote  -> pure ()
            _      -> A.anyWord8 >> go
    {-# INLINE skipQUnit #-}
{-# INLINE skipToEndQ #-}

-- | universal parser that skips over arbitrary-type JSON values
--   does not perform any sanity validation
--
--   consumes leading word8 of value before calling type-specific parsers
skipValue :: A.Parser ()
skipValue =
    A.anyWord8 >>= \case
        Quote -> skipToEndQ       -- leading '"' implies string
        Minus -> skipNumber True  -- numbers that begin in '-' must have at least one digit
        LBracket -> skipArray
        LBrace -> skipObject
        Lit_n -> _null  *> A.skipSpace
        Lit_t -> _true  *> A.skipSpace
        Lit_f -> _false *> A.skipSpace
        w | A.isDigit_w8 w -> skipNumber False
        _  -> mzero
    where
        -- character sequence required for JSON literals (null, true, false)
        -- is truncated at head and therefore less transparent than named constants
        _null  = A.string "ull"
        _true  = A.string "rue"
        _false = A.string "alse"
        {-# INLINE _null #-}
        {-# INLINE _true #-}
        {-# INLINE _false #-}

-- | skipArray : skips over contents of JSON-formatted array value,
--               ignoring internal whitespace
skipArray :: A.Parser ()
skipArray = do
    A.skipSpace
    A.peekWord8 >>= \case
        Just RBracket -> A.anyWord8 *> A.skipSpace
        _   -> skipVals -- pattern-match does not distinguish between non-] character and EOF
  where
    skipVals = do
        skipValue
        token >>= \case
            Comma -> skipVals
            RBracket -> pure ()
            _    -> mzero

-- | skipNumber : numbers contain no special characters and can be skipped
--   efficiently without validation.
skipNumber :: Bool -> A.Parser ()
skipNumber wantDigit = do
    when wantDigit $ A.skip A.isDigit_w8
    A.skipWhile nonTerminal
    A.skipSpace
  where
    nonTerminal :: Word8 -> Bool
    nonTerminal Comma = False
    nonTerminal RBracket = False
    nonTerminal RBrace = False
    nonTerminal w = not $ A.isSpace_w8 w

-- | Skip any trailing list of object keys and values and final close-brace
-- starting at the initial comma.
--
skipTail :: A.Parser ()
skipTail = A.anyWord8 >>= \case
    Comma  -> A.skipSpace >> A.word8 Quote *> skipKeyVals
    RBrace -> pure ()
    _    -> mzero

-- | Starting just after the double-quote of the first key, Skip key-value
-- pairs to the end of the object, including final close-brace.
--
skipKeyVals :: A.Parser ()
skipKeyVals = do
    skipToEndQ >> symbol Colon >> skipValue
    skipTail

-- | Skip object including final close-brace and trailing whitespace.
--
skipObject :: A.Parser ()
skipObject = A.skipSpace >> A.anyWord8 >>= \case
    RBrace -> A.skipSpace
    Quote  -> skipKeyVals >> A.skipSpace
    _    -> mzero


-- | efficiently skips to end of current object without validating sanity of contents
skipRestObj :: A.Parser ()
skipRestObj = do
    A.skipWhile $ not . isSpecial
    A.anyWord8 >>= \case
        RBrace   -> pure ()
        Quote    -> skipToEndQ >> skipRestObj
        LBracket -> skipRestArr >> skipRestObj
        LBrace   -> skipRestObj >> skipRestObj
        _    -> mzero

-- | efficiently skips to end of current array without validating sanity of contents
skipRestArr :: A.Parser ()
skipRestArr = do
    A.skipWhile $ not . isSpecial
    A.anyWord8 >>= \case
        RBracket -> pure ()
        Quote    -> skipToEndQ >> skipRestArr
        LBracket -> skipRestArr >> skipRestArr
        LBrace   -> skipRestObj >> skipRestArr
        _    -> mzero

-- | generic fast-skip to matching terminal symbol
--
-- @skipQuick@ is less efficient than 'skipRestObj' or 'skipRestArr' for @]@ or @}@ (respectively).
-- Please use those functions instead of calling @skipQuick 0x5d@ or @skipQuick 0x7d@ (same for
-- calls using the synonyms 'RBrace' and 'RBracket').
skipQuick :: Word8 -> A.Parser ()
skipQuick end = do
    A.skipWhile $ not . isSpecial
    A.anyWord8 >>= \w ->
        if w == end
        then pure ()
        else case w of
            Quote    -> skipToEndQ >> skipQuick end
            LBracket -> skipQuick RBracket >> skipQuick end
            LBrace   -> skipQuick RBrace >> skipQuick end
            _    -> mzero
