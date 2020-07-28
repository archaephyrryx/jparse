{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}


module Parse.ReadAlt where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isDigit_w8, isSpace_w8, skipSpace)
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Char8 as S8
import           Data.Word (Word8)

import           Parse.Symbol

-- * Generic utility-parsers based on low-level attoparsec constructs

-- | skipWhile1 : skips one or more characters for which a predicate holds
skipWhile1 :: (Word8 -> Bool) -> A.Parser ()
skipWhile1 p = A.skip p *> A.skipWhile p
{-# INLINE skipWhile1 #-}

-- * Size-agnostic single-character predicate functions for efficient string-parsing

-- | determines whether a character is a non-escaping string-internal character
isSimple :: Word8 -> Bool
isSimple Bslash = False
isSimple Quote  = False
isSimple _      = True
{-# INLINE isSimple #-}

-- | single-character predicate that only '\' passes
isBslash :: Word8 -> Bool
isBslash Bslash = True
isBslash _      = False
{-# INLINE isBslash #-}

-- | predicate test for case-insensitive hexadecimal characters (0-9,A-F,a-f)
isHexChar :: Word8 -> Bool
isHexChar w = (w >= Hex_0 && w <= Hex_9) ||
              (w >= Hex_A && w <= Hex_F) ||
              (w >= Hex_a && w <= Hex_f)
{-# INLINE isHexChar #-}

-- tests for single-character escape sequences
escAtom :: Word8 -> Bool
escAtom Quote = True
escAtom Slash = True
escAtom Esc_b = True
escAtom Esc_f = True
escAtom Esc_n = True
escAtom Esc_r = True
escAtom Esc_t = True
escAtom _     = False
{-# INLINE escAtom #-}

-- | tests for whether a word8 requires special handling when fast-skipping to closing brace/bracket
isSpecial :: Word8 -> Bool
isSpecial Quote    = True
isSpecial LBracket = True
isSpecial RBracket = True
isSpecial LBrace   = True
isSpecial RBrace   = True
isSpecial _        = False
{-# INLINE isSpecial #-}

-- * String-centric parsers

-- | shorthand parser used only for consuming an
--   unescaped close-quote of a string
eQuote :: A.Parser Word8
eQuote = A.word8 Quote
{-# INLINE eQuote #-}

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

-- | skipToEndQ : efficiently skips the remainder of a JSON-formatted string
--   can be called partway into string parsing provided that no characters
--   are orphaned from associated preceding unescaped backslashes
--
--   does not check for EOF or invalid backslash escapes
skipToEndQ :: A.Parser ()
skipToEndQ = skipQUnit >> A.skipSpace
    where
        skipQUnit :: A.Parser ()
        skipQUnit = do
            A.skipWhile isSimple
            A.anyWord8 >>= \case
              Quote  -> pure ()
              _      -> A.anyWord8 >> skipQUnit
        {-# INLINE skipQUnit #-}

-- | basic parser that interprets escaped characters (except backslash)
parseEscaped :: A.Parser Builder
parseEscaped =
  A.anyWord8 >>= \case
    e | escAtom e -> pure $ D.word8 e
    Hex_u -> do
      q <- parseHex
      pure $ D.word8 Hex_u <> D.byteString q

-- | parses uXXXX hexcodes (without initial u)
parseHex :: A.Parser ByteString
parseHex = do
  q <- A.take 4
  if B.all isHexChar q
     then pure q
     else mzero
  where
    parseHexChar = A.satisfy isHexChar
    {-# INLINE parseHexChar #-}



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
