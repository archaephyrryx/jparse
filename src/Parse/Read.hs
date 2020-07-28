{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}


module Parse.Read where

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
           A.eitherP eQuote parseEscapes >>= \case
                Left _ -> pure sim
                Right e -> ((sim <> e) <>) <$> parseQBuilder

-- | skipToEndQ : efficiently skips the remainder of a JSON-formatted string
--   can be called partway into string parsing provided that no characters
--   are orphaned from associated preceding unescaped backslashes
--   informally validates sanity of contents but does not check for invalid hexadecimal quartet escapes
skipToEndQ :: A.Parser ()
skipToEndQ = skipQUnit >> A.skipSpace
    where
        skipQUnit :: A.Parser ()
        skipQUnit = do
            A.skipWhile isSimple
            void eQuote <|> (skipEscapes >> skipQUnit)

-- | escape-parser optimized for many consecutive backslashes
parseEscapes :: A.Parser Builder
parseEscapes = do
    bs <- A.takeWhile isBslash
    let l = B.length bs
        b = D.byteString bs
    if even l
        then pure b
        else (b<>) <$> parseEscaped

-- | efficiently skips over escaped characters
skipEscapes :: A.Parser ()
skipEscapes = do
    n <- B.length <$> A.takeWhile isBslash
    when (odd n) skipEscaped


-- | basic parser that interprets escaped characters (except backslash)
--   Note: this parser employs backtracking to optimize Builder construction
parseEscaped :: A.Parser Builder
parseEscaped =  (D.word8      <$> A.satisfy escAtom)
            <|> (D.byteString <$>          parseHex)






-- | skipEscaped : efficiently skips over escaped character sequences (omitting leading backslash)
--   Avoids backtracking (in contrast to parseEscaped) as skipping character-by-character
--   does not incur the same overhead as constructing Builders character-by-character
skipEscaped :: A.Parser ()
skipEscaped =
    A.anyWord8 >>= \case
        Quote -> pure ()
        Slash -> pure ()
        Esc_b -> pure ()
        Esc_f -> pure ()
        Esc_n -> pure ()
        Esc_r -> pure ()
        Esc_t -> pure ()
        Hex_u -> void $ A.count 4 skipHexChar
        _    -> mzero



-- | parses uXXXX hexcodes
parseHex :: A.Parser ByteString
parseHex = B.pack <$> ((:) <$> A.word8 Hex_u <*> A.count 4 parseHexChar)
  where
    parseHexChar = A.satisfy isHexChar
    {-# INLINE parseHexChar #-}

{-
-- XXX: orphaned counterpart to parseHex that is not currently used
skipHex :: A.Parser ()
skipHex = void $ A.word8 0x75 >> A.count 4 skipHexChar
-}

-- | skipHexChar : consumes a single hexadecimal character (sanity-checking)
skipHexChar :: A.Parser ()
skipHexChar = A.skip isHexChar
{-# INLINE skipHexChar #-}

-- | universal parser that skips over arbitrary-type JSON values
--   validates sanity of value skipped provided subordinate functions do so
--
--   consumes leading word8 of value before calling type-specific parsers
skipValue :: A.Parser ()
skipValue =
    A.anyWord8 >>= \case
        Quote -> skipToEndQ       -- leading '"' implies string
        Minus -> skipNumber True  -- numbers that begin in '-' must have at least one digit
        LBracket -> skipArray
        LBrace -> skipObject
        Lit_n -> A.string _null  *> A.skipSpace
        Lit_t -> A.string _true  *> A.skipSpace
        Lit_f -> A.string _false *> A.skipSpace
        w     | A.isDigit_w8 w
             -> skipNumber False
        _    -> mzero
    where
        -- character sequence required for JSON literals (null, true, false)
        -- is truncated at head and therefore less transparent than named constants
        _null  = "ull"
        _true  = "rue"
        _false = "alse"
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
        _         -> skipVals -- pattern-match does not distinguish between non-] character and EOF
  where
    skipVals = do
        skipValue
        A.anyWord8 >>= \case
            Comma -> A.skipSpace >> skipVals
            RBracket -> A.skipSpace
            _    -> mzero

-- | skipNumber : skips a JSON-formatted number value whose first character has been consumed
--     requires a boolean argument that optionally mandates at least one digit
--        to be set when only a unary-minus has been pre-consumed
skipNumber :: Bool -> A.Parser ()
skipNumber wantDigit = do
    when wantDigit $ A.skip A.isDigit_w8
    A.skipWhile A.isDigit_w8
    A.option () . void $ A.word8 Period >> skipWhile1 A.isDigit_w8
    A.option () skipExponent
    A.skipSpace
  where
    skipExponent = do
        void $ A.skip isE
        A.option () $ A.skip plusMinus
        skipWhile1 A.isDigit_w8

    -- Returns true only for ASCII 'E' or 'e'
    isE 0x45 = True
    isE 0x65 = True
    isE _    = False
    {-# INLINE isE #-}

    -- Returns true only for ASCII '+' or '-'
    plusMinus 0x2b = True
    plusMinus 0x2d = True
    plusMinus _    = False
    {-# INLINE plusMinus #-}


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
{-# NOINLINE [1] skipQuick #-}
{-# RULES
"skipQuick/skipRestObj" [2] skipQuick (125 :: Word8) = skipRestObj
"skipQuick/skipRestArr" [2] skipQuick (93 :: Word8) = skipRestArr
  #-}

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
