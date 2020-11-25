{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}


module Parse.JSON.Read.Attoparsec.Exact where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isDigit_w8, skipSpace)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import           Data.Word (Word8)

import           Parse.ASCII.ByteLiterals
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
{-# INLINE skipToEndQ #-}

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
{-# INLINE skipEscapes #-}

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
{-# INLINE skipEscaped #-}

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
        Lit_n -> _null  *> A.skipSpace
        Lit_t -> _true  *> A.skipSpace
        Lit_f -> _false *> A.skipSpace
        w     | A.isDigit_w8 w
             -> skipNumber False
        _    -> mzero
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
        _ -> skipVals -- pattern-match does not distinguish between non-] character and EOF
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


-- | Skip any trailing list of object keys and values and final close-brace
-- starting at the initial comma.
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
