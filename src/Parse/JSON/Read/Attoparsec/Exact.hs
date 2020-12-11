{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}


module Parse.JSON.Read.Attoparsec.Exact
  ( parseToEndQ
  , skipToEndQ
  , skipRestObj
  , skipValue
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as D

import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)
import           Data.ByteString.Builder (Builder)
import           Data.Word (Word8)

import qualified Parse.Parser.Attoparsec as A

import           Util.Helper (if_)

import           Parse.ASCII.Attoparsec (symbol, token)
import           Parse.ASCII.ByteLiterals
import           Parse.JSON.Read.Internal
import           Parse.JSON.Read.Attoparsec.Common

-- | Parses the payload of a JSON-formatted string, silently consumimg
--   the closing double-quote character and any following whitespace.
--   This combinator implicitly assumes that the opening double-quote
--   character has been consumed, and the result will only include the
--   right-justified substring beginning with the first unconsumed character;
--   any characters after the opening double-quote that have already been
--   consumed are omitted from the parser return value.
parseToEndQ :: A.Parser Builder
parseToEndQ = parseQBuilder <* A.skipSpace
  where
    parseQBuilder :: A.Parser Builder
    parseQBuilder = do
        sim <- D.byteString <$> A.takeWhile isSimple
        A.peek >>= \case
            Just Quote -> A.pop >> pure sim
            _ -> do
                e <- parseEscapes
                ((sim <> e) <>) <$> parseQBuilder

-- | Efficiently skips the remainder of a JSON-formatted string and any subsequent
--   whitespace, provided that the opening double-quote has been consumed already.
--  
--   This parser may be called partway into the JSON-string as long as the first byte
--   in the buffer isn't an escape sequence whose escaping backslash character was already
--   consumed.
--  
--   This combinator informally validates the sanity of the JSON-string contents; for instance,
--   quartet escapes containing non-hexadecimal bytes will produce a parser error,
--   whereas valid hexadecimal quartets that are invalid as codepoints will not.
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
      else (b <>) <$> parseEscaped

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
skipEscaped = A.pop >>= \case
    Quote -> pure ()
    Slash -> pure ()
    Esc_b -> pure ()
    Esc_f -> pure ()
    Esc_n -> pure ()
    Esc_r -> pure ()
    Esc_t -> pure ()
    Hex_u -> do
        quad <- A.take 4
        if B.all isHexChar quad
          then pure ()
          else mzero
    _     -> mzero
{-# INLINE skipEscaped #-}

-- | universal parser that skips over arbitrary-type JSON values
--   validates sanity of value skipped provided subordinate functions do so
--
--   consumes leading word8 of value before calling type-specific parsers
skipValue :: A.Parser ()
skipValue =
    A.pop >>= \case
        Quote -> skipToEndQ       -- leading '"' implies string
        Minus -> skipNumber True  -- numbers that begin in '-' must have at least one digit
        LBracket -> skipArray
        LBrace -> skipObject
        Lit_n -> _null  *> A.skipSpace
        Lit_t -> _true  *> A.skipSpace
        Lit_f -> _false *> A.skipSpace
        w | isDigit w -> skipNumber False
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
    A.peek >>= \case
        Just RBracket -> A.pop *> A.skipSpace
        _ -> skipVals -- pattern-match does not distinguish between non-] character and EOF
  where
    skipVals = do
        skipValue
        A.pop >>= \case
            Comma -> A.skipSpace >> skipVals
            RBracket -> A.skipSpace
            _    -> mzero

-- | skipNumber : skips a JSON-formatted number value whose first character has been consumed
--     requires a boolean argument that optionally mandates at least one digit
--        to be set when only a unary-minus has been pre-consumed
skipNumber :: Bool -> A.Parser ()
skipNumber wantDigit = do
    if_ wantDigit skipWhile1 A.skipWhile isDigit
    skipDecimal  <|> pure ()
    skipExponent <|> pure ()
    A.skipSpace
  where
    skipDecimal = void $ do
      A.word8 Period
      skipWhile1 isDigit

    skipExponent = skipE >> skipSign >> skipDigits
      where
        -- skips ASCII 'E' (case-insensitive)
        skipE :: A.Parser ()
        skipE = void $ A.word8 0x45 <|> A.word8 0x65
        {-# INLINE skipE #-}

        -- optionally skips ASCII '+' or '-'
        skipSign :: A.Parser ()
        skipSign = void (A.word8 0x2b <|> A.word8 0x2d) <|> pure ()
        {-# INLINE skipSign #-}

        -- skips one or more digits
        skipDigits :: A.Parser ()
        skipDigits = skipWhile1 isDigit
        {-# INLINE skipDigits #-}
{-# INLINE skipNumber #-}

-- | Skip any trailing list of object keys and values and final close-brace
-- starting at the initial comma.
skipTail :: A.Parser ()
skipTail = A.pop >>= \case
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
skipObject = A.skipSpace >> A.pop >>= \case
    RBrace -> A.skipSpace
    Quote  -> skipKeyVals >> A.skipSpace
    _    -> mzero

-- | efficiently skips to end of current object without validating sanity of contents
skipRestObj :: A.Parser ()
skipRestObj = do
    A.skipWhile $ not . isSpecial
    A.pop >>= \case
        RBrace   -> pure ()
        Quote    -> skipToEndQ >> skipRestObj
        LBracket -> skipRestArr >> skipRestObj
        LBrace   -> skipRestObj >> skipRestObj
        _    -> mzero

-- | efficiently skips to end of current array without validating sanity of contents
skipRestArr :: A.Parser ()
skipRestArr = do
    A.skipWhile $ not . isSpecial
    A.pop >>= \case
        RBracket -> pure ()
        Quote    -> skipToEndQ >> skipRestArr
        LBracket -> skipRestArr >> skipRestArr
        LBrace   -> skipRestObj >> skipRestArr
        _    -> mzero
