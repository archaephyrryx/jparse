{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse.JSON.Read.Attoparsec.Fast
  ( parseToEndQ
  , skipToEndQ
  , skipRestObj
  , skipValue
  ) where

import qualified Data.ByteString.Builder as D

import           Control.Monad (mzero, when)
import           Data.ByteString.Builder (Builder)
import           Data.Word (Word8)

import qualified Parse.Parser.Attoparsec as A

import           Parse.ASCII.Attoparsec (symbol, token)
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
        A.pop >>= \case
            Quote -> pure sim
            _     -> do
              e <- parseEscaped
              ((sim <> e) <>) <$> parseQBuilder
{-# INLINE parseToEndQ #-}

-- | Efficiently skips the remainder of a JSON-formatted string and any subsequent
--   whitespace, provided that the opening double-quote has been consumed already.
--  
--   This parser may be called partway into the JSON-string as long as the first byte
--   in the buffer isn't an escape sequence whose escaping backslash character was already
--   consumed.
--  
--   Does not check for EOF, and accepts invalid escape-sequences following an unescaped backslash.
skipToEndQ :: A.Parser ()
skipToEndQ = skipQUnit >> A.skipSpace
  where
    skipQUnit :: A.Parser ()
    skipQUnit = go
      where
        go = do
          A.skipWhile isSimple
          A.pop >>= \case
              Quote  -> pure ()
              _      -> A.pop >> go
    {-# INLINE skipQUnit #-}
{-# INLINE skipToEndQ #-}

-- | universal parser that skips over arbitrary-type JSON values
--   does not perform any sanity validation
--
--   consumes leading word8 of value before calling type-specific parsers
skipValue :: A.Parser ()
skipValue = A.pop >>= \case
      Quote -> skipToEndQ       -- leading '"' implies string
      Minus -> skipNumber True  -- numbers that begin in '-' must have at least one digit
      LBracket -> skipArray
      LBrace -> skipObject
      Lit_n -> _null  *> A.skipSpace
      Lit_t -> _true  *> A.skipSpace
      Lit_f -> _false *> A.skipSpace
      w | isDigit w -> skipNumber False
      _  -> mzero
  where
    -- byte-sequence length required for JSON literals (null, true, false)
    _null  = A.skip 3 -- "ull"
    _true  = A.skip 3 -- "rue"
    _false = A.skip 4 -- "alse"
    {-# INLINE _null #-}
    {-# INLINE _true #-}
    {-# INLINE _false #-}

-- | skips over contents of JSON-formatted array value,
--   ignoring internal whitespace
skipArray :: A.Parser ()
skipArray = do
    A.skipSpace
    A.peek >>= \case
        Just RBracket -> A.pop *> A.skipSpace
        _   -> skipVals -- does not distinguish between non-] and EOF
  where
    skipVals = do
        skipValue
        token >>= \case
            Comma -> skipVals
            RBracket -> pure ()
            _    -> mzero

-- | Skips numbers by consuming until a terminal character is encountered
skipNumber :: Bool -> A.Parser ()
skipNumber wantDigit = do
    when wantDigit $ A.pop >>= \case
      w | isDigit w -> pure ()
      _ -> mzero
    A.skipWhile nonTerminal
    A.skipSpace
  where
    nonTerminal :: Word8 -> Bool
    nonTerminal Comma = False
    nonTerminal RBracket = False
    nonTerminal RBrace = False
    nonTerminal w = not $ isSpace w
    {-# INLINE nonTerminal #-}

-- | Skip any trailing list of object keys and values and final close-brace
-- starting at the initial comma.
skipTail :: A.Parser ()
skipTail = A.pop >>= \case
    Comma  -> A.skipSpace >> A.word8 Quote *> skipKeyVals
    RBrace -> pure ()
    _    -> mzero

-- | Starting just after the double-quote of the first key, skip key-value
-- pairs to the end of the object, including final close-brace.
skipKeyVals :: A.Parser ()
skipKeyVals = do
    skipToEndQ >> symbol Colon >> skipValue
    skipTail

-- | Skip nested object, including terminal '}', as well as any trailing whitespace
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
