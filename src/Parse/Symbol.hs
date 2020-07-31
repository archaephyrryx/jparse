{-# LANGUAGE PatternSynonyms #-}

module Parse.Symbol
  ( symbol
  , token
  , pattern Bslash
  , pattern Quote
  , pattern Slash
  , pattern Minus
  , pattern Comma
  , pattern Period
  , pattern Colon
  , pattern Esc_b, pattern Esc_f, pattern Esc_n, pattern Esc_r, pattern Esc_t
  , pattern Ctr_b, pattern Ctr_f, pattern Ctr_n, pattern Ctr_r, pattern Ctr_t
  , pattern LBracket, pattern LBrace
  , pattern RBracket, pattern RBrace
  , pattern Lit_n, pattern Lit_t, pattern Lit_f
  , pattern Hex_u
  , pattern Hex_0, pattern Hex_9
  , pattern Hex_A, pattern Hex_F
  , pattern Hex_a, pattern Hex_f
  ) where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (skipSpace)

import           Data.Word (Word8)

-- | symbol : skips a single-character token and any trailing whitespace
symbol :: Word8 -> A.Parser ()
symbol w = A.word8 w *> A.skipSpace
{-# INLINE symbol #-}

-- | token : parses an arbitrary single-character token and any trailing whitespace
token = A.anyWord8 <* A.skipSpace
{-# INLINE token #-}

-- * Single-character pattern synonyms, with occasional overlap

-- ** Patterns for special parse-interrupt characters

-- | ASCII '\\'
pattern Bslash :: Word8
pattern Bslash = 0x5c

-- | ASCII '"'
pattern Quote :: Word8
pattern Quote = 0x22

-- | ASCII '-'
pattern Minus :: Word8
pattern Minus = 0x2d

-- | ASCII ','
pattern Comma :: Word8
pattern Comma = 0x2c

-- | ASCII '.'
pattern Period :: Word8
pattern Period = 0x2e

-- | ASCII ':'
pattern Colon :: Word8
pattern Colon = 0x3a

-- ** Patterns for escaped characters

-- | ASCII '/'
pattern Slash :: Word8
pattern Slash = 0x2F

-- | ASCII 'b'
pattern Esc_b :: Word8
pattern Esc_b = 0x62

-- | ASCII 'f'
pattern Esc_f :: Word8
pattern Esc_f = 0x66

-- | ASCII 'n'
pattern Esc_n :: Word8
pattern Esc_n = 0x6E

-- | ASCII 'r'
pattern Esc_r :: Word8
pattern Esc_r = 0x72

-- | ASCII 't'
pattern Esc_t :: Word8
pattern Esc_t = 0x74

-- ** Patterns for control characters

-- | ASCII '\b'
pattern Ctr_b :: Word8
pattern Ctr_b = 0x08

-- | ASCII '\f'
pattern Ctr_f :: Word8
pattern Ctr_f = 0x0c

-- | ASCII '\n'
pattern Ctr_n :: Word8
pattern Ctr_n = 0x0a

-- | ASCII '\r'
pattern Ctr_r :: Word8
pattern Ctr_r = 0x0d

-- | ASCII '\t'
pattern Ctr_t :: Word8
pattern Ctr_t = 0x09

-- ** Patterns for delimiters

-- | ASCII '['
pattern LBracket :: Word8
pattern LBracket = 0x5b
-- | ASCII ']'
pattern RBracket :: Word8
pattern RBracket = 0x5d
-- | ASCII '{'
pattern LBrace :: Word8
pattern LBrace = 0x7b
-- | ASCII '}'
pattern RBrace :: Word8
pattern RBrace = 0x7d

-- ** Patterns for specific literal ASCII chars

pattern Lit_n :: Word8
pattern Lit_n = 0x6e

pattern Lit_t :: Word8
pattern Lit_t = 0x74

pattern Lit_f :: Word8
pattern Lit_f = 0x66

-- ** Patterns for hexadecimal quads

-- | Pattern for ASCII 'u'
pattern Hex_u :: Word8
pattern Hex_u = 0x75

-- *** Patterns for range bounds on ASCII characters that are valid hexadecimal digits

-- | ASCII '0'
pattern Hex_0 :: Word8

-- | ASCII '9'
pattern Hex_9 :: Word8

-- | ASCII 'A'
pattern Hex_A :: Word8

-- | ASCII 'F'
pattern Hex_F :: Word8

-- | ASCII 'a'
pattern Hex_a :: Word8

-- | ASCII 'f'
pattern Hex_f :: Word8

pattern Hex_0 = 0x30
pattern Hex_9 = 0x39
pattern Hex_A = 0x41
pattern Hex_F = 0x46
pattern Hex_a = 0x61
pattern Hex_f = 0x66
