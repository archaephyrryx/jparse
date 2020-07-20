{-# LANGUAGE PatternSynonyms #-}

module Parse.Symbol
  ( symbol
  , pattern Bslash
  , pattern Quote
  , pattern Slash
  , pattern Esc_b
  , pattern Esc_f
  , pattern Esc_n
  , pattern Esc_r
  , pattern Esc_t
  , pattern LBracket
  , pattern RBracket
  , pattern LBrace
  , pattern RBrace
  , pattern Hex_0
  , pattern Hex_9
  , pattern Hex_A
  , pattern Hex_F
  , pattern Hex_a
  , pattern Hex_f
  ) where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isDigit_w8, isSpace_w8, skipSpace)

import           Data.Word (Word8)

-- | symbol : parses a single-character token and any trailing whitespace
symbol :: Word8 -> A.Parser ()
symbol w = A.word8 w *> A.skipSpace
{-# INLINE symbol #-}

-- * Single-character pattern synonyms, with occasional overlap

-- ** Patterns for special parse-interrupt characters

-- | ASCII '\\'
pattern Bslash :: Integral a => a
pattern Bslash = 0x5c

-- | ASCII '"'
pattern Quote :: Integral a => a
pattern Quote = 0x22

-- ** Patterns for escaped characters

-- | ASCII '/'
pattern Slash :: Integral a => a
pattern Slash = 0x2F

-- | ASCII 'b'
pattern Esc_b :: Integral a => a
pattern Esc_b = 0x62

-- | ASCII 'f'
pattern Esc_f :: Integral a => a
pattern Esc_f = 0x66

-- | ASCII 'n'
pattern Esc_n :: Integral a => a
pattern Esc_n = 0x6E

-- | ASCII 'r'
pattern Esc_r :: Integral a => a
pattern Esc_r = 0x72

-- | ASCII 't'
pattern Esc_t :: Integral a => a
pattern Esc_t = 0x74

-- ** Patterns for delimiters

-- | ASCII '['
pattern LBracket :: Integral a => a
pattern LBracket = 0x5b
-- | ASCII ']'
pattern RBracket :: Integral a => a
pattern RBracket = 0x5d
-- | ASCII '{'
pattern LBrace :: Integral a => a
pattern LBrace = 0x7b
-- | ASCII '}'
pattern RBrace :: Integral a => a
pattern RBrace = 0x7d

-- ** Patterns for range bounds on ASCII characters that are valid hexadecimal digits

-- | ASCII '0'
pattern Hex_0 :: Integral a => a

-- | ASCII '9'
pattern Hex_9 :: Integral a => a

-- | ASCII 'A'
pattern Hex_A :: Integral a => a

-- | ASCII 'F'
pattern Hex_F :: Integral a => a

-- | ASCII 'a'
pattern Hex_a :: Integral a => a

-- | ASCII 'f'
pattern Hex_f :: Integral a => a

pattern Hex_0 = 0x30
pattern Hex_9 = 0x39
pattern Hex_A = 0x41
pattern Hex_F = 0x46
pattern Hex_a = 0x61
pattern Hex_f = 0x66
