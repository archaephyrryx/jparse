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
pattern Bslash :: Word8
pattern Bslash = 0x5c

-- | ASCII '"'
pattern Quote :: Word8
pattern Quote = 0x22

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

-- ** Patterns for range bounds on ASCII characters that are valid hexadecimal digits

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
