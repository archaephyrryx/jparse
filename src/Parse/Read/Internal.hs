module Parse.Read.Internal
  ( isSimple
  , isBslash
  , isHexChar
  , escAtom
  , isSpecial
  , isDigit
  , isSpace
  )
    where

import qualified Data.Attoparsec.ByteString.Char8 as A (isDigit_w8, isSpace_w8)

import           Data.Word (Word8)
import           Parse.Symbol

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
escAtom Bslash = True
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

isDigit :: Word8 -> Bool
isDigit = A.isDigit_w8
{-# INLINE isDigit #-}

isSpace :: Word8 -> Bool
isSpace = A.isSpace_w8
{-# INLINE isSpace #-}
