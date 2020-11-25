module Parse.ASCII.Attoparsec (token, symbol) where

import           Data.Word (Word8)

import qualified Data.Attoparsec.ByteString.Char8 as A (skipSpace)
import           Parse.Parser.Attoparsec (word8, pop, Parser)

-- | Skips a given single-byte literal and any trailing whitespace
symbol :: Word8 -> Parser ()
symbol w = word8 w *> A.skipSpace
{-# INLINE symbol #-}

-- | Parses an arbitrary single-byte token, skipping any trailing whitespace
token :: Parser Word8
token = pop <* A.skipSpace
{-# INLINE token #-}
