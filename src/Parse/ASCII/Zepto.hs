module Parse.ASCII.Zepto (token, symbol) where

import Data.Word (Word8)

import Parse.Parser.Zepto (word8, pop, skipSpace, Parser)

-- | Skips a given single-byte literal and any trailing whitespace
symbol :: Word8 -> Parser ()
symbol w = word8 w *> skipSpace
{-# INLINE symbol #-}

-- | Parses an arbitrary single-byte token, skipping any trailing whitespace
token :: Parser Word8
token = pop <* skipSpace
{-# INLINE token #-}
