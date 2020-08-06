module Parse.Parser.Stream where

import qualified Data.Attoparsec.ByteString.Char8 as A (isSpace_w8)
import           Data.Word (Word8)

import Parse.Parser.ZeptoStream

skipSpace :: Parser ()
skipSpace = skipWhile A.isSpace_w8
{-# INLINE skipSpace #-}

-- | symbol : skips a single-character token and any trailing whitespace
symbol :: Word8 -> Parser ()
symbol w = word8 w *> skipSpace
{-# INLINE symbol #-}

-- | token : parses an arbitrary single-character token and any trailing whitespace
token :: Parser Word8
token = pop <* skipSpace
{-# INLINE token #-}
