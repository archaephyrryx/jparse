{-# LANGUAGE CPP #-}

#define ATTOPARSEC 0
#define ZEPTO 1
#define UNUSED 2

module Parse.Parser where

import qualified Data.Attoparsec.ByteString.Char8 as A (isSpace_w8)
import           Data.Word (Word8)

#define PARSER ZEPTO


#if (PARSER==ATTOPARSEC)
import Parse.Parser.Attoparsec
#elif (PARSER==ZEPTO)
import Parse.Parser.Zepto
#else
skipWhile :: Monad m => m ()
skipWhile = error "no parser currently specified"
#endif

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
