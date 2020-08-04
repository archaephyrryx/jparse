{-# LANGUAGE CPP #-}

#define ATTOPARSEC 0
#define ZEPTO 1
#define HANDROLL 2
#define UNUSED 3

module Parse.Parser where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)

import qualified Data.Attoparsec.ByteString.Char8 as A (isSpace_w8)

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)

import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Unsafe as U
import           Data.ByteString.Unsafe (unsafeIndex)

import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)

import           Data.Word (Word8)

#define PARSER ZEPTO


#if (PARSER==ATTOPARSEC)
import Parse.Parser.Attoparsec
#elif (PARSER==ZEPTO)
import Parse.Parser.Zepto
#elif (PARSER==HANDROLL)
import Parse.Parser.Internal
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
