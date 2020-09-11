module Parse.Parser.Attoparsec where

import           Data.ByteString      (ByteString)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (stringCI)
import Control.Monad (void)
import Data.Word (Word8)

type Parser = A.Parser

peek :: Parser (Maybe Word8)
peek = A.peekWord8
{-# INLINE peek #-}

pop :: Parser Word8
pop = A.anyWord8
{-# INLINE pop #-}

take :: Int -> Parser ByteString
take = A.take
{-# INLINE take #-}

skip :: Int -> Parser ()
skip n = void $ A.take n
{-# INLINE skip #-}

string :: ByteString -> Parser ByteString
string = A.string
{-# INLINE string #-}

stringCI :: ByteString -> Parser ByteString
stringCI = A.stringCI
{-# INLINE stringCI #-}

skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile = A.skipWhile
{-# INLINE skipWhile #-}

takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile = A.takeWhile
{-# INLINE takeWhile #-}

word8 :: Word8 -> Parser Word8
word8 = A.word8
{-# INLINE word8 #-}
