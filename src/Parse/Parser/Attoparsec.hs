module Parse.Parser.Attoparsec where

import qualified Data.ByteString as B
import           Data.ByteString      (ByteString)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isSpace)
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

{-
A.satisfy
A.skip

A.skipSpace
A.skipWhile
A.string
A.stringCI
A.takeWhile
A.word8
-}
