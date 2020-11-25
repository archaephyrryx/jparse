-- | Limited re-export of "Data.Attoparsec.ByteString" combinators under
--   aliases uniform with "Parse.Parser.Zepto" combinator names
module Parse.Parser.Attoparsec where

import           Data.ByteString      (ByteString)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (stringCI, isSpace_w8)
import Control.Monad (void)
import Data.Word (Word8)

type Parser = A.Parser

-- | Returns the next byte of input without consuming it. Alias for 'A.peekWord8'
peek :: Parser (Maybe Word8)
peek = A.peekWord8
{-# INLINE peek #-}

-- | Consumes and returns a single byte of input. Alias for 'A.anyWord8'
pop :: Parser Word8
pop = A.anyWord8
{-# INLINE pop #-}

-- | Consumes and returns a prefix of the specified length. Alias for 'A.take'
take :: Int -> Parser ByteString
take = A.take
{-# INLINE take #-}

-- | Consumes and discards a prefix of the specified length. Shorthand for @void . take@
skip :: Int -> Parser ()
skip n = void $ A.take n
{-# INLINE skip #-}

-- | Consumes and returns a matching prefix. Alias for 'A.string'
string :: ByteString -> Parser ByteString
string = A.string
{-# INLINE string #-}

-- | Consumes and returns a case-insensitively matching prefix. Alias for 'A.stringCI'
stringCI :: ByteString -> Parser ByteString
stringCI = A.stringCI
{-# INLINE stringCI #-}

-- | Consumes and discards longest prefix of bytes for which a predicate holds. Alias for 'A.skipWhile'
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile = A.skipWhile
{-# INLINE skipWhile #-}

skipSpace :: Parser ()
skipSpace = skipWhile A.isSpace_w8
{-# INLINE skipSpace #-}

-- | Consumes and returns longest prefix of bytes for which a predicate holds. Alias for 'A.skipWhile'
takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile = A.takeWhile
{-# INLINE takeWhile #-}

-- | Consumes and returns a matching byte. Alias for 'A.word8'
word8 :: Word8 -> Parser Word8
word8 = A.word8
{-# INLINE word8 #-}
