{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}


module Parse.JSON.Read.Attoparsec.Common where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (isDigit_w8, skipSpace)
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import           Data.Word (Word8)

import           Parse.Symbol
import           Parse.JSON.Read.Internal

-- * Generic utility-parsers based on low-level attoparsec constructs

-- | skipWhile1 : skips one or more characters for which a predicate holds
skipWhile1 :: (Word8 -> Bool) -> A.Parser ()
skipWhile1 p = A.skip p *> A.skipWhile p
{-# INLINE skipWhile1 #-}

-- * String-centric parsers

-- | shorthand parser used only for consuming an
--   unescaped close-quote of a string
eQuote :: A.Parser Word8
eQuote = A.word8 Quote
{-# INLINE eQuote #-}

-- | basic parser that interprets escaped characters
parseEscaped :: A.Parser Builder
parseEscaped =
  A.anyWord8 >>= \case
    e | escAtom e -> pure $ D.word8 e
    Hex_u -> do
      q <- parseHex
      pure $ D.word8 Hex_u <> D.byteString q
    _ -> mzero
{-# INLINE parseEscaped #-}

-- | parses uXXXX hexcodes (without initial u)
parseHex :: A.Parser ByteString
parseHex = do
  q <- A.take 4
  if B.all isHexChar q
     then pure q
     else mzero
{-# INLINE parseHex #-}

