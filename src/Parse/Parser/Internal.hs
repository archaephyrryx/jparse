{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Parse.Parser.Internal where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)

import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Unsafe as U
import           Data.ByteString.Unsafe (unsafeIndex)

import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)

import qualified Data.ByteString.Char8 as S8

import           Data.Word (Word8)

#define USE_HANDROLL 0

#if USE_HANDROLL
type Input = L.ByteString
type Remainder = L.ByteString
type FailState = ()

data ParseResult a
  = Parsed a Remainder
  | Partial (Input -> ParseResult a)
  | Failure FailState

type ParseState = (Input, !Int, !Int)


data Parser a = Parser
  { parse :: ParseState -> (ParseResult a, ParseState) }

instance Functor ParseResult where
  fmap f (Parsed x b) = Parsed (f x) b
  fmap f (Partial cont) = Partial (fmap f . cont)
  fmap f x@(Failure _) = x

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s0 ->
    let (x, s1) = p s0 in
        (fmap f x, s1)

instance Applicative Parser where
  (Parser p1)<*>(Parser p2) = Parser $ \s0 ->
    let (f, s1) = p1 s0
        (x, s2) = p2 s1
     in (f x, s2)
  pure x = Parser (\s -> (x, s))

instance Monad Parser where
  return = pure
  (Parser p1)>>=f = Parser $ \s0 ->
    let (x, s1) = p1 s0
     in f x $ s1

instance MonadPlus



runParser :: Parser a -> a
runParser = \p -> let (x, _) = parse p in x
{-# INLINE runParser #-}

peekWord8 :: Parser Word8
peekWord8 = Parser peek
  where
    peek p@(b,!i) = (b `unsafeIndex` i, p)

anyWord8 :: Parser Word8
anyWord8 = Parser get
  where
    get p@(b,!i) = (b `unsafeIndex` i, (b,i+1))

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = Parser $
#endif
