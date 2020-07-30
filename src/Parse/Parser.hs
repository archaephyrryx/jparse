{-# LANGUAGE CPP #-}

#define ATTOPARSEC 0
#define ZEPTO 1
#define HANDROLL 2
#define UNUSED 3

module Parse.Parser where

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

#define PARSER UNUSED


#if (PARSER==ATTOPARSEC)
import Parse.Parser.Attoparsec
#elif (PARSER==ZEPTO)
import Parse.Parser.Zepto
#elif (PARSER==HANDROLL)
import Parse.Parser.Internal
#endif





