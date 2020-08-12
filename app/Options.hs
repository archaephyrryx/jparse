{-# LANGUAGE MultiWayIf #-}

module Options where

import Data.ByteString (ByteString)

import Data.List (stripPrefix)
import Data.Maybe (isJust, fromJust, mapMaybe)

import JParse (qkey)

-- * Command-line specification of input processing mode

-- | Algebraic data type encapsulating the two primary modes of operation:
--
-- * @BlockMode@ for data that is either formatted with multiple objects per line, multiple lines per object, or arbitrarily long lines that are not safe to read into memory as a unit.
--
-- * @LineMode@ for (theoretically) more efficient parsing of data where every line contains exactly one complete JSON object, and lines are trusted to be sufficiently short to permit reading entire lines into memory as a unit.
--
-- * Additionally supports @DebugMode@ for human-readable printing of the raw return values of line-mode parser, rather than printing them in a machine-readable way after filtering and eliminating extraneous constructors around the actual result value.
data Mode = BlockMode -- ^ Attoparsec inside Conduit
          | LineMode -- ^ Zepto inside Streaming
          | DebugMode -- ^ Zepto inside Streaming (verbose)
          deriving (Eq)

getKeyMode :: [String] -> (ByteString, Mode)
getKeyMode xs = (qkey xs, getMode xs)
{-# INLINE getKeyMode #-}

getMode :: [String] -> Mode
getMode xs =
  case mapMaybe (stripPrefix "--mode=") xs of
    [] -> BlockMode
    x:_ -> if | x == "line" -> LineMode
              | x == "block" -> BlockMode
              | x == "debug" -> DebugMode
              | otherwise -> error $ "unrecognized mode \""++x++"\""
{-# INLINE getMode #-}

