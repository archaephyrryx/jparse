{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8 (stdin)

import qualified Data.Attoparsec.ByteString as A (parse)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as D
import System.IO (stdout)

import Parse (mapClass, ParseClass)
import JParse
import JParse.Attoparsec
import JParse.Driver
import JParse.Zepto
import Options

import Options.Applicative

import Data.ByteString.Streaming.Gates

import Data.ByteString.Build

opts :: ParserInfo Options
opts =
  info (getOptions <**> helper)
       ( fullDesc
      <> progDesc "Extract values associated with QUERY KEY from JSON data read from standard input"
      <> header "jparse - an efficient single-key JSON value-lookup program" )

main :: IO ()
main = do
  Options{..} <- execParser opts
  let !ckey = mapClass $! query
  mbs <- generate gated zipped http
  case mode of
    LineMode  -> lineParse' ckey mbs
    BlockMode -> blockParse  ckey mbs

blockParse :: [ParseClass] -> BS.ByteString IO () -> IO ()
blockParse !ckey = runParses (seekInObj' ckey)

blockParse' :: [ParseClass] -> BS.ByteString IO () -> IO ()
blockParse' !ckey = runParsed (seekInObj' ckey)

lineParse :: [ParseClass] -> BS.ByteString IO () -> IO ()
lineParse !ckey mbs =
  streamZepto (seekInObjZepto ckey) mbs

lineParse' :: [ParseClass] -> BS.ByteString IO () -> IO ()
lineParse' !ckey mbs =
  S.mapM_ B8.putStr $
    lineParseFold (seekInObjZepto ckey) concatLine mempty buildLong $ mbs

concatLine :: D.Builder -> D.Builder -> D.Builder
concatLine bld rest = bld <> D.word8 0xa <> rest
{-# INLINE concatLine #-}
