{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming as BS

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as D

import JParse
import JParse.Zepto
import Options

import JParse.Global

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
  mbs <- withConf conf $ generate gated zipped http
  case mode of
    LineMode  -> lineParse conf query mbs
    BlockMode -> blockParse query mbs

blockParse :: String -> BS.ByteString IO () -> IO ()
blockParse = runParses . strToAtto'
{-# INLINE blockParse #-}

{-
blockParse' :: String -> BS.ByteString IO () -> IO ()
blockParse' = runParsed . strToAtto'
{-# INLINE blockParse' #-}
-}

lineParse :: GlobalConf -> String -> BS.ByteString IO () -> IO ()
lineParse conf s mbs =
  S.mapM_ B8.putStr $
    lineParseFold conf (strToZepto s) concatLine mempty buildLong $ mbs
{-# INLINE lineParse #-}

concatLine :: D.Builder -> D.Builder -> D.Builder
concatLine bld rest = bld <> D.word8 0xa <> rest
{-# INLINE concatLine #-}
