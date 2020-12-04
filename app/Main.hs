{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Streaming.Prelude as S
import qualified Streaming.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as D

import Options.Applicative
import System.IO (stdout)

import Gates
import JParse
import JParse.Zepto
import JParse.Global
import Options
import Util.ByteString.Build

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

blockParse :: String -> BS.ByteStream IO () -> IO ()
blockParse q mbs =
  S.mapM_ putLnBuilder $ blockParseStream (strToAtto' q) mbs
  where
    putLnBuilder bld = D.hPutBuilder stdout (bld <> D.word8 0xa)
{-# INLINE blockParse #-}

{-
blockParse' :: String -> BS.ByteStream IO () -> IO ()
blockParse' = runParsed . strToAtto'
{-# INLINE blockParse' #-}
-}

lineParse :: GlobalConf -> String -> BS.ByteStream IO () -> IO ()
lineParse conf s mbs =
  S.mapM_ B8.putStr $
    lineParseFold conf (strToZepto s) concatLine mempty buildLong $ mbs
{-# INLINE lineParse #-}

concatLine :: D.Builder -> D.Builder -> D.Builder
concatLine bld rest = bld <> D.word8 0xa <> rest
{-# INLINE concatLine #-}
