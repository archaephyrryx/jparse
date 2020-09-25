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
import JParse (seekInObj', seekInObjZepto, runParsec, runParses, runParsed)
import JParse.Attoparsec (putLnBuilderS)
import JParse.Driver (streamZepto, streamZeptoHttp)
import JParse.Zepto (lineParseStream, lineParseFold )
import Options (getOptions, Mode(..), Options(..))

import Options.Applicative
import Sources (getHttp, getStdin, condUnzip, unzip)
import Gates

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
  let ckey = mapClass $! query
  mbs <- generate gated zipped http
  case mode of
    LineMode  -> lineParse'  ckey mbs -- http zipped gated
    BlockMode -> blockParse  ckey mbs

blockParse :: [ParseClass] -> BS.ByteString IO () -> IO ()
blockParse !ckey = runParses (A.parse (seekInObj' ckey))

blockParse' :: [ParseClass] -> BS.ByteString IO () -> IO ()
blockParse' !ckey = runParsed (seekInObj' ckey)

lineParse :: [ParseClass] -> Maybe String -> Bool -> Bool -> IO ()
lineParse !ckey Nothing     = streamZepto (seekInObjZepto ckey)
lineParse !ckey (Just !url) = streamZeptoHttp (seekInObjZepto ckey) url

lineParse' :: [ParseClass] -> BS.ByteString IO () -> IO ()
lineParse' !ckey mbs =
  S.mapM_ B8.putStr $
    lineParseFold (seekInObjZepto ckey) concatLine mempty buildLong $ mbs

concatLine :: D.Builder -> D.Builder -> D.Builder
concatLine bld rest = bld <> D.word8 0xa <> rest
{-# INLINE concatLine #-}
