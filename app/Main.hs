{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Conduit as C (stdinC)
import qualified Streaming.Prelude as S

import qualified Data.ByteString.Char8 as B8 (putStrLn)
import qualified Data.ByteString as B
import qualified Data.ByteString.Streaming.Char8 as BS8 (stdin)
import qualified Data.Attoparsec.ByteString as A (parse)
import qualified Data.ByteString.Builder as D
import System.IO (stdout)

import Parse (mapClass, ParseClass)
import JParse (seekInObj', seekInObjZepto, runParsec, runParses)
import JParse.Attoparsec (putLnBuilderS)
import JParse.Driver (streamZepto, streamZeptoHttp)
import JParse.Zepto (lineParseStream, lineParseFold )
import Options (getOptions, Mode(..), Options(..))

import Options.Applicative
import Sources (getHttp, getStdin, condUnzip, unzip)
import Gates (produce, generate)

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
  case mode of
    BlockMode -> blockParse' ckey
    LineMode -> lineParse' ckey http zipped gated

blockParse :: [ParseClass] -> IO ()
blockParse !ckey = do
  let parser = A.parse (seekInObj' ckey)
  runParsec parser C.stdinC

blockParse' :: [ParseClass] -> IO ()
blockParse' !ckey = do
  let parser = A.parse (seekInObj' ckey)
  runParses parser BS8.stdin

lineParse :: [ParseClass] -> Maybe String -> Bool -> Bool -> IO ()
lineParse !ckey Nothing     = streamZepto (seekInObjZepto ckey)
lineParse !ckey (Just !url) = streamZeptoHttp (seekInObjZepto ckey) url


lineParse' :: [ParseClass] -> Maybe String -> Bool -> Bool -> IO ()
lineParse' !ckey mUrl isZipped isGated = strat1
  where
    mbs = produce $ generate isGated isZipped mUrl
    strat1 =
      let str = lineParseStream (fmap buildShort <$> seekInObjZepto ckey) mbs
       in S.mapM_ putStrLns str
    strat2 =
      let str = lineParseFold (seekInObjZepto ckey) concatLine mempty id mbs
       in S.mapM_ (D.hPutBuilder stdout) str

concatLine :: D.Builder -> D.Builder -> D.Builder
concatLine bld rest = bld <> D.word8 0xa <> rest
{-# INLINE concatLine #-}

putStrLns :: [B.ByteString] -> IO ()
putStrLns bs = D.hPutBuilder stdout $ mconcat $ map (\b -> D.byteString b <> D.word8 0xa) bs
-- mapM_ B8.putStrLn
