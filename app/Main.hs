{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Conduit as C (stdinC)
import qualified System.Environment as Sys
import qualified Data.Attoparsec.ByteString as A (parse)

import Parse (mapClass, ParseClass)
import JParse (seekInObj', seekInObjZepto, runParse, putLnBuilderC)
import Driver (streamZepto, debugZepto)
import Options (getOptions, Mode(..), Options(..))

import Options.Applicative

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
    BlockMode -> blockParse ckey
    LineMode -> lineParse ckey vector zipped
    DebugMode -> debugParse ckey

blockParse :: [ParseClass] -> IO ()
blockParse !ckey = do
  let parser = A.parse (seekInObj' ckey)
  runParse parser C.stdinC

lineParse :: [ParseClass] -> Bool -> Bool -> IO ()
lineParse !ckey = streamZepto (seekInObjZepto ckey)

debugParse :: [ParseClass] -> IO ()
debugParse !ckey = do
  let parser = seekInObjZepto ckey
  debugZepto parser
