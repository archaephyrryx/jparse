{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Conduit as C (stdinC)
import qualified Data.Attoparsec.ByteString as A (parse)

import Parse (mapClass, ParseClass)
import JParse (seekInObj', seekInObjZepto, runParse)
import Driver (streamZepto, streamZeptoHttp)
import Options (getOptions, Mode(..), Options(..))

import Options.Applicative

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
    BlockMode -> blockParse ckey
    LineMode -> lineParse ckey http zipped gated

blockParse :: [ParseClass] -> IO ()
blockParse !ckey = do
  let parser = A.parse (seekInObj' ckey)
  runParse parser C.stdinC

lineParse :: [ParseClass] -> Maybe String -> Bool -> Bool -> IO ()
lineParse !ckey Nothing = streamZepto (seekInObjZepto ckey)
lineParse !ckey (Just !url) = streamZeptoHttp (seekInObjZepto ckey) url