{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Conduit as C (stdinC)
import qualified System.Environment as Sys
import qualified Data.Attoparsec.ByteString as A (parse)

import Parse (mapClass, ParseClass)
import JParse (seekInObj', seekInObjZepto, runParse, putLnBuilderC)
import Driver (getKeyMode, Mode(..), streamZepto, debugZepto)
import qualified Parse.Parser.ZeptoStream as ZS


main :: IO ()
main = do
  args <- Sys.getArgs
  let (key, mode) = getKeyMode args
      ckey = mapClass $! key
  case mode of
    BlockMode -> blockParse ckey
    LineMode -> lineParse ckey
    DebugMode -> debugParse ckey

blockParse :: [ParseClass] -> IO ()
blockParse !ckey = do
  let parser = A.parse (seekInObj' ckey)
  runParse parser C.stdinC

lineParse :: [ParseClass] -> IO ()
lineParse !ckey = do
  let parser = seekInObjZepto ckey
  streamZepto parser

debugParse :: [ParseClass] -> IO ()
debugParse !ckey = do
  let parser = seekInObjZepto ckey
  debugZepto parser
