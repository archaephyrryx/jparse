module Main (main) where

import qualified Conduit as C (stdinC)
import qualified Data.Attoparsec.ByteString as A (parse)
import qualified System.Environment as Sys

import Parse (mapClass)
import JParse (qkey, seekInObj, runParse, putLnBuilderC)

main :: IO ()
main = do
   key <- qkey <$> Sys.getArgs
   let ckey = mapClass $! key
       parser = A.parse (seekInObj ckey)
   runParse parser C.stdinC
