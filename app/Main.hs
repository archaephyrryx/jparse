module Main (main) where

import qualified Conduit as C (stdinC)
import qualified System.Environment as Sys

import Parse (mapClass)
import JParse (qkey, seekInObjZeptoStream, runZepto, putLnBuilderC)
import qualified Parse.Parser.ZeptoStream as ZS

main :: IO ()
main = do
   key <- qkey <$> Sys.getArgs
   let ckey = mapClass $! key
       parser = ZS.parseR (seekInObjZeptoStream ckey)
   runZepto parser C.stdinC
