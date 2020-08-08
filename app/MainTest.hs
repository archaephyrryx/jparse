module MainTest (main) where

import qualified Conduit as C (stdinC)
import qualified System.Environment as Sys

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Q (putStrLn)

import Parse (mapClass)
-- import JParse (qkey, seekInObjZepto, {- runZepto, -} putLnBuilderC)
import qualified Parse.Parser.Zepto as Z

import Driver
import Data.Maybe (fromJust)

headDef :: a -> [a] -> a
headDef z [] = z
headDef _ (x:_) = x
{-# INLINE headDef #-}

main :: IO ()
main = do
  key <- headDef "name" <$> Sys.getArgs
  iter 1000 key

iter :: Int -> String -> IO ()
iter 0 _ = return ()
iter n key = do
  Q.putStrLn $ runBase key fooson
  Q.putStrLn $ runAlt key fooson
  Q.putStrLn $ runZepto key fooson
  Q.putStrLn $ runZeptoS key fooson
  iter (n-1) key
{-# NOINLINE iter #-}

runBase, runAlt, runZepto, runZeptoS :: String -> ByteString -> ByteString
runBase   s = run  (keyToParser  s) (build.fromJust)
runAlt    s = run  (keyToParser' s) (build.fromJust)
runZepto  s = run' (keyToZepto   s) (build.fromJust)
runZeptoS s = runZ (keyToStream  s) (build.fromJust)
