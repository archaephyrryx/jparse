{-# LANGUAGE OverloadedStrings #-}

module Benchmark where

import Criterion.Main

import qualified Data.Attoparsec.ByteString as A

import Data.Semigroup ((<>))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)

import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D

import qualified Conduit as C
import qualified Data.Conduit as C
import Data.Conduit ((.|))

import Data.Maybe (isJust)

import JParse
import Parse

import qualified Parse.ReadAlt as Alt
import qualified Parse.ReadZepto as Zep

import qualified Parse.Parser.Zepto as Z


main :: IO ()
main = do
--  _hrun >> _lrun >> _mrun
  defaultMain $
    [ {- bgroup "parseObj" $
      [ bench "first"   $ nfIO $ countParse "foo" "heads.txt"
      , bench "last"    $ nfIO $ countParse "foo" "lasts.txt"
      , bench "middle"  $ nfIO $ countParse "foo" "mids.txt"
      ]
    , bgroup "parseObjAlt" $
      [ bench "first"   $ nfIO $ countParse' "foo" "heads.txt"
      , bench "last"    $ nfIO $ countParse' "foo" "lasts.txt"
      , bench "middle"  $ nfIO $ countParse' "foo" "mids.txt"
      ] -}
      bgroup "skipToEndQ escapes" $
        [ bench "Read"    $ whnf (run skipToEndQ id) bstring
        , bench "ReadAlt" $ whnf (run Alt.skipToEndQ id) bstring
        , bench "ReadZepto" $ whnf (run' Zep.skipToEndQ id) bstring
        ]
    , bgroup "parseToEndQ escapes" $
        [ bench "Read"    $ whnf (run parseToEndQ build)     bstring
        , bench "ReadAlt" $ whnf (run Alt.parseToEndQ build) bstring
        , bench "ReadZepto" $ whnf (run' Zep.parseToEndQ build) bstring
        ]
    , bgroup "skipToEndQ simple" $
        [ bench "Read"    $ whnf (run skipToEndQ id) bstring'
        , bench "ReadAlt" $ whnf (run Alt.skipToEndQ id) bstring'
        , bench "ReadZepto" $ whnf (run' Zep.skipToEndQ id) bstring'
        ]
    , bgroup "parseToEndQ simple" $
        [ bench "Read"    $ whnf (run parseToEndQ build)     bstring'
        , bench "ReadAlt" $ whnf (run Alt.parseToEndQ build) bstring'
        , bench "ReadZepto" $ whnf (run' Zep.parseToEndQ build) bstring'
        ]
    ]



run :: A.Parser a -> (a -> b) -> ByteString -> b
run p f b = (\(A.Done _ x) -> f x) $ A.parse p b

run' :: Z.Parser a -> (a -> b) -> ByteString -> b
run' p f b = (\(Right x) -> f x) $ Z.parse p b

bstring :: ByteString
bstring = "\"this is an exceptionally long string with escapes such as \\b, \\t, \\\\, and even \\u2f3F-like quads\""

bstring' :: ByteString
bstring' = "\"this is an exceptionally long string with a single quad and no other escapes. the quad is \\u2f3f and everything else is just plain text. really nothing too interesting.\""

countParse :: String -> String -> IO Int
countParse str inp =
  let parser = keyToParser str
   in C.runResourceT $ runParseWithC (C.lengthIfC isJust) parser (C.sourceFile inp)

countParse' :: String -> String -> IO Int
countParse' str inp =
  let parser = keyToParser' str
   in C.runResourceT $ runParseWithC (C.lengthIfC isJust) parser (C.sourceFile inp)

_count = 100000
_len = 10

_hrun, _lrun, _mrun :: IO ()
_hrun = C.runResourceT $ C.runConduit $ _hsource .| C.takeC _count .| C.unlinesAsciiC .| C.sinkFile "heads.txt"
_lrun = C.runResourceT $ C.runConduit $ _lsource .| C.takeC _count .| C.unlinesAsciiC .| C.sinkFile "lasts.txt"
_mrun = C.runResourceT $ C.runConduit $ _msource .| C.takeC _count .| C.unlinesAsciiC .| C.sinkFile "mids.txt"


_hsource = C.repeatC $! _hed _len
_lsource = C.repeatC $! _lst _len
_msource = C.repeatC $! _mid _len



_hed = (\n -> genObj "foo" 0 n)
_lst = (\n -> genObj "foo" (n-1) n)
_mid = (\n -> genObj "foo" (n`div`2) n)


-- takes a key value, position of key, and number of values and generates generic simple JSON object
genObj :: ByteString -> Int -> Int -> ByteString
genObj bs i n = build $  "{ " <> genObj' bs i n <> " }"
  where
    genObj' :: ByteString -> Int -> Int -> D.Builder
    genObj' bs 0 1 = "\""<>D.byteString bs<>"\":\"bar\\f\\udead\\ubeef\""
    genObj' bs _ 1 = "\"baz\":\"bar\\f\\udead\\ubeef\""
    genObj' bs i n = genObj' bs i 1 <> ", " <> genObj' bs (i-1) (n-1)




build :: D.Builder -> ByteString
build = L.toStrict . D.toLazyByteStringWith buildStrat L.empty
  where
    buildStrat = D.untrimmedStrategy 128 256
    {-# INLINE buildStrat #-}
{-# INLINE build #-}




extract :: A.Result (Maybe D.Builder) -> Int
extract (A.Done _ (Just x)) = B.length $ build x
extract _ = 0

keyToParser str =
  let key = qkey [str]
      ckey = mapClass key
   in A.parse (seekInObj ckey)

keyToParser' str =
  let key = qkey [str]
      ckey = mapClass key
   in A.parse (seekInObj' ckey)