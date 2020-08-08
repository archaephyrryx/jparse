{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Driver where

import qualified Data.Attoparsec.ByteString as A

import Data.Semigroup ((<>))

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D

import qualified Conduit as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit ((.|))

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import Streaming (Of(..))
import qualified Streaming as S
import qualified Streaming.Prelude as S


import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Either (isRight, fromRight)
import Data.List (stripPrefix)

import JParse hiding (zeptoC)
import Parse

import qualified Parse.ReadAlt as Alt
import qualified Parse.ReadZepto as Zep
import qualified Parse.ReadStream as ZepS

import qualified Parse.Parser.Zepto as Z
import qualified Parse.Parser.ZeptoStream as ZS


data Mode = BlockMode | LineMode deriving (Eq)

getKeyMode :: [String] -> (ByteString, Mode)
getKeyMode xs = (qkey xs, getMode xs)
{-# INLINE getKeyMode #-}

getMode :: [String] -> Mode
getMode xs =
  case mapMaybe (stripPrefix "--mode=") xs of
    [] -> BlockMode
    x:_ -> if | x == "line" -> LineMode
              | x == "block" -> BlockMode
              | otherwise -> error $ "unrecognized mode \""++x++"\""
{-# INLINE getMode #-}




{-
zeptoC :: Monad m => String -> C.ConduitT ByteString ByteString m ()
zeptoC str = C.mapC (run' (keyToZepto str) $ build.fromJust)
-}

streamZepto :: MonadIO m => Z.Parser (Maybe Builder) -> m ()
streamZepto z = S.mapM_ putLnBuilder $ trimZepto $ streamZ z streamlines

trimZepto :: MonadIO m => S.Stream (Of (Either String (Maybe Builder))) m r -> S.Stream (Of (Maybe Builder)) m r
trimZepto = S.map (fromRight Nothing) . S.filter isRight

{- streamZ :: MonadIO m => Z.Parser (Maybe Builder)
        -> S.Stream (BS8.ByteString m) m r
        -> S.Stream (Of (Either String (Maybe Builder))) m r -}
streamZ z = S.mapped (streamParse z)

streamParse :: MonadIO m
            => Z.Parser (Maybe Builder)
            -> BS8.ByteString m r
            -> m (Of (Either String (Maybe Builder)) r)
streamParse z bs = do
  bs' <- BS.toStrict_ (void bs)
  let val = Z.parse z bs'
  res <- BS.effects bs
  return $ val :> res

streamlines :: C.MonadIO m => S.Stream (BS8.ByteString m) m ()
streamlines = BS8.lines BS.stdin

run :: A.Parser a -> (a -> b) -> ByteString -> b
run p f b = (\(A.Done _ x) -> f x) $ A.parse p b

run' :: Z.Parser a -> (a -> b) -> ByteString -> b
run' p f b = (\(Right x) -> f x) $ Z.parse p b

runZ :: ZS.Parser a -> (a -> b) -> ByteString -> b
runZ p f b = (\(Right x) -> f x) $ ZS.parse p b

bstring :: ByteString
bstring = "\"this is an exceptionally long string with escapes such as \\b, \\t, \\\\, and even \\u2f3F-like quads\""

bstring' :: ByteString
bstring' = "\"this is an exceptionally long string with a single quad and no other escapes. the quad is \\u2f3f and everything else is just plain text. really nothing too interesting.\""

fooson :: ByteString
fooson = "{ \"ignore\" : -123 , \"everything\":null, \"until\":[\"you\",\"see\"] , \"the\":{ \"key\":\"foo\" }, \"foo\":\"<- here it is!\" }"

countParse :: String -> String -> IO Int
countParse str inp =
  let parser = keyToParser str
   in C.runResourceT $ runParseWithC (C.lengthIfC isJust) (A.parse parser) (C.sourceFile inp)

countParse' :: String -> String -> IO Int
countParse' str inp =
  let parser = keyToParser' str
   in C.runResourceT $ runParseWithC (C.lengthIfC isJust) (A.parse parser) (C.sourceFile inp)

countZepto :: String -> String -> IO Int
countZepto str inp =
  let parser = keyToZepto str
   in C.runResourceT $ runZeptoWithC (C.lengthIfC isJust) (Z.parseR parser) (C.sourceFile inp)


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
    genObj' :: ByteString -> Int -> Int -> Builder
    genObj' bs 0 1 = "\""<>D.byteString bs<>"\":\"bar\\f\\udead\\ubeef\""
    genObj' bs _ 1 = "\"baz\":\"bar\\f\\udead\\ubeef\""
    genObj' bs i n = genObj' bs i 1 <> ", " <> genObj' bs (i-1) (n-1)




build :: Builder -> ByteString
build = L.toStrict . D.toLazyByteStringWith buildStrat L.empty
  where
    buildStrat = D.untrimmedStrategy 128 256
    {-# INLINE buildStrat #-}
{-# INLINE build #-}

buildM :: MonadIO m => Builder -> BS.ByteString m ()
buildM = BS.toStreamingByteStringWith buildStrat
  where
    buildStrat = D.untrimmedStrategy 128 256
    {-# INLINE buildStrat #-}
{-# INLINE buildM #-}



extract :: A.Result (Maybe Builder) -> Int
extract (A.Done _ (Just x)) = B.length $ build x
extract _ = 0

extract' :: Either String (Maybe Builder) -> Int
extract' (Right (Just x)) = B.length $ build x
extract' _ = 0

keyToParser str =
  let key = qkey [str]
      ckey = mapClass key
   in seekInObj ckey

keyToParser' str =
  let key = qkey [str]
      ckey = mapClass key
   in seekInObj' ckey

keyToZepto str =
  let key = qkey [str]
      ckey = mapClass key
   in seekInObjZepto ckey

keyToStream str =
  let key = qkey [str]
      ckey = mapClass key
   in seekInObjZeptoStream ckey
