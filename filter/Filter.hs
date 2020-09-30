{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Streaming.Prelude as SP

import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Build
import Data.ByteString.Streaming.Gates
import Data.Word (Word8)

import Toascii (toAlabels)
import FilterOptions (getOpts, FilterOptions(..), Mode(..))

import JParse
import JParse.Driver.Internal (toStricts)
import JParse.Helper (if_)
import JParse.Zepto
import Parse (mapClass)

type TldMap    = HS.HashSet B.ByteString
type SuffixMap = HM.HashMap B.ByteString Int

loadTlds :: FilePath -> IO TldMap
loadTlds fname = runResourceT
    $ SP.fold_ buildMap HS.empty id
    $ toStricts
    $ BS8.lines
    $ BS.readFile fname
  where
    buildMap :: TldMap -> B.ByteString -> TldMap
    buildMap !tm !b
      | B.length b < 2 = tm
      | otherwise      = HS.insert b tm

{-
augmentSuffixes :: FilePath -> SuffixMap -> IO SuffixMap
augmentSuffixes fname smap = runResourceT
    $ SP.fold_ augmentMap smap id
    $ toStricts
    $ BS8.lines
    $ BS.readFile fname
  where
    augmentMap :: SuffixMap -> B.ByteString -> SuffixMap
    augmentMap sm !bs =
      if | B.length bs > 0
         , B.take 2 bs /= "//"
         , Right t <- T.decodeUtf8' bs
         -> addSuffix t sm
         | otherwise -> sm

    addSuffix :: T.Text -> SuffixMap -> SuffixMap
    addSuffix t sm
      | T.head t == '-' = addNode (toAlabels $! T.tail t) sm
      | otherwise = sm

    addNode :: Maybe [B.ByteString] -> SuffixMap -> SuffixMap
    addNode Nothing   sm = sm
    addNode (Just ls) sm = ins ls sm
      where
        ins ls m = HM.insert (ldomain ls) -1 m

        ldomain :: [B.ByteString] -> B.ByteString
        ldomain = B.map wmap . B8.unwords
        {-# INLINE ldomain #-}

        wmap :: Word8 -> Word8
        wmap 0x20 = 0x2e
        wmap 0x2e = 0x20
        wmap w = w
        {-# INLINE wmap #-}
-}

loadSuffixes :: FilePath -> IO SuffixMap
loadSuffixes fname = runResourceT
    $ SP.fold_ buildMap HM.empty id
    $ toStricts
    $ BS8.lines
    $ BS.readFile fname
  where
    buildMap :: SuffixMap -> B.ByteString -> SuffixMap
    buildMap !sm !bs =
      if | B.length bs > 0
         , B.take 2 bs /= "//"
         , Right !t <- T.decodeUtf8' bs
         -> addSuffix t sm
         | otherwise -> sm

    addSuffix :: T.Text -> SuffixMap -> SuffixMap
    addSuffix !t !sm =
        let !e = T.head t == '!'
            !k = if_ e (T.tail t) t
         in addNode e (toAlabels k) sm

    addNode :: Bool -> Maybe [B.ByteString] -> SuffixMap -> SuffixMap
    addNode _  Nothing  !sm = sm
    addNode !e (Just !ls) !sm = ins
      where
        ins | e
            = HM.insert (ldomain ls) 0 sm
            | head ls == "*"
            = HM.insert (ldomain $ tail ls) 2 sm
            | otherwise
            = HM.insert (ldomain ls) 1 sm

        ldomain :: [B.ByteString] -> B.ByteString
        ldomain = \ !d -> B.map wmap $! B8.unwords d
        {-# INLINE ldomain #-}

        wmap :: Word8 -> Word8
        wmap 0x20 = 0x2e
        wmap 0x2e = 0x20
        wmap w = w
        {-# INLINE wmap #-}



trimStream :: SuffixMap -> TldMap -> (B.ByteString -> ((B.ByteString -> x -> x) -> (x -> x)))
trimStream !sufmap !tldmap = \ !domain -> (\f -> maybe id f $! wanted $! undot domain)
  where
    undot :: B.ByteString -> B.ByteString
    undot b | B.null b = b
            | otherwise = case BU.unsafeLast b of { 0x2e -> BU.unsafeInit b; _ -> b }
    {-# INLINE undot #-}

    wanted name
      | (_, !tld) <- B.breakEnd (== 0x2e) name
      , HS.member tld tldmap = trim Nothing Nothing name
      | otherwise = Nothing

    trim !_  !_  "" = Nothing
    trim !l1 !l2 !bs = case HM.lookup bs sufmap of
      Nothing -> let t = B.dropWhile (/= 0x2e) bs
                  in if | B.null t  -> Nothing
                        | otherwise -> trim l2 (Just bs) $ B.tail t
      Just  n -> case n of
           0 -> Just bs
           1 -> l2
           2 -> l1
           _ -> Nothing

main :: IO ()
main = do
  FilterOptions{..} <- getOpts
  sufmap <- loadSuffixes sfile
  tldmap <- loadTlds tfile
  let !ckey = mapClass $! query
      parserZ = seekInObjZepto ckey
      parserA = seekInObj' ckey
      fmerge = concatLine sufmap tldmap
  mbs <- generate gated zipped http
  case mode of
    LineMode  -> SP.mapM_ B8.putStr $ lineParseFold parserZ fmerge mempty buildLong mbs
    BlockMode -> B8.putStr =<< mapParses parserA fmerge mempty buildLong mbs

concatLine :: SuffixMap -> TldMap -> D.Builder -> D.Builder -> D.Builder
concatLine sm tm bld acc =
  let !bs = buildShort bld
   in (trimStream sm tm bs $ (\ !b rest -> D.byteString b <> D.word8 0xa <> rest)) acc
