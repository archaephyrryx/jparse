{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module JParse.Zepto where

import Streaming
import qualified Streaming.Prelude as S

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming as BS

import           Data.ByteString.Split (unconsLine)

import qualified Parse.Parser.Zepto as Z

import JParse.Helper
import JParse.Channels
import JParse.Zepto.Internal

import JParse.Streams (lazyLineSplit)

-- Concurrency mode
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM_, unless)

import qualified Data.Nullable as N


-- | Parses a monadic bytestring that is already pre-processed to raw JSON format
lineParseStream :: Z.Parser (Maybe a)
                -> BS.ByteString IO ()
                -> Stream (Of [a]) IO ()
lineParseStream parser mbs = parseLines parser $ lazyLineSplit mbs
{-# INLINE lineParseStream #-}

-- | Parses a monadic bytestring that is already pre-processed to raw JSON format
--   and processes output with specified fold-and-convert parameters
lineParseFold :: Z.Parser (Maybe a)
              -> (a -> x -> x) -> x -> (x -> b)
              -> BS.ByteString IO ()
              -> Stream (Of b) IO ()
lineParseFold parser f z g mbs = parseLinesFold parser f z g $ lazyLineSplit mbs
{-# INLINE lineParseFold #-}

parseLines :: Z.Parser (Maybe a)
           -> Stream (Of L.ByteString) IO ()
           -> Stream (Of [a]) IO ()
parseLines z str = do
  ZEnv{..} <- liftIO (newZEnv :: IO (ZEnv [] a))
  liftIO $ do
    async $ feedChanBounded input str
    replicateM_ nworkers $ async $ worker input output nw z
    async $ monitor output nw
  drainChanBounded output
{-# INLINE parseLines #-}

parseLinesFold :: Z.Parser (Maybe a)
               -> (a -> x -> x) -> x -> (x -> b)
               -> Stream (Of L.ByteString) IO ()
               -> Stream (Of b) IO ()
parseLinesFold parser f z g str = do
  ZEnv{..} <- liftIO (newZEnv :: IO (ZEnv Maybe b))
  liftIO $ do
    async $ feedChanBounded input str
    replicateM_ nworkers $ async $ workerFold input output nw parser f z g
    async $ monitor output nw
  drainChanBoundedMaybe output
{-# INLINE parseLinesFold #-}

worker :: ChanBounded L.ByteString
       -> ChanBounded [a]
       -> TVar Int
       -> Z.Parser (Maybe a)
       -> IO ()
worker input output nw z = go
  where
    go = do
      lbs <- readChanBounded input
      if L.null lbs
         then do
           atomically $ modifyTVar nw pred
           writeChanBounded input lbs
         else labor output z lbs >> go
{-# INLINE worker #-}

workerFold :: ChanBounded L.ByteString
           -> ChanBounded (Maybe b)
           -> TVar Int
           -> Z.Parser (Maybe a)
           -> (a -> x -> x) -> x -> (x -> b)
           -> IO ()
workerFold input output nw parser f z g = go
  where
    go = do
      lbs <- readChanBounded input
      if L.null lbs
         then do
           atomically $ modifyTVar nw pred
           writeChanBounded input lbs
         else laborFold output parser f z g lbs >> go
{-# INLINE workerFold #-}

labor :: ChanBounded ([a])
      -> Z.Parser (Maybe a)
      -> L.ByteString
      -> IO ()
labor output z lbs = do
  let xs = refold unconsLine (accZepto z) [] lbs
  unless (null xs) $
    writeChanBounded output xs
{-# INLINE labor #-}

laborFold :: ChanBounded (Maybe b)
          -> Z.Parser (Maybe a)
          -> (a -> x -> x) -> x -> (x -> b)
          -> L.ByteString
          -> IO ()
laborFold output parser f z g lbs = do
  let xs = refold unconsLine (accZeptoFold parser f) z lbs
  let !ys = Just $! g xs
  writeChanBounded output ys
{-# INLINE laborFold #-}

accZepto :: Z.Parser (Maybe a) -> L.ByteString -> [a] -> [a]
accZepto z bs acc =
  case Z.parse z (L.toStrict bs) of
    Right (Just x) -> x : acc
    _ -> acc
{-# INLINE accZepto #-}

accZeptoFold :: Z.Parser (Maybe a) -> (a -> x -> x) -> L.ByteString -> x -> x
accZeptoFold parser f bs acc =
  case Z.parse parser (L.toStrict bs) of
    Right (Just x) -> f x acc
    _ -> acc
{-# INLINE accZeptoFold #-}

monitor :: N.Nullable w => ChanBounded w -> TVar Int -> IO ()
monitor output nw = do
  atomically $ do
    n <- readTVar nw
    unless (n == 0) retry
  writeChanBounded output N.null
{-# INLINE monitor #-}
