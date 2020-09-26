{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module JParse.Driver where

import qualified Data.ByteString.Streaming as BS

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as D

import Data.ByteString.Split (unconsLine)

import qualified Parse.Parser.Zepto as Z

import JParse.Helper
import JParse.Channels
import JParse.Streams (lazyLineSplit)

import JParse.Driver.Internal

-- Concurrency mode
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM_, unless)


import Data.ByteString.Build (buildLong)
-- * LineMode specialization

streamZepto :: Z.Parser (Maybe Builder) -> BS.ByteString IO () -> IO ()
streamZepto z mbs = do
  ZEnv{..} <- newZEnv
  async $ feedChanBounded input $ lazyLineSplit mbs
  replicateM_ nworkers $ async $ worker input output nw z
  done <- async $ collector output
  monitor output nw
  wait done

monitor :: ChanBounded (Maybe a) -> TVar Int -> IO ()
monitor output nw = do
  atomically $ do
    n <- readTVar nw
    unless (n == 0) retry
  writeChanBounded output Nothing

collector :: ChanBounded (Maybe ByteString) -> IO ()
collector output = go
  where
    go = readChanBounded output >>= \case
           Just bs -> B.putStr bs >> go
           Nothing -> return ()

worker :: ChanBounded L.ByteString
       -> ChanBounded (Maybe ByteString)
       -> TVar Int
       -> Z.Parser (Maybe Builder)
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

labor :: ChanBounded (Maybe ByteString)
      -> Z.Parser (Maybe Builder)
      -> L.ByteString
      -> IO ()
labor output z lbs = do
  let bld = refold unconsLine (accZepto z) (mempty :: Builder) lbs
      !bs = buildLong bld
  writeChanBounded output (Just bs)

accZepto :: Z.Parser (Maybe Builder)
         -> L.ByteString
         -> Builder
         -> Builder
accZepto z bs bld =
  case Z.parse z (L.toStrict bs) of
    Right (Just x) -> x <> D.word8 0x0a <> bld
    _ -> bld
