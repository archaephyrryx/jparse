{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Driver where

import Prelude hiding (getLine)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString (ByteString)

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as D

import qualified Parse.Parser.Zepto as Z

import Helper

import Driver.Internal
import Driver.Distributor

-- Concurrency mode
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM_, unless)

-- * LineMode specialization


streamZepto :: Z.Parser (Maybe Builder) -> Bool -> Bool -> IO ()
streamZepto = zeptoMain

streamZeptoHttp :: Z.Parser (Maybe Builder) -> String -> Bool -> Bool -> IO ()
streamZeptoHttp = zeptoMainHttp

dist :: Bool -> ChanBounded L.ByteString -> Bool -> IO ()
dist True = distributorGated
dist False = distributor
{-# INLINE dist #-}

distHttp :: Bool -> ChanBounded L.ByteString -> String -> Bool -> IO ()
distHttp True = distributorHttpGated
distHttp False = distributorHttp
{-# INLINE distHttp #-}

zeptoMain :: Z.Parser (Maybe Builder) -> Bool -> Bool -> IO ()
zeptoMain z isZipped isGated = do
  ZEnv{..} <- newZEnv
  dist isGated input isZipped
  replicateM_ nworkers $ async $ worker input output nw z
  done <- async $ collector output
  monitor output nw
  wait done

zeptoMainHttp :: Z.Parser (Maybe Builder) -> String -> Bool -> Bool -> IO ()
zeptoMainHttp z url isZipped isGated = do
  ZEnv{..} <- newZEnv
  distHttp isGated input url isZipped
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
  let bld = refold getLine (accZepto z) (mempty :: Builder) lbs
      !bs = build bld
  writeChanBounded output (Just bs)

accZepto :: Z.Parser (Maybe Builder)
         -> L.ByteString
         -> Builder
         -> Builder
accZepto z bs bld =
  case Z.parse z (L.toStrict bs) of
    Right (Just x) -> x <> D.word8 0x0a <> bld
    _ -> bld