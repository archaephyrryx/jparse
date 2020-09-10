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

import qualified Data.Attoparsec.ByteString as A

import Data.Semigroup ((<>))

import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D

import Data.Either (isLeft, fromLeft)

import qualified Conduit as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit ((.|))

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import Streaming
import qualified Streaming.Prelude as S
import Streaming.Internal (Stream(..))

import qualified Streaming.Zip as Zip

import JParse
import Parse

import qualified Parse.ReadAlt as Alt

import qualified Parse.ReadZepto as Zep
import qualified Parse.Parser.Zepto as Z

import qualified Parse.ReadStream as ZepS
import qualified Parse.Parser.ZeptoStream as ZS

import Vectorize
import Global
import Helper
import Bundle

import Driver.Internal
import Driver.Distributor


import Data.Vector (Vector)
import qualified Data.Vector as V

-- Concurrency mode
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (replicateM_, unless, when)
import System.IO (stdout)
import System.Environment



-- * LineMode specialization


streamZepto :: Z.Parser (Maybe Builder) -> Bool -> Bool -> IO ()
streamZepto = zeptoMain

streamZeptoHttp :: Z.Parser (Maybe Builder) -> String -> Bool -> Bool -> IO ()
streamZeptoHttp = zeptoMainHttp

dist :: Bool -> ChanBounded (Bundle L.ByteString) -> Bool -> IO ()
dist True = distributorGated
dist False = distributor
{-# INLINE dist #-}

distHttp :: Bool -> ChanBounded (Bundle L.ByteString) -> String -> Bool -> IO ()
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

worker :: ChanBounded (Bundle L.ByteString)
       -> ChanBounded (Maybe ByteString)
       -> TVar Int
       -> Z.Parser (Maybe Builder)
       -> IO ()
worker input output nw z = go
  where
    go = do
      bnd <- readChanBounded input
      if nullBundle bnd
         then do
           atomically $ modifyTVar nw pred
           writeChanBounded input bnd
         else labor output z bnd >> go

labor :: ChanBounded (Maybe ByteString)
      -> Z.Parser (Maybe Builder)
      -> Bundle L.ByteString
      -> IO ()
labor output z bnd = do
  let bld = refoldBundle getLine (accZepto z) (mempty :: Builder) bnd
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