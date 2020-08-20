{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Driver where

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

import Final
import Streams
import Global


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

-- | number of worker threads to run Zepto parsing in parallel
nWorkers :: IO Int
nWorkers = getNumCapabilities
{-# INLINE nWorkers #-}

-- | Upper bound on number of unprocessed items in a channel
uBound :: Int
uBound = 1000
{-# INLINE uBound #-}

streamZepto :: Z.Parser (Maybe Builder) -> Bool -> Bool -> IO ()
streamZepto = zeptoMain

streamZeptoHttp :: Z.Parser (Maybe Builder) -> String -> Bool -> Bool -> IO ()
streamZeptoHttp = zeptoMainHttp

data Bundle a where
  ListOf   :: [a] -> Bundle a
  VectorOf :: Vector a -> Bundle a
  Empty    :: Bundle a

foldrBundle :: (a -> b -> b) -> b -> Bundle a -> b
foldrBundle f z (ListOf xs) = foldr f z xs
foldrBundle f z (VectorOf v) = V.foldr f z v
foldrBundle _ z _ = z
{-# INLINE foldrBundle #-}

nullBundle :: Bundle a -> Bool
nullBundle (ListOf xs) = null xs
nullBundle (VectorOf v) = V.null v
nullBundle _ = True
{-# INLINE nullBundle #-}

-- | set of common synchronization values for concurrent linemode
data ZEnv
   = ZEnv
     { nworkers :: Int -- ^ number of workers (runtime)
     , nw :: TVar Int -- ^ number of unterminated worker threads
     , input :: Chan (Bundle L.ByteString) -- ^ channel for unparsed input
     , inCap :: TVar Int -- ^ counter for items in input channel
     , output :: Chan (Maybe ByteString) -- ^ channel for parsed output
     , outCap :: TVar Int -- ^ counter for items in output channel
     }

newZEnv :: IO ZEnv
newZEnv = do
  nworkers <- nWorkers
  nw <- newTVarIO nworkers
  inCap <- newTVarIO 0
  outCap <- newTVarIO 0
  input <- newChan
  output <- newChan
  return ZEnv{..}


zeptoMain :: Z.Parser (Maybe Builder) -> Bool -> Bool -> IO ()
zeptoMain z vec isZipped = do
  ZEnv{..} <- newZEnv
  async $ distributor input inCap vec isZipped
  replicateM_ nworkers $ async $ worker input output inCap outCap nw z
  async (collector output outCap) >>= \done -> monitor output nw >> wait done

zeptoMainHttp :: Z.Parser (Maybe Builder) -> String -> Bool -> Bool -> IO ()
zeptoMainHttp z url vec isZipped = do
  ZEnv{..} <- newZEnv
  dist <- async $ distributorHttp input inCap url vec isZipped
  link dist
  replicateM_ nworkers $ async $ worker input output inCap outCap nw z
  done <- async $ collector output outCap -- done
  monitor output nw
  wait done


monitor :: Chan (Maybe a) -> TVar Int -> IO ()
monitor output nw = do
  atomically $ do
    n <- readTVar nw
    unless (n == 0) retry
  writeChan output Nothing

distributor :: Chan (Bundle L.ByteString) -> TVar Int -> Bool -> Bool -> IO ()
distributor input inCap vec isZipped = do
  S.mapM_ (doWrite input inCap) $
    if vec
       then S.map VectorOf $ vecStreamOf  (InFormat isZipped)
       else S.map ListOf   $ listStreamOf (InFormat isZipped)
  writeChan input Empty

distributorHttp :: Chan (Bundle L.ByteString) -> TVar Int -> String -> Bool -> Bool -> IO ()
distributorHttp input inCap url vec isZipped = do
  C.runResourceT $ S.mapM_ (liftIO . doWrite input inCap) $
    if vec
       then S.map VectorOf $ vecStreamOfHttp  url (InFormat isZipped)
       else S.map ListOf   $ listStreamOfHttp url (InFormat isZipped)
  writeChan input Empty

collector :: Chan (Maybe ByteString) -> TVar Int -> IO ()
collector output outCap = go
  where
    go = doRead output outCap >>= \case
           Just bs -> B.putStr bs >> go
           Nothing -> return ()

-- Roundabout BoundedChannel

doWrite :: Chan a -> TVar Int -> a -> IO ()
doWrite chan capt val = do
  atomically $ do
    level <- readTVar capt
    when (level >= uBound) retry
  atomically $ modifyTVar capt succ
  writeChan chan val

doRead :: Chan a -> TVar Int -> IO a
doRead chan capt = do
  atomically $ modifyTVar capt pred
  readChan chan

worker :: Chan (Bundle L.ByteString)
       -> Chan (Maybe ByteString)
       -> TVar Int
       -> TVar Int
       -> TVar Int
       -> Z.Parser (Maybe Builder)
       -> IO ()
worker input output inCap outCap nw z = go
  where
    go = do
      bnd <- doRead input inCap
      if nullBundle bnd
         then do
           atomically $ modifyTVar nw pred
           writeChan input bnd
         else labor output outCap z bnd >> go

labor :: Chan (Maybe ByteString)
      -> TVar Int
      -> Z.Parser (Maybe Builder)
      -> Bundle L.ByteString
      -> IO ()
labor output outCap z bnd = do
  let bld = foldrBundle (accZepto z) (mempty :: Builder) bnd
      !bs = build bld
  doWrite output outCap (Just bs)

accZepto :: Z.Parser (Maybe Builder)
         -> L.ByteString
         -> Builder
         -> Builder
accZepto z bs bld =
  case Z.parse z (L.toStrict bs) of
    Right (Just x) -> x <> D.word8 0x0a <> bld
    _ -> bld

debugZepto :: MonadIO m => Z.Parser (Maybe Builder) -> m ()
debugZepto z = S.mapM_ (liftIO . Prelude.putStrLn) $ debugPrint $ streamZ z streamlines
-- debugZepto z = linestream $ streamlines

debugPrint :: MonadIO m
           => S.Stream (Of (Either String (Maybe Builder))) m r
           -> S.Stream (Of String) m r
debugPrint = S.map (either ("Left>"++) buildString)

buildString Nothing = "Right>Nothing"
buildString (Just d) = "Right>Just>" ++ S8.unpack (build d)

purgeZepto :: MonadIO m => S.Stream (Of (Either String (Maybe Builder))) m r -> S.Stream (Of String) m r
purgeZepto = S.map (fromLeft "weird...") . S.filter isLeft

streamZ :: MonadIO m => Z.Parser (Maybe Builder)
        -> S.Stream (BS8.ByteString m) m r
        -> S.Stream (Of (Either String (Maybe Builder))) m r
streamZ z = S.map (parseStream z) . mapped BS.toLazy
{-# INLINE streamZ #-}

parseStream :: Z.Parser (Maybe Builder)
            -> L.ByteString
            -> Either String (Maybe Builder)
parseStream z = Z.parse z . L.toStrict
{-# INLINE parseStream #-}

build :: Builder -> ByteString
build = L.toStrict . D.toLazyByteStringWith buildStrat L.empty
  where
    buildStrat = D.untrimmedStrategy 2048 4096
    {-# INLINE buildStrat #-}
{-# INLINE build #-}
