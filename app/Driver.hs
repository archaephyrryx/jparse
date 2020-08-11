{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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

import qualified Conduit as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit ((.|))

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import Streaming
import qualified Streaming.Prelude as S


import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Either (isRight, fromRight, isLeft, fromLeft)
import Data.List (stripPrefix)

import JParse
import Parse

import qualified Parse.ReadAlt as Alt

import qualified Parse.ReadZepto as Zep
import qualified Parse.Parser.Zepto as Z

import qualified Parse.ReadStream as ZepS
import qualified Parse.Parser.ZeptoStream as ZS

-- Concurrency mode
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (replicateM_, unless, when)
import System.IO (stdout)
import System.Environment


-- * Command-line specification of input processing mode

-- | Algebraic data type encapsulating the two primary modes of operation:
--
-- * @BlockMode@ for data that is either formatted with multiple objects per line, multiple lines per object, or arbitrarily long lines that are not safe to read into memory as a unit.
--
-- * @LineMode@ for (theoretically) more efficient parsing of data where every line contains exactly one complete JSON object, and lines are trusted to be sufficiently short to permit reading entire lines into memory as a unit.
--
-- * Additionally supports @DebugMode@ for human-readable printing of the raw return values of line-mode parser, rather than printing them in a machine-readable way after filtering and eliminating extraneous constructors around the actual result value.
data Mode = BlockMode -- ^ Attoparsec inside Conduit
          | LineMode -- ^ Zepto inside Streaming
          | DebugMode -- ^ Zepto inside Streaming (verbose)
          deriving (Eq)

getKeyMode :: [String] -> (ByteString, Mode)
getKeyMode xs = (qkey xs, getMode xs)
{-# INLINE getKeyMode #-}

getMode :: [String] -> Mode
getMode xs =
  case mapMaybe (stripPrefix "--mode=") xs of
    [] -> BlockMode
    x:_ -> if | x == "line" -> LineMode
              | x == "block" -> BlockMode
              | x == "debug" -> DebugMode
              | otherwise -> error $ "unrecognized mode \""++x++"\""
{-# INLINE getMode #-}

-- * LineMode specialization

-- | number of lines per individual sub-stream in line-mode
nLines :: Int
nLines = 1024
{-# INLINE nLines #-}

-- | number of worker threads to run Zepto parsing in parallel
nWorkers :: IO Int
nWorkers = getNumCapabilities
{-# INLINE nWorkers #-}

-- | Upper bound on number of unprocessed items in a channel
uBound :: Int
uBound = 1000
{-# INLINE uBound #-}

streamZepto :: Z.Parser (Maybe Builder) -> IO ()
streamZepto = zeptoMain

-- | set of common synchronization values for concurrent linemode
data ZEnv
   = ZEnv
     { nworkers :: Int -- ^ number of workers (runtime)
     , nw :: TVar Int -- ^ number of unterminated worker threads
     , done :: MVar () -- ^ signal for end-of-processing
     , input :: Chan [L.ByteString] -- ^ channel for unparsed input
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
  done <- newEmptyMVar
  input <- newChan
  output <- newChan
  return ZEnv{..}


zeptoMain :: Z.Parser (Maybe Builder) -> IO ()
zeptoMain z = do
  ZEnv{..} <- newZEnv
  forkIO $ distributor input inCap
  mapM_ (\_ -> forkIO $ worker input output inCap outCap nw z) [1..nworkers]
  forkIO $ collector output outCap done
  monitor output nw
  readMVar done

monitor :: Chan (Maybe a) -> TVar Int -> IO ()
monitor output nw = do
  atomically $ do
    n <- readTVar nw
    unless (n == 0) retry
  writeChan output Nothing

distributor :: Chan [L.ByteString] -> TVar Int -> IO ()
distributor input inCap = do
  S.mapM_ (doWrite input inCap) listStream
  writeChan input []

collector :: Chan (Maybe ByteString) -> TVar Int -> MVar () -> IO ()
collector output outCap done = go
  where
    go = do
      doRead output outCap >>= \case
        Just bs -> B.putStr bs >> go
        Nothing -> putMVar done ()


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

worker :: Chan [L.ByteString]
       -> Chan (Maybe ByteString)
       -> TVar Int
       -> TVar Int
       -> TVar Int
       -> Z.Parser (Maybe Builder)
       -> IO ()
worker input output inCap outCap nw z = go
  where
    go = do
      bss <- doRead input inCap
      if null bss
         then do
           atomically $ modifyTVar nw pred
           writeChan input []
         else labor output outCap z bss >> go

labor :: Chan (Maybe ByteString)
      -> TVar Int
      -> Z.Parser (Maybe Builder)
      -> [L.ByteString]
      -> IO ()
labor output outCap z lbs = do
  let bld = foldr (accZepto z) (mempty :: Builder) lbs
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


chunkStream :: MonadIO m => Stream (Stream (Of L.ByteString) m) m ()
chunkStream = chunksOf nLines $ mapped BS.toLazy streamlines

listStream :: Stream (Of [L.ByteString]) IO ()
listStream = mapped S.toList chunkStream

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

parseStream :: Z.Parser (Maybe Builder)
            -> L.ByteString
            -> Either String (Maybe Builder)
parseStream z = Z.parse z . L.toStrict

streamlines :: MonadIO m => S.Stream (BS8.ByteString m) m ()
streamlines = BS8.lines BS.stdin

build :: Builder -> ByteString
build = L.toStrict . D.toLazyByteStringWith buildStrat L.empty
  where
    buildStrat = D.untrimmedStrategy 2048 4096
    {-# INLINE buildStrat #-}
{-# INLINE build #-}
