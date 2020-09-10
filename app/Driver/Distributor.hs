module Driver.Distributor where

import Control.Concurrent.Async

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)

import Data.Vector (Vector)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import Streaming
import Streaming.Internal (Stream(..))
import qualified Streaming.Prelude as S

import qualified Streaming.Zip as Zip

import Driver.Internal
import Vectorize
import Global

import Streams as Refactor

import Helper
import Bundle

-- * Ungated version

distributor :: ChanBounded (Bundle L.ByteString) -> Bool -> IO ()
distributor input isZipped = async go >> pure ()
  where
    src = S.map VectorOf $ Refactor.vecStreamSplitOf isZipped
    {-# INLINE src #-}

    src' :: Stream (Of (Bundle L.ByteString)) IO ()
    src' = S.map SingleOf $ lazyLineSplit $ condUnzip getStdin isZipped
    {-# INLINE src' #-}

    go = feedChanBounded input src'
    {-# INLINE go #-}
{-# INLINE distributor #-}

distributorHttp :: ChanBounded (Bundle L.ByteString) -> String -> Bool -> IO ()
distributorHttp input url isZipped = async go >>= link
  where
    src = S.map VectorOf $ Refactor.vecStreamSplitOfHttp url isZipped
    {-# INLINE src #-}

    src' :: MonadResource m => Stream (Of (Bundle L.ByteString)) m ()
    src' = S.map SingleOf $ lazyLineSplit $ condUnzip (getHttp url) isZipped
    {-# INLINE src' #-}

    go = runResourceT $ feedChanBounded input src
    {-# INLINE go #-}
{-# INLINE distributorHttp #-}

-- * Gated Version

distributorGated :: ChanBounded (Bundle L.ByteString) -> Bool -> IO ()
distributorGated input True = do
  zvGate <- newChanBounded uBound_gate
  async $ stdinGZSource zvGate
  async $ gatedVect zvGate input
  return ()
distributorGated input False = do
  async $ stdinRawSource input
  return ()
{-# INLINE distributorGated #-}

distributorHttpGated :: ChanBounded (Bundle L.ByteString) -> String -> Bool -> IO ()
distributorHttpGated input url True = do
  hzGate <- newChanBounded uBound_gate
  zvGate <- newChanBounded uBound_gate
  link =<< async (httpSource hzGate url)
  async $ gatedGZ hzGate zvGate
  async $ gatedVect zvGate input
  return ()
distributorHttpGated input url False = do
  hvGate <- newChanBounded uBound_gate
  link =<< async (httpSource hvGate url)
  async $ gatedVect hvGate input
  return ()
{-# INLINE distributorHttpGated #-}

httpSource :: ChanBounded B.ByteString -> String -> IO ()
httpSource hzGate url = runResourceT $ writeBS hzGate $ Refactor.getHttp url
{-# INLINE httpSource #-}

stdinGZSource :: ChanBounded B.ByteString -> IO ()
stdinGZSource zvGate = writeBS zvGate $ Zip.gunzip Refactor.getStdin
{-# INLINE stdinGZSource #-}

stdinRawSource :: ChanBounded (Bundle L.ByteString) -> IO ()
stdinRawSource input = feedChanBounded input
                     $ S.map VectorOf
                     $ Refactor.vecStreamSplit
{-# INLINE stdinRawSource #-}

gatedGZ :: ChanBounded B.ByteString -> ChanBounded B.ByteString -> IO ()
gatedGZ hzGate zvGate = do
  let mbs = readBS hzGate
  writeBS zvGate $ Zip.gunzip mbs
{-# INLINE gatedGZ #-}

gatedVect :: ChanBounded B.ByteString -> ChanBounded (Bundle L.ByteString) -> IO ()
gatedVect zvGate input = do
  let mbs = readBS zvGate
  feedChanBounded input $ S.map VectorOf $ Refactor.vectorLineSplit mbs
{-# INLINE gatedVect #-}

gatedSingle :: ChanBounded B.ByteString -> ChanBounded (Bundle L.ByteString) -> IO ()
gatedSingle zvGate input = do
  let mbs = readBS zvGate
  feedChanBounded input $ S.map SingleOf $ Refactor.lazyLineSplit mbs
{-# INLINE gatedSingle #-}
