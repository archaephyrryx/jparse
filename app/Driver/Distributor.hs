module Driver.Distributor where

import Control.Concurrent.Async

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import Streaming
import Streaming.Internal (Stream(..))
import qualified Streaming.Prelude as S

import qualified Streaming.Zip as Zip

import Driver.Internal
import Global

import Streams

import Helper

-- * Ungated version

distributor :: ChanBounded L.ByteString -> Bool -> IO ()
distributor input isZipped = async go >> pure ()
  where
    src = lbsStreamSplitOf isZipped
    {-# INLINE src #-}

    go = feedChanBounded input src
    {-# INLINE go #-}
{-# INLINE distributor #-}

distributorHttp :: ChanBounded L.ByteString -> String -> Bool -> IO ()
distributorHttp input url isZipped = async go >>= link
  where
    src = lbsStreamSplitOfHttp url isZipped
    {-# INLINE src #-}

    go = runResourceT $ feedChanBounded input src
    {-# INLINE go #-}
{-# INLINE distributorHttp #-}

-- * Gated Version

distributorGated :: ChanBounded L.ByteString -> Bool -> IO ()
distributorGated input True = do
  zlGate <- newChanBounded uBound_gate
  async $ stdinGZSource zlGate
  async $ gatedLazy zlGate input
  return ()
distributorGated input False = do
  async $ stdinRawSource input
  return ()
{-# INLINE distributorGated #-}

distributorHttpGated :: ChanBounded L.ByteString -> String -> Bool -> IO ()
distributorHttpGated input url True = do
  hzGate <- newChanBounded uBound_gate
  zlGate <- newChanBounded uBound_gate
  link =<< async (httpSource hzGate url)
  async $ gatedGZ hzGate zlGate
  async $ gatedLazy zlGate input
  return ()
distributorHttpGated input url False = do
  hlGate <- newChanBounded uBound_gate
  link =<< async (httpSource hlGate url)
  async $ gatedLazy hlGate input
  return ()
{-# INLINE distributorHttpGated #-}

httpSource :: ChanBounded B.ByteString -> String -> IO ()
httpSource hzGate url = runResourceT $ writeBS hzGate $ getHttp url
{-# INLINE httpSource #-}

stdinGZSource :: ChanBounded B.ByteString -> IO ()
stdinGZSource zlGate = writeBS zlGate $ Zip.gunzip getStdin
{-# INLINE stdinGZSource #-}

stdinRawSource :: ChanBounded L.ByteString -> IO ()
stdinRawSource input = feedChanBounded input $ lazyLineSplit getStdin
{-# INLINE stdinRawSource #-}

gatedGZ :: ChanBounded B.ByteString -> ChanBounded B.ByteString -> IO ()
gatedGZ hzGate zlGate = do
  let mbs = readBS hzGate
  writeBS zlGate $ Zip.gunzip mbs
{-# INLINE gatedGZ #-}

gatedLazy :: ChanBounded B.ByteString -> ChanBounded L.ByteString -> IO ()
gatedLazy zlGate input = do
  let mbs = readBS zlGate
  feedChanBounded input $ lazyLineSplit mbs
{-# INLINE gatedLazy #-}
