module JParse.Driver.Distributor where

import Control.Concurrent.Async

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad (void)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Streaming.Zip as Zip

import JParse.Driver.Internal
import JParse.Global

import JParse.Streams
import JParse.Channels

-- | helper to suppress unused-do-binding warnings
vsync :: IO a -> IO ()
vsync = void . async
{-# INLINE vsync #-}


-- * Ungated version


distributor :: ChanBounded L.ByteString -> Bool -> IO ()
distributor inp isZipped = async go >> pure ()
  where
    src = lbsStreamSplitOf isZipped
    {-# INLINE src #-}

    go = feedChanBounded inp src
    {-# INLINE go #-}
{-# INLINE distributor #-}

distributorHttp :: ChanBounded L.ByteString -> String -> Bool -> IO ()
distributorHttp inp url isZipped = async go >>= link
  where
    src = lbsStreamSplitOfHttp url isZipped
    {-# INLINE src #-}

    go = runResourceT $ feedChanBounded inp src
    {-# INLINE go #-}
{-# INLINE distributorHttp #-}

-- * Gated Version

distributorGated :: ChanBounded L.ByteString -> Bool -> IO ()
distributorGated inp True = do
  zlGate <- newChanBounded uBound_gate
  vsync $ stdinGZSource zlGate
  vsync $ gatedLazy zlGate inp
  return ()
distributorGated inp False = do
  vsync $ stdinRawSource inp
  return ()
{-# INLINE distributorGated #-}

distributorHttpGated :: ChanBounded L.ByteString -> String -> Bool -> IO ()
distributorHttpGated inp url True = do
  hzGate <- newChanBounded uBound_gate
  zlGate <- newChanBounded uBound_gate
  link =<< async (httpSource hzGate url)
  vsync $ gatedGZ hzGate zlGate
  vsync $ gatedLazy zlGate inp
  return ()
distributorHttpGated inp url False = do
  hlGate <- newChanBounded uBound_gate
  link =<< async (httpSource hlGate url)
  vsync $ gatedLazy hlGate inp
  return ()
{-# INLINE distributorHttpGated #-}

httpSource :: ChanBounded B.ByteString -> String -> IO ()
httpSource hzGate url = runResourceT $ writeBS hzGate $ getHttp url
{-# INLINE httpSource #-}

stdinGZSource :: ChanBounded B.ByteString -> IO ()
stdinGZSource zlGate = writeBS zlGate $ Zip.gunzip getStdin
{-# INLINE stdinGZSource #-}

stdinRawSource :: ChanBounded L.ByteString -> IO ()
stdinRawSource inp = feedChanBounded inp $ lazyLineSplit getStdin
{-# INLINE stdinRawSource #-}

gatedGZ :: ChanBounded B.ByteString -> ChanBounded B.ByteString -> IO ()
gatedGZ hzGate zlGate = do
  let mbs = readBS hzGate
  writeBS zlGate $ Zip.gunzip mbs
{-# INLINE gatedGZ #-}

gatedLazy :: ChanBounded B.ByteString -> ChanBounded L.ByteString -> IO ()
gatedLazy zlGate inp = do
  let mbs = readBS zlGate
  feedChanBounded inp $ lazyLineSplit mbs
{-# INLINE gatedLazy #-}
