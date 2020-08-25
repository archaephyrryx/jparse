module Driver.Distributor where

import Control.Concurrent.Async

import Control.Monad.IO.Class (MonadIO(..), liftIO)
import Control.Monad.Trans.Resource (runResourceT)

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
import Final
import Global
import Streams


-- * Ungated version

distributor :: ChanBounded (Bundle L.ByteString) -> Bool -> IO ()
distributor input isZipped = async go >> pure ()
  where
    go = do
      S.mapM_ (writeChanBounded input) $ vecStreamOf  (InFormat isZipped)
      writeChanBounded input mempty
    {-# INLINE go #-}
{-# INLINE distributor #-}

distributorHttp :: ChanBounded (Bundle L.ByteString) -> String -> Bool -> IO ()
distributorHttp input url isZipped = async go >>= link
  where
    go = do
      runResourceT $ S.mapM_ (liftIO . writeChanBounded input) $ vecStreamOfHttp  url (InFormat isZipped)
      writeChanBounded input mempty
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
  httpThread <- async $ httpSource hvGate url
  link httpThread
  async $ shortedVect hvGate input
  return ()

httpSource :: ChanBounded B.ByteString -> String -> IO ()
httpSource hzGate url = runResourceT $ feedChanBounded hzGate $ BS.toChunks (getHttp url)

stdinGZSource :: ChanBounded B.ByteString -> IO ()
stdinGZSource zvGate = feedChanBounded zvGate $ toStricts streamlinesGZ

stdinRawSource :: ChanBounded (Vector L.ByteString) -> IO ()
stdinRawSource input = feedChanBounded input vecStream

gatedGZ :: ChanBounded B.ByteString -> ChanBounded B.ByteString -> IO ()
gatedGZ hzGate zvGate = do
  let stream = drainChanBounded hzGate
  feedChanBounded zvGate $ toStricts $ gunzipLines $ BS.fromChunks stream

shortedVect :: ChanBounded B.ByteString -> ChanBounded (Vector L.ByteString) -> IO ()
shortedVect zvGate input = do
  let stream = drainChanBounded zvGate
  feedChanBounded input $ toVectorsIO nLines $ lazyLines $ BS.fromChunks stream

gatedVect :: ChanBounded B.ByteString -> ChanBounded (Vector L.ByteString) -> IO ()
gatedVect zvGate input = do
  let stream = drainChanBounded zvGate
  feedChanBounded input $ toVectorsIO nLines $ S.map L.fromStrict stream


