module Driver.Distributor where

import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import Streaming
import qualified Streaming.Prelude as S
import Streaming.Internal (Stream(..))

import Final
import Streams
import Global

import Driver.Internal

import Data.Vector (Vector)

-- Concurrency mode
import Control.Concurrent.Async

distributorHttp :: ChanBounded (Bundle L.ByteString) -> String -> Bool -> IO ()
distributorHttp input url True = do
  hzGate <- newChanBounded uBound_gate
  zvGate <- newChanBounded uBound_gate
  link =<< async (httpSource hzGate url)
  async $ gatedGZ hzGate zvGate
  async $ gatedVect zvGate input
  return ()
distributorHttp input url False = do
  hvGate <- newChanBounded uBound_gate
  httpThread <- async $ httpSource hvGate url
  link httpThread
  async $ shortedVect hvGate input
  return ()

distributor :: ChanBounded (Bundle L.ByteString) -> Bool -> IO ()
distributor input True = do
  zvGate <- newChanBounded uBound_gate
  async $ stdinGZSource zvGate
  async $ gatedVect zvGate input
  return ()
distributor input False = do
  async $ stdinRawSource input
  return ()

httpSource :: ChanBounded B.ByteString -> String -> IO ()
httpSource hzGate url = runResourceT $ feedChanBounded hzGate $ BS.toChunks (getHttp url)

stdinGZSource :: ChanBounded B.ByteString -> IO ()
stdinGZSource zvGate = feedChanBounded zvGate $ S.map L.toStrict $ mapped BS.toLazy streamlinesGZ

stdinRawSource :: ChanBounded (Vector L.ByteString) -> IO ()
stdinRawSource input = feedChanBounded input vecStream

gatedGZ :: ChanBounded B.ByteString -> ChanBounded B.ByteString -> IO ()
gatedGZ hzGate zvGate =
  feedChanBounded zvGate $
  S.map L.toStrict $
  mapped BS.toLazy $
  (`convertLines`Zipped) $
  BS.fromChunks $
  drainChanBounded hzGate

shortedVect :: ChanBounded B.ByteString -> ChanBounded (Vector L.ByteString) -> IO ()
shortedVect zvGate input = do
  let stream = drainChanBounded zvGate
  feedChanBounded input $ toVectorsIO nLines $ mapped BS.toLazy $ BS8.lines $ BS.fromChunks stream

gatedVect :: ChanBounded B.ByteString -> ChanBounded (Vector L.ByteString) -> IO ()
gatedVect zvGate input = do
  let stream = drainChanBounded zvGate
  feedChanBounded input $ toVectorsIO nLines $ S.map L.fromStrict $ stream
