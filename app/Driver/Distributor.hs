module Driver.Distributor where

import Control.Monad.IO.Class (MonadIO(..), liftIO )

import qualified Data.ByteString.Lazy as L

import qualified Streaming.Prelude as S

import Control.Monad.Trans.Resource (runResourceT)

import Final
import Streams
import Global

import Driver.Internal

-- Concurrency mode
import Control.Concurrent.Async

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
