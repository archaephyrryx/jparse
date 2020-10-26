{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Streaming.Sources where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming as BS

import qualified Data.ByteString.Streaming.HTTP as H
import Data.ByteString.Streaming.HTTP (MonadResource(..))

import           Streaming
import qualified Streaming.Zip as Zip

-- local module imports

import JParse.Helper

-- * Unprocessed sources

-- | Name-standardized alias for 'BS.stdin'
getStdin :: MonadIO m => BS.ByteString m ()
getStdin = BS.stdin
{-# INLINE getStdin #-}

-- | Monadic computation to procure a 'BS.ByteString' over http(s)
getHttp :: (MonadIO m, MonadResource m)
        => String -- ^ URL to read data from
        -> m (BS.ByteString m ())
getHttp !url = do
  req <- liftIO $ H.parseRequest url
  man <- liftIO $ H.newManager H.tlsManagerSettings
  resp <- H.http req man
  return $ H.responseBody resp
{-# INLINE getHttp #-}

-- * Unzipping

-- | Alias for 'Zip.gunzip'
unzip :: MonadIO m => BS.ByteString m () -> BS.ByteString m ()
unzip = Zip.gunzip
{-# INLINE unzip #-}

-- | Performs conditional decompression of a monadic 'BS.ByteString'
condUnzip :: MonadIO m
          => Bool -- ^ Indicates whether input is compressed (performs decompression if @True@)
          -> BS.ByteString m () -- ^ Possibly compressed bytestring
          -> BS.ByteString m () -- ^ Non-compressed bytestring
condUnzip !b mbs = if_ b (Zip.gunzip mbs) mbs
{-# INLINE condUnzip #-}

-- * Stream Conversion

-- | Convert each monadic 'BS.ByteString' in a 'Stream' to a strict 'B.ByteString'
toStricts :: Monad m => Stream (BS.ByteString m) m r -> Stream (Of B.ByteString) m r
toStricts = mappedPost _toStrict
  where
    _toStrict :: Monad m => BS.ByteString m r -> m (Of B.ByteString r)
    _toStrict mbs = do
      (lbs :> ret) <- BS.toLazy mbs
      return $! (L.toStrict lbs :> ret)
    {-# INLINE _toStrict #-}
{-# INLINE toStricts #-}
