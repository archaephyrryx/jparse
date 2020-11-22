{-# LANGUAGE BangPatterns #-}

module Sources where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming.HTTP as H

import           Data.ByteString.Streaming.HTTP (MonadResource(..))
import           Streaming
import qualified Streaming.Zip as Zip

import qualified Streaming.ByteString as BS

import JParse.Helper

-- * Unprocessed sources

-- | Name-standardized alias for 'BS.stdin'
getStdin :: MonadIO m => BS.ByteStream m ()
getStdin = BS.stdin
{-# INLINE getStdin #-}

-- | Monadic computation to procure a 'BS.ByteString' over http(s)
getHttp :: (MonadIO m, MonadResource m)
        => String -- ^ URL to read data from
        -> m (BS.ByteStream m ())
getHttp !url = do
  req <- liftIO $ H.parseRequest url
  man <- liftIO $ H.newManager H.tlsManagerSettings
  resp <- H.http req man
  return $ H.responseBody resp
{-# INLINE getHttp #-}

-- * Unzipping

-- | Alias for 'Zip.gunzip'
unzip :: MonadIO m => BS.ByteStream m () -> BS.ByteStream m ()
unzip = Zip.gunzip
{-# INLINE unzip #-}

-- | Performs conditional decompression of a monadic 'BS.ByteString'
condUnzip :: MonadIO m
          => Bool -- ^ Indicates whether input is compressed (performs decompression if @True@)
          -> BS.ByteStream m () -- ^ Possibly compressed bytestring
          -> BS.ByteStream m () -- ^ Non-compressed bytestring
condUnzip !b mbs = if_ b (Zip.gunzip mbs) mbs
{-# INLINE condUnzip #-}
