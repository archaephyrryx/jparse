module Sources where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import qualified Data.ByteString.Streaming.HTTP as H
import Data.ByteString.Streaming.HTTP (MonadResource(..))

import           Streaming
import qualified Streaming.Zip as Zip

-- local module imports

import JParse.Helper

-- | Name-standardized alias for 'BS.stdin'
getStdin :: MonadIO m => BS.ByteString m ()
getStdin = BS.stdin
{-# INLINE getStdin #-}

-- | Extract monadic bytestring from a url over http/https
getHttp :: (MonadIO m, MonadResource m) => String -> BS.ByteString m ()
getHttp url = rejoin $ do
  req <- liftIO $ H.parseRequest url
  man <- liftIO $ H.newManager H.tlsManagerSettings
  H.http req man
{-# INLINE getHttp #-}

rejoin :: Monad m
       => m (H.Response (BS.ByteString m ()))
       ->                BS.ByteString m ()
rejoin = BS.mwrap . fmap H.responseBody
{-# INLINE rejoin #-}

-- * Unzipping

-- | Alias for 'Zip.gunzip'
unzip :: MonadIO m => BS.ByteString m () -> BS.ByteString m ()
unzip = Zip.gunzip
{-# INLINE unzip #-}

-- | Performs conditional decompression of a monadic bytestring (format argument second)
condUnzip :: MonadIO m => BS.ByteString m () ->  Bool ->  BS.ByteString m ()
condUnzip mbs = fi (Zip.gunzip mbs) mbs
{-# INLINE condUnzip #-}
