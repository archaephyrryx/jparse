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
getHttp :: (MonadIO m, MonadResource m) => String -> m (BS.ByteString m ())
getHttp url = do
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

-- | Performs conditional decompression of a monadic bytestring (format argument second)
condUnzip :: MonadIO m =>  Bool ->  BS.ByteString m () -> BS.ByteString m ()
condUnzip b mbs = if_ b (Zip.gunzip mbs) mbs
{-# INLINE condUnzip #-}
