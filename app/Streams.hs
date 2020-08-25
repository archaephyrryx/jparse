{-# LANGUAGE PatternSynonyms #-}

module Streams where

import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import qualified Data.ByteString.Streaming.HTTP as H
import Data.ByteString.Streaming.HTTP (MonadResource(..), HttpException(..))

import           Streaming
import           Streaming.Internal (Stream(..))
import qualified Streaming.Prelude as S
import qualified Streaming.Zip as Zip

import           Data.Vector (Vector)
import qualified Data.Vector as V


import Control.Exception.Base (try)


-- local module imports

import Final (toVectorsIO)
import Global


-- Newtypes for variants
newtype SourceType = SourceType { isHttp :: Bool }
newtype InFormat = InFormat { isZipped :: Bool }

-- Patterns for variant types

pattern Stdin :: SourceType
pattern Stdin = SourceType False

pattern Http :: SourceType
pattern Http = SourceType True

{-# COMPLETE Stdin, Http #-}

pattern Raw :: InFormat
pattern Raw = InFormat False

pattern Zipped :: InFormat
pattern Zipped = InFormat True

{-# COMPLETE Raw, Zipped #-}

-- helper functions

if_ :: Bool -> a -> a -> a
if_ p x y = if p then x else y
{-# INLINE if_ #-}

fi :: a -> a -> Bool -> a
fi x y p = if_ p x y
{-# INLINE fi #-}

lazyLines :: MonadIO m => BS.ByteString m () -> Stream (Of L.ByteString) m ()
lazyLines = mapped BS.toLazy . BS8.lines
{-# INLINE lazyLines #-}

gunzipLines :: MonadIO m => BS.ByteString m () -> Stream (BS.ByteString m) m ()
gunzipLines = BS8.lines . Zip.gunzip
{-# INLINE gunzipLines #-}

-- | Generic function for producing a line-split input stream from a monadic bytestring produced
--   via a specified transformation of a seed value
linesOf :: MonadIO m => (a -> BS.ByteString m ()) -> (a -> Stream (BS.ByteString m) m ())
linesOf f x = BS8.lines $ f x
{-# INLINE linesOf #-}

-- | Convert monadic bytestring to normalized raw format via conditional gunzip
convertLines :: MonadIO m => BS.ByteString m () -> InFormat -> Stream (BS.ByteString m) m ()
convertLines src = linesOf input
  where
    input = fi (Zip.gunzip src) src . isZipped
    {-# INLINE input #-}
{-# INLINE convertLines #-}

stdinLines :: MonadIO m => InFormat -> Stream (BS.ByteString m) m ()
stdinLines = convertLines BS.stdin
{-# INLINE stdinLines #-}

httpLines :: (MonadIO m, MonadResource m) => String -> InFormat -> Stream (BS.ByteString m) m ()
httpLines url = convertLines (getHttp url)
{-# INLINE httpLines #-}

getHttp :: (MonadIO m, MonadResource m) => String -> BS.ByteString m ()
getHttp url = rejoin $ do
  req <- liftIO $ H.parseRequest url
  man <- liftIO $ H.newManager H.tlsManagerSettings
  H.http req man

rejoin :: Monad m
       => m (H.Response (BS.ByteString m ()))
       ->                BS.ByteString m ()
rejoin = BS.mwrap . fmap H.responseBody
{-# INLINE rejoin #-}

streamlines :: MonadIO m => Stream (BS8.ByteString m) m ()
streamlines = stdinLines Raw
{-# INLINE streamlines #-}

streamlinesGZ :: MonadIO m => Stream (BS8.ByteString m) m ()
streamlinesGZ = stdinLines Zipped
{-# INLINE streamlinesGZ #-}

vecStream :: Stream (Of (Vector L.ByteString)) IO ()
vecStream = toVectorsIO nLines $ mapped BS.toLazy streamlines
{-# INLINE vecStream #-}

vecStreamGZ :: Stream (Of (Vector L.ByteString)) IO ()
vecStreamGZ = toVectorsIO nLines $ mapped BS.toLazy streamlinesGZ
{-# INLINE vecStreamGZ #-}

vecStreamOf :: InFormat -> Stream (Of (Vector L.ByteString)) IO ()
vecStreamOf = toVectorsIO nLines . mapped BS.toLazy . stdinLines
{-# INLINE vecStreamOf #-}

vecStreamOfHttp :: (MonadResource m, MonadIO m)
                => String
                -> InFormat
                -> Stream (Of (Vector L.ByteString)) m ()
vecStreamOfHttp url = toVectorsIO nLines . mapped BS.toLazy . httpLines url
{-# INLINE vecStreamOfHttp #-}
