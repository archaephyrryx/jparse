{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
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

import Vectorize (toVectorsIO, toVectorsLBS)
import Global
import Helper

-- * Source creation

type MBStream m r = Stream (BS.ByteString m) m r
type LBStream m r = Stream (Of L.ByteString) m r
type VLStream m r = Stream (Of (Vector L.ByteString)) m r


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

-- | Performs conditional decompression of a monadic bytestring (format argument second)
condUnzip :: MonadIO m => BS.ByteString m () ->  Bool ->  BS.ByteString m ()
condUnzip mbs = fi (Zip.gunzip mbs) mbs
{-# INLINE condUnzip #-}

gunzipLines :: MonadIO m => BS.ByteString m () -> MBStream m ()
gunzipLines = BS8.lines . Zip.gunzip
{-# INLINE gunzipLines #-}

gunzipLineSplit :: MonadIO m => BS.ByteString m () -> MBStream m ()
gunzipLineSplit = BS8.lineSplit nLines . Zip.gunzip
{-# INLINE gunzipLineSplit #-}

-- | Convert monadic bytestring to raw single-line stream via conditional gunzip
convertLines :: MonadIO m => BS.ByteString m () -> Bool -> MBStream m ()
convertLines = linesOf . condUnzip
{-# INLINE convertLines #-}

-- | Convert monadic bytestring to raw multi-line stream via conditional gunzip
convertLineSplit :: MonadIO m => BS.ByteString m () -> Bool -> MBStream m ()
convertLineSplit = lineSplitOf . condUnzip
{-# INLINE convertLineSplit #-}


-- Parametric Abstraction

-- * Compound Combinators

stdinLines, stdinLineSplit :: MonadIO m => Bool -> MBStream m ()
stdinLines     = convertLines     getStdin
stdinLineSplit = convertLineSplit getStdin
{-# INLINE stdinLines #-}
{-# INLINE stdinLineSplit #-}

httpLines, httpLineSplit :: (MonadIO m, MonadResource m) => String -> Bool -> MBStream m ()
httpLines     url = convertLines     $ getHttp url
httpLineSplit url = convertLineSplit $ getHttp url
{-# INLINE httpLines #-}
{-# INLINE httpLineSplit #-}

streamlines, streamlinesGZ :: MonadIO m => MBStream m ()
streamlines   = stdinLines False
streamlinesGZ = stdinLines True
{-# INLINE streamlines #-}
{-# INLINE streamlinesGZ #-}


-- * Vectorization combinators and fusions

vectorLines, vectorLineSplit :: MonadIO m => BS.ByteString m () -> VLStream m ()
vectorLines     = toVectorsIO  nLines . lazyLines
vectorLineSplit = toVectorsLBS nLines . lazyLineSplit

{-# INLINE vectorLines #-}
{-# INLINE vectorLineSplit #-}

vecStream, vecStreamSplit :: VLStream IO ()
vecStream      = vectorLines     getStdin
vecStreamSplit = vectorLineSplit getStdin

vecStreamGZ, vecStreamSplitGZ :: VLStream IO ()
vecStreamGZ      = vectorLines     $ Zip.gunzip getStdin
vecStreamSplitGZ = vectorLineSplit $ Zip.gunzip getStdin

vecStreamOf, vecStreamSplitOf :: Bool -> VLStream IO ()
vecStreamOf      = vectorLines     . condUnzip getStdin
vecStreamSplitOf = vectorLineSplit . condUnzip getStdin

{-# INLINE vecStream #-}
{-# INLINE vecStreamGZ #-}
{-# INLINE vecStreamOf #-}

{-# INLINE vecStreamSplit #-}
{-# INLINE vecStreamSplitGZ #-}
{-# INLINE vecStreamSplitOf #-}

vecStreamOfHttp, vecStreamSplitOfHttp :: (MonadResource m, MonadIO m)
                                      => String -> Bool -> VLStream m ()
vecStreamOfHttp      url = vectorLines     . condUnzip (getHttp url)
vecStreamSplitOfHttp url = vectorLineSplit . condUnzip (getHttp url)

{-# INLINE vecStreamOfHttp #-}
{-# INLINE vecStreamSplitOfHttp #-}

-- * Helper Functions

-- | Shorthand for conversion of a stream of monadic bytestrings to a stream of lazy bytestrings
lazy :: MonadIO m => MBStream m r -> LBStream m r
lazy = mapped BS.toLazy
{-# INLINE lazy #-}

-- | Applies 'lazy' transformation to the output of a one-argument function whose output is a stream of monadic bytestrings
lazyOf :: MonadIO m => (a -> MBStream m r) -> (a -> LBStream m r)
lazyOf f = lazy . f
{-# INLINE lazyOf #-}

lazyLines :: MonadIO m => BS.ByteString m () -> LBStream m ()
lazyLines = lazyOf BS8.lines
{-# INLINE lazyLines #-}

lazyLineSplit :: MonadIO m => BS.ByteString m () -> LBStream m ()
lazyLineSplit = lazyOf lineSplit
{-# INLINE lazyLineSplit #-}

-- | Generic function for producing a line-separated input stream from a monadic bytestring produced
--   via a specified transformation of a seed value
linesOf :: MonadIO m => (a -> BS.ByteString m ()) -> (a -> MBStream m ())
linesOf f = BS8.lines . f
{-# INLINE linesOf #-}

-- | Shorthand for @BS8.lineSplit@ with a parameter of 'nLines'
lineSplit :: MonadIO m => BS.ByteString m r -> MBStream m r
lineSplit = BS8.lineSplit nLines
{-# INLINE lineSplit #-}

-- | Generic function for producing a line-chunked input stream from a monadic bytestring produced
--   via a specified transformation of a seed value
lineSplitOf :: MonadIO m => (a -> BS.ByteString m ()) -> (a -> MBStream m ())
lineSplitOf f = lineSplit . f
{-# INLINE lineSplitOf #-}
