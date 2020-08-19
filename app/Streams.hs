{-# LANGUAGE PatternSynonyms #-}

module Streams where

import Data.Semigroup ((<>))

import Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.ByteString (ByteString)

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as D
import qualified Data.ByteString.Builder.Extra as D

import Data.Either (isLeft, fromLeft)

import qualified Conduit as C
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import Data.Conduit ((.|))

import qualified Data.ByteString.Streaming as BS
import qualified Data.ByteString.Streaming.Char8 as BS8

import qualified Data.ByteString.Streaming.HTTP as H
import Data.ByteString.Streaming.HTTP (MonadResource(..))

import Streaming
import qualified Streaming.Prelude as S
import Streaming.Internal (Stream(..))

import qualified Streaming.Zip as Zip

import qualified Parse.ReadAlt as Alt

import qualified Parse.ReadZepto as Zep
import qualified Parse.Parser.Zepto as Z

import qualified Parse.ReadStream as ZepS
import qualified Parse.Parser.ZeptoStream as ZS

import Final (toVector, toVectorIO, toVectorsIO)
import Global (nLines)

import Data.Vector (Vector)
import qualified Data.Vector as V


-- Newtypes for variants
newtype SourceType = SourceType { isHttp :: Bool }
newtype InFormat = InFormat { isZipped :: Bool }
newtype Packaging = Packaging { isVector :: Bool }


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

pattern ToList :: Packaging
pattern ToList = Packaging False

pattern ToVector :: Packaging
pattern ToVector = Packaging True

{-# COMPLETE ToList, ToVector #-}


-- helper functions

if_ :: Bool -> a -> a -> a
if_ p x y = if p then x else y
{-# INLINE if_ #-}

fi :: a -> a -> Bool -> a
fi x y p = if_ p x y
{-# INLINE fi #-}




-- | Generic function for producing a line-split input stream from a monadic bytestring produced
--   via a specified transformation of a seed value
linesOf :: MonadIO m => (a -> BS8.ByteString m ()) -> (a -> Stream (BS8.ByteString m) m ())
linesOf f x = BS8.lines $ f x
{-# INLINE linesOf #-}

-- | Convert monadic bytestring to normalized raw format via conditional gunzip
convertLines :: MonadIO m => BS8.ByteString m () -> InFormat -> Stream (BS8.ByteString m) m ()
convertLines src = linesOf input
  where
    input = fi (Zip.gunzip src) src . isZipped
    {-# INLINE input #-}
{-# INLINE convertLines #-}

stdinLines :: MonadIO m => InFormat -> Stream (BS8.ByteString m) m ()
stdinLines = convertLines BS.stdin
{-# INLINE stdinLines #-}

httpLines :: (MonadIO m, MonadResource m) => String -> InFormat -> Stream (BS8.ByteString m) m ()
httpLines url = convertLines (getHttp url)
{-# INLINE httpLines #-}

getHttp :: (MonadIO m, MonadResource m) => String -> BS8.ByteString m ()
getHttp url = rejoin $ do
  req <-liftIO $ H.parseRequest url
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

chunkStream :: MonadIO m
            => Stream (BS8.ByteString m) m ()
            -> Stream (Stream (Of L.ByteString) m) m ()
chunkStream = chunksOf nLines . mapped BS.toLazy
{-# INLINE chunkStream #-}

listStream :: Stream (Of [L.ByteString]) IO ()
listStream = mapped S.toList $ chunkStream streamlines
{-# INLINE listStream #-}

listStreamGZ ::  Stream (Of [L.ByteString]) IO ()
listStreamGZ = mapped S.toList $ chunkStream streamlinesGZ
{-# INLINE listStreamGZ #-}

listStreamOf :: InFormat -> Stream (Of [L.ByteString]) IO ()
listStreamOf = mapped S.toList . chunkStream . stdinLines
{-# INLINE listStreamOf #-}

listStreamOfHttp :: (MonadResource m, MonadIO m)
                 => String
                 -> InFormat
                 -> Stream (Of [L.ByteString]) m ()
listStreamOfHttp url = mapped S.toList . chunkStream . httpLines url
{-# INLINE listStreamOfHttp #-}

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
