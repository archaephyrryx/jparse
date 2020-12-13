{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

-- | Functions for reading and writing 'Stream's and 'BS.ByteStream's from and to 'BoundedChan's
module JParse.Channels (
  feedChan,
  drainChan,
  feedChanMaybe,
  drainChanMaybe,
  writeBS,
  readBS,
  -- * "Control.Concurrent.BoundedChan" re-exports
  BoundedChan,
  newBoundedChan,
  writeChan,
  readChan
  )
where

import qualified Data.ByteString as B
import qualified Streaming.Prelude as S

import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan, readChan, writeChan)
import Streaming
import Streaming.Internal (Stream(..))

import qualified Data.ByteString.Streaming.Compat as BS

import Data.ByteString.Streaming.Compat.Type
import Data.Nullable

-- | Feed values from a 'Stream' into a 'BoundedChan', writing 'nullValue' once the stream is exhausted
--
-- Note that it is impossible to distinguish between a literal 'nullValue' occuring in the
-- stream and the 'nullValue' used as an end-of-stream marker; if 'nullValue' writes are unavoidable
-- or otherwise desirable, use 'feedChanMaybe' instead.
feedChan :: (MonadIO m, Nullable a) => BoundedChan a -> Stream (Of a) m r -> m ()
feedChan cb = go
  where
    go stream =
      S.uncons stream >>= \case
        Just (x, rest) -> liftIO (writeChan cb x) >> go rest
        Nothing -> liftIO $ writeChan cb nullValue
{-# INLINE feedChan #-}


-- | Construct a 'Stream' consisting of values read from a 'BoundedChan', terminating on 'nullValue'
--
-- Note that it is impossible to distinguish between a literal 'nullValue' value occuring in the
-- channel and the 'nullValue' used as an end-of-input marker; if it is impossible to preclude 'nullValue'
-- occuring in the channel, use 'drainChanMaybe' instead.
drainChan :: (MonadIO m, Nullable a) => BoundedChan a -> Stream (Of a) m ()
drainChan cb = go
  where
    go = do
      x <- liftIO $ readChan cb
      if isNull x then Return () else Step (x :> go)
{-# INLINE drainChan #-}

-- | Feed 'Just'-wrapped values from a 'Stream' into a 'BoundedChan',
--   writing 'Nothing' once the stream is exhausted.
--
-- This should be used over 'feedChanBounded' if 'nullValue' writes of the relevant
-- type is an unavoidable possibility
-- or if the type in question does not have a 'Nullable' instance.
feedChanMaybe :: MonadIO m => BoundedChan (Maybe a) -> Stream (Of a) m r -> m ()
feedChanMaybe cb = go
  where
    go stream =
      S.uncons stream >>= \case
        Just (x, rest) -> liftIO (writeChan cb $! Just x) >> go rest
        Nothing -> liftIO $ writeChan cb Nothing
{-# INLINE feedChanMaybe #-}

-- | Construct a 'Stream' consisting of unwrapped 'Just' values
---  read from a 'BoundedChan', terminating on 'Nothing'.
--
-- This should be used in combination with 'feedChanMaybe'
-- when 'nullValue' may be written, or the relevant type
-- does not have instances of 'Nullable'
drainChanMaybe :: MonadIO m => BoundedChan (Maybe a) -> Stream (Of a) m ()
drainChanMaybe cb = go
  where
    go = do
      mx <- liftIO $ readChan cb
      case mx of
        Nothing -> Return ()
        Just x -> Step (x :> go)
{-# INLINE drainChanMaybe #-}

-- | Feed a 'BS.ByteStream' to a 'BoundedChan' 'B.ByteString' by sequential writes of each pure chunk
writeBS :: MonadIO m => BoundedChan B.ByteString -> BS.ByteStream m r -> m ()
writeBS cb = go
  where
    go = \case
      Empty _ -> liftIO $ writeChan cb B.empty
      Chunk b mb -> (liftIO $ writeChan cb b) >> go mb
      Go m -> m >>= go
{-# INLINE writeBS #-}

-- | Extract a 'BS.ByteStream' from a 'BoundedChan' 'B.ByteString' populated via 'writeBS' or similar
readBS :: MonadIO m => BoundedChan B.ByteString -> BS.ByteStream m ()
readBS cb = go
  where
    go = Go $ do
      b <- liftIO $ readChan cb
      if B.null b
        then return (Empty ())
        else return $ Chunk b go
{-# INLINE readBS #-}
