{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module JParse.Channels where

import Prelude hiding (null)

import qualified Control.Concurrent.BoundedChan as BC
import qualified Data.ByteString as B
import qualified Streaming.Prelude as S

import Control.Concurrent
import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan)
import Streaming
import Streaming.Internal (Stream(..))

import qualified Data.ByteString.Streaming.Compat as BS

import Data.ByteString.Streaming.Compat.Type
import Data.Nullable

-- | Type alias to preserve abstract implementation
type ChanBounded a = BoundedChan a

-- | Create a 'ChanBounded' of specified capacity and arbitrary internal type
newChanBounded :: Int -> IO (ChanBounded a)
newChanBounded = newBoundedChan
{-# INLINE newChanBounded #-}

-- | Write a value to a 'ChanBounded' (alias for 'BC.writeChan')
writeChanBounded :: ChanBounded a -> a -> IO ()
writeChanBounded = BC.writeChan
{-# INLINE writeChanBounded #-}

-- | Read a value from a 'ChanBounded' (alias for 'BC.readChan')
readChanBounded :: ChanBounded a -> IO a
readChanBounded = BC.readChan
{-# INLINE readChanBounded #-}

-- | Feed values from a 'Stream' into a 'Chan', writing 'null' once the stream is exhausted
--
-- Note that it is impossible to distinguish between a literal 'null' value occuring in the
-- stream and the 'null' used as an end-of-stream marker; if 'null' writes are unavoidable
-- or otherwise desirable, use 'feedChanMaybe' instead.
feedChan :: (MonadIO m, Nullable a) => Chan a -> Stream (Of a) m r -> m ()
feedChan chan = go
  where
    go stream =
      S.uncons stream >>= \case
        Just (x, rest) -> liftIO (writeChan chan x) >> go rest
        Nothing -> liftIO $ writeChan chan null
{-# INLINE feedChan #-}

-- | Construct a 'Stream' consisting of values read from a 'Chan', terminating on 'null'
--
-- Note that it is impossible to distinguish between a literal 'null' value occuring in the
-- chan and the 'null' used as an end-of-input marker; if it is impossible to preclude 'null'
-- occuring in the chan, use 'drainChanMaybe' instead.
drainChan :: (MonadIO m, Nullable a) => Chan a -> Stream (Of a) m ()
drainChan chan = go
  where
    go = do
      x <- liftIO $ readChan chan
      if isNull x then Return () else Step (x :> go)
{-# INLINE drainChan #-}

-- | 'feedChan' for 'ChanBounded'
--
-- Use 'feedChanBoundedMaybe' if 'null' writes are unavoidable
feedChanBounded :: (MonadIO m, Nullable a) => ChanBounded a -> Stream (Of a) m r -> m ()
feedChanBounded cb = go
  where
    go stream =
      S.uncons stream >>= \case
        Just (x, rest) -> liftIO (writeChanBounded cb x) >> go rest
        Nothing -> liftIO $ writeChanBounded cb null
{-# INLINE feedChanBounded #-}

-- | 'drainChan' for 'ChanBounded'
--
-- Use 'drainChanBoundedMaybe' if 'null' reads are possible
drainChanBounded :: (MonadIO m, Nullable a) => ChanBounded a -> Stream (Of a) m ()
drainChanBounded cb = go
  where
    go = do
      x <- liftIO $ readChanBounded cb
      if isNull x then Return () else Step (x :> go)
{-# INLINE drainChanBounded #-}


-- | Feed 'Just'-wrapped values from a 'Stream' into a 'Chan', writing 'Nothing' once the stream is exhausted
--
-- This should be used over 'feedChan' if 'mempty' writs of the relevant type is an unavoidable possibility
-- or if the type in question does not have a 'Monoid' instance.
feedChanMaybe :: MonadIO m => Chan (Maybe a) -> Stream (Of a) m r -> m ()
feedChanMaybe chan = go
  where
    go stream =
      S.uncons stream >>= \case
        Just (x, rest) -> liftIO (writeChan chan $! Just x) >> go rest
        Nothing -> liftIO (writeChan chan Nothing)
{-# INLINE feedChanMaybe #-}

-- | Construct a 'Stream' consisting of unwrapped 'Just' values read from a 'Chan', terminating on 'Nothing'
--
-- This should be used in combination with 'feedChanMaybe' when 'mempty' may be written, or the relevant type
-- does not have instances of both 'Eq' and 'Monoid'.
drainChanMaybe :: MonadIO m => Chan (Maybe a) -> Stream (Of a) m ()
drainChanMaybe chan = go
  where
    go = do
      mx <- liftIO $ readChan chan
      case mx of
        Nothing -> Return ()
        Just x -> Step (x :> go)
{-# INLINE drainChanMaybe #-}

-- | 'feedChanMaybe' for 'ChanBounded'
feedChanBoundedMaybe :: MonadIO m => ChanBounded (Maybe a) -> Stream (Of a) m r -> m ()
feedChanBoundedMaybe cb = go
  where
    go stream =
      S.uncons stream >>= \case
        Just (x, rest) -> liftIO (writeChanBounded cb $! Just x) >> go rest
        Nothing -> liftIO $ writeChanBounded cb Nothing
{-# INLINE feedChanBoundedMaybe #-}

-- | 'drainChanMaybe' for 'ChanBounded'
drainChanBoundedMaybe :: MonadIO m => ChanBounded (Maybe a) -> Stream (Of a) m ()
drainChanBoundedMaybe cb = go
  where
    go = do
      mx <- liftIO $ readChanBounded cb
      case mx of
        Nothing -> Return ()
        Just x -> Step (x :> go)
{-# INLINE drainChanBoundedMaybe #-}

-- | Feed a (monadic) 'BS.ByteString' to a 'ChanBounded B.ByteString' by incrementally writing each chunk
writeBS :: MonadIO m => ChanBounded B.ByteString -> BS.ByteStream m r -> m ()
writeBS cb = go
  where
    go = \case
      Empty _ -> liftIO $ writeChanBounded cb B.empty
      Chunk b mb -> (liftIO $ writeChanBounded cb b) >> go mb
      Go m -> m >>= go
{-# INLINE writeBS #-}

-- | Extract a (monadic) 'BS.ByteString' from a 'ChanBounded' 'B.ByteString' populated via 'writeBS' or similar
readBS :: MonadIO m => ChanBounded B.ByteString -> BS.ByteStream m ()
readBS cb = go
  where
    go = Go $ do
      b <- liftIO $ readChanBounded cb
      if B.null b
        then return (Empty ())
        else return $ Chunk b go
{-# INLINE readBS #-}
