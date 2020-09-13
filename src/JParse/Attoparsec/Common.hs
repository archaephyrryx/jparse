{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module JParse.Attoparsec.Common
  ( putLnBuilder
  , trim
  ) where

import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Attoparsec.ByteString.Char8 as A (isSpace_w8)
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as D
import           Data.ByteString.Builder (Builder)
import           System.IO (stdout)

-- | print a Builder to stdout with a trailing newline
putLnBuilder :: MonadIO m => Maybe Builder -> m ()
putLnBuilder Nothing = pure ()
putLnBuilder (Just b) = liftIO $ D.hPutBuilder stdout (b <> D.word8 0xa)
{-# INLINE putLnBuilder #-}

-- | Strip leading whitespace from a ByteString
trim :: ByteString -> ByteString
trim !bs = B.dropWhile A.isSpace_w8 bs
{-# INLINE trim #-}