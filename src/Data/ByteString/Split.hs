{-# LANGUAGE BangPatterns #-}

module Data.ByteString.Split where

import qualified Data.ByteString.Lazy as L

-- | Splits a lazy 'L.ByteString' on the first newline (@\\n@) character, returning
-- a tuple of the contents before and after the newline byte (which is itself omitted).
--
-- If a bytestring @x@ contains no newline characters,
-- > unconsLine x = Just (x, L.empty)
--
-- prop> unconsLine L.empty = Nothing
unconsLine :: L.ByteString -> Maybe (L.ByteString, L.ByteString)
unconsLine lb
  | L.null lb = Nothing
  | otherwise =
    case L.elemIndex 0xa lb of
      Nothing  -> Just (lb, L.empty)
      Just !ix -> Just (L.take ix lb, L.drop (ix+1) lb)
{-# INLINE unconsLine #-}
