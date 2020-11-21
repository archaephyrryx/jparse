{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}

-- | FFI-based function[s] for efficiently \"seeking\" into strict 'ByteString's
module Util.ByteString.Seek ( skipString ) where

import Data.ByteString          (ByteString)
import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)

import Data.Word (Word8)

import Foreign.Ptr              (Ptr, plusPtr, minusPtr, nullPtr)
import Foreign.C.Types          (CSize(..))
import Foreign.ForeignPtr       (withForeignPtr)

foreign import capi unsafe "static seek.h seek_quote" c_seek_quote
    :: Ptr Word8 -> CSize ->  IO (Ptr Word8)

-- | Given a 'ByteString' whose first byte is strictly inside of a quoted string (JSON or equivalent syntax),
-- attempt to return the suffix of that 'ByteString' immediately following the closing double-quote,
-- effectively \'skipping\' over the payload and delimiter of the quoted string.
--
-- Requires that the leading double-quote byte is not present; if it is included, this function will
-- instead skip the leading double-quote and return the suffix of the 'ByteString' beginning with the
-- first byte of the quoted string's contents.
--
-- If no closing double-quote is found, returns 'Nothing'.
--
-- This implementation uses memchr(3).
skipString :: ByteString -> Maybe ByteString
skipString (PS fp !o l) = accursedUnutterablePerformIO $ withForeignPtr fp go
  where
    go !ptr = do
        q <- c_seek_quote (ptr`plusPtr`o) $ fromIntegral l
        if q == nullPtr
            then return $ Nothing
            else let !j = (q`minusPtr`ptr) + 1
                     !l' = l + o - j
            in return $ Just $ PS fp j l'
