{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface, CApiFFI #-}

module Data.ByteString.Seek ( skipString ) where

import Data.ByteString          (ByteString)
import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)

import Data.Word (Word8)

import Foreign.Ptr              (Ptr, plusPtr, minusPtr, nullPtr)
import Foreign.C.Types          (CSize(..))
import Foreign.ForeignPtr       (withForeignPtr)



foreign import capi unsafe "static seek.h seek_quote" c_seek_quote
    :: Ptr Word8 -> CSize ->  IO (Ptr Word8)

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
