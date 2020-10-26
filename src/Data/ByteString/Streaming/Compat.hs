{-|
Module      : Data.ByteString.Streaming.Compat
Description : streaming-bytestring compatibility suite
Copyright   : (c) Peter Duchovni, 2020
License     : BSD-3
Maintainer  : caufeminecraft+github@gmail.com

Module suite for re-exporting streaming-bytestring definitions
in a consistent style with the 0.1.7 version interface changes.

Avoids importing deprecated modules if v0.1.7 is used, while maintaining
backwards-compatibility with API of v0.1.6
-}
module Data.ByteString.Streaming.Compat
  ( module Data.ByteString.Streaming.Compat.Type
  , module Data.ByteString.Streaming.Compat.Internal
  ) where

import Data.ByteString.Streaming.Compat.Type
import Data.ByteString.Streaming.Compat.Internal
