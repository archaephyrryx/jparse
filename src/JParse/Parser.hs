-- | Limited re-export of internal module @Parse.Parser.Zepto@ to allow for customized pipelines without JSON key-extraction
module JParse.Parser
  ( Parser
  , idParser
  ) where

import Parse.Parser.Zepto (Parser, popAll)
import Data.ByteString (ByteString)

-- | Line-mode parser that returns the entire contents of its buffer as a strict bytestring
idParser :: Parser ByteString
idParser = popAll
{-# INLINE idParser #-}
