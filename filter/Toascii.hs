{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Toascii (toAlabels) where

import           Data.Char (chr)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8 (ByteString, pack, singleton)
import qualified Text.IDNA as IDNA

-- Besides U+002E (full stop) IDNA2003 allows DNS labels to be
-- separated by any of the Unicode variants U+3002 (ideographic
-- full stop), U+FF0E (fullwidth full stop), and U+FF61
-- (halfwidth ideographic full stop).

dots :: [Char]
dots = map chr [0x002E, 0x3002, 0xFF0E, 0xFF61]

encode :: T.Text -> Maybe T.Text
encode label
  | T.null label = Nothing
  | otherwise = IDNA.toASCII True True label

convert_labels :: [T.Text] -> Maybe [B8.ByteString]
convert_labels ls =
    let (wilds, rest) = L.span (== "*") ls
     in (map (B8.pack . T.unpack)) <$> ((wilds ++) <$> sequence (map encode rest))

toAlabels :: T.Text -> Maybe [B8.ByteString]
toAlabels s =
  if | null ls -> Nothing
     | length ls == 2 && all T.null ls
     -> Just $ [B8.singleton '.']
     | T.null (last ls)
     -> convert_labels (L.init ls)
     | otherwise -> convert_labels ls
  where ls = T.split (`elem` dots) s
