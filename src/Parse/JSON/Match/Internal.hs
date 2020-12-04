{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parse.JSON.Match.Internal
Description : Definitions of parse-directives for JSON-string matching
Copyright   : (c) Peter Duchovni, 2020
License     : BSD-3
Maintainer  : caufeminecraft+github@gmail.com

Type definitions and internal logic for translating JSON-encoded strings into
sequences of 'ParseClass' values; each 'ParseClass' is an abstract lexical token
that allows for a canonical form of codepoints with multiple valid JSON-string 
representations, and the sequence in question is used as a specific directive for
parsing any valid JSON-encoded byte-sequence that is to be considered a match against
the given string.

-}
module Parse.JSON.Match.Internal
  ( DeconBS
  , UnconBS
  , Quad
  , QuadPair
  , escapesTo
  , ParseClass(..)
  , Res
  , pass
  , mark
  , fail
  , mapClass
  )
  where

import           Prelude hiding (fail)
import           Control.Monad (MonadPlus(..))
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Word (Word8)
import qualified Data.ByteString.Base16 as H (encode)

import Parse.ASCII.ByteLiterals

type DeconBS = (Word8, [Word8])
type UnconBS = (Word8, ByteString)

-- | Bytestring consisting of 4 hexadecimal characters, to be matched case-insensitively
type Quad = ByteString
-- | sequence of two quads for surrogate pair hi-low escapes
type QuadPair = (Quad,Quad)


type EscapeSeq = Word8
type ControlChar = Word8

-- | binary predicate that matches control characters to (un)escaped literals
escapesTo :: ControlChar -> EscapeSeq -> Bool
escapesTo Ctr_n Esc_n = True
escapesTo Ctr_b Esc_b = True
escapesTo Ctr_f Esc_f = True
escapesTo Ctr_r Esc_r = True
escapesTo Ctr_t Esc_t = True
escapesTo _     _     = False
{-# INLINE escapesTo #-}

-- | ParseClass is an algebraic type expressing specific characters and character sequences
--   to be interpreted as parser directives. With the exception of @BSlash@, @VQuote@, and
--   @FSlash@, all constructors simultaneously carry a basic representation and their representation
--   as hexadecimal tetragraphs (@Quad@ or @QuadPair@).
data ParseClass = BSlash -- ^ Backslash
                | VQuote -- ^ Verbatim double-quote
                | FSlash -- ^ Forward slash
                | Control Word8  Quad -- ^ Control character
                | Direct  Word8  Quad -- ^ Directly representable ASCII
                | BMP    UnconBS Quad -- ^ UTF-16 character within the BMP
                | Surr   DeconBS QuadPair -- ^ UTF-16 surrogate pair
                deriving (Eq)

-- | abstraction for parser success/failure
type Res = Bool

-- | monadic computation indicating success
pass :: (Monad m) => m Res
pass = return True
{-# INLINE pass #-}

-- | map success value over a monadic computation
mark :: (Monad m) => m a -> m Res
mark = (True <$)
{-# INLINE mark #-}

-- | monadic computation indicating failure
fail :: (MonadPlus m) => m Res
fail = return False
{-# INLINE fail #-}

-- | unpack the first @n@ characters of a ByteString into a list of words
depack :: Int -> ByteString -> ([Word8], ByteString)
depack 0 b = ([],b)
depack n b =
    let (h, t) = B.splitAt n b
     in (B.unpack h, t)
{-# INLINE depack #-}

-- | Extracts the first full codepoint in a ByteString and converts it into a 'ParseClass' value
_class :: ByteString -> (ParseClass, ByteString)
_class b = case B.uncons b of
    Just (w, bt) | w < 0x20
                 -> let q = "00" <> H.encode (B.singleton w)
                     in (Control w q, bt)
                 | w < 0x80
                 -> case w of
                        Quote  -> (VQuote, bt)
                        Bslash -> (BSlash, bt)
                        Slash  -> (FSlash, bt)
                        _    -> let q = "00" <> H.encode (B.singleton w)
                                 in (Direct w q, bt)
                 | w >= 0xc0 && w < 0xe0
                 -> let Just (w', bt') = B.uncons bt
                        hi = (w `shiftR` 2) .&. 0x7
                        lo = ((w .&. 0x3) `shiftL` 6) .|. (w' .&. 0x3f)
                        q = H.encode (B.singleton hi) <> H.encode (B.singleton lo)
                     in (BMP (w, B.singleton w') q, bt')
                 | w >= 0xe0 && w < 0xf0
                 -> let (w'@[w1,w2], bt') = depack 2 bt
                        hi = (w `shiftL` 4) .|. ((w1 `shiftR` 2) .&. 0xf)
                        lo = ((w1 .&. 0x3) `shiftL` 6) .|. (w2 .&. 0x3f)
                        q = H.encode (B.singleton hi) <> H.encode (B.singleton lo)
                     in (BMP (w, B.pack w') q, bt')
                 | w >= 0xf0 && w < 0xf8
                 -> let (w'@[w1,w2,w3], bt') = depack 3 bt
                        hh =  (w .&. 0x3) + 0xd8
                        hl =  ((w1 .&. 0x3f) `shiftL` 2)
                          .|. ((w2 `shiftR` 4) .&. 0x3)
                        lh =  ((w2 `shiftR` 2) .&. 0x3) + 0xdc
                        ll =  ((w2 .&. 0x3) `shiftL` 6)
                          .|.  (w3 .&. 0x3f)
                        q1 = H.encode (B.singleton hh)
                          <> H.encode (B.singleton hl)
                        q2 = H.encode (B.singleton lh)
                          <> H.encode (B.singleton ll)
                     in (Surr (w,w') (q1,q2), bt')
                 | otherwise -> _invalidUnicodeError
    _ -> undefined -- unchecked end-of-bytestring

_invalidUnicodeError :: (ParseClass, ByteString)
_invalidUnicodeError = error "encountered invalid unicode byte in query string"

-- | Maps every codepoint in a ByteString to a corresponding 'ParseClass' value
mapClass :: ByteString -> [ParseClass]
mapClass b | B.null b = []
           | otherwise =
             let (c, b') = _class b
              in c : mapClass b'
