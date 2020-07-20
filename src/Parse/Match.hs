{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse.Match (mapClass, parseMatch, ParseClass(..)) where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when)
import qualified Data.ByteString.Base16 as H (encode)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Word (Word8)

import           Data.Bits


-- import GHC.Prim


import Parse.Read (skipToEndQ)


type UnconBS = (Word8, ByteString)
-- | 4-hex digit bytestring to be matched case-insensitively
type Quad = ByteString
-- | sequence of two quads for surrogate pair hi-low escapes
type QuadPair = (Quad,Quad)


type EscapeSeq = Word8
type ControlChar = Word8

-- | binary predicate that matches control characters to escaped literals
escapesTo :: ControlChar -> EscapeSeq -> Bool
escapesTo 0x0a 0x6e = True
escapesTo 0x08 0x62 = True
escapesTo 0x0c 0x66 = True
escapesTo 0x0d 0x72 = True
escapesTo 0x09 0x74 = True
escapesTo    _    _ = False
{-# INLINE escapesTo #-}

-- type classifying Unicode characters according to how they are to be parsed
data ParseClass = BSlash
                | VQuote
                | FSlash
                | Control Word8  Quad
                | Direct  Word8  Quad
                | BMP    UnconBS Quad
                | Surr   UnconBS QuadPair
                deriving (Eq)

_quad :: Quad -> A.Parser ()
_quad = void . A.stringCI
{-# INLINE _quad #-}

-- | match : attempt to match parser output against a single parse-class character directive
_match :: ParseClass -> A.Parser ()
_match BSlash        = _bslash
_match VQuote        = _vquote
_match FSlash        = _fslash
_match (Direct  w q)  = _ascii w q
_match (Control w q)  = _ctr w q
_match (BMP  v q)     = _char v q
_match (Surr v q)     = _surr v q


-- | parse every valid JSON-string internal encoding of a backslash character
_bslash :: A.Parser ()
_bslash = A.anyWord8 >>= \case
            0x5c -> A.anyWord8 >>= \case
                0x5c -> return () -- \\
                0x75 -> void $ _quad "005c" -- \u005c
                _    -> mzero
            _    -> mzero
{-# INLINE _bslash #-}

-- because 0022 is digit-only, A.string is slightly better than A.stringCI
_vquote :: A.Parser ()
_vquote = A.anyWord8 >>= \case
            0x5c -> A.anyWord8 >>= \case
                0x22 -> return () -- \"
                0x75 -> void $ A.string "0022" -- \u0022
                _    -> mzero
            _    -> mzero
{-# INLINE _vquote #-}

_fslash :: A.Parser ()
_fslash = A.anyWord8 >>= \case
            0x2f -> return ()
            0x5c -> A.anyWord8 >>= \case
                0x2f -> return ()
                0x75 -> _quad "002f" -- \u002f
                _    -> mzero
            _    -> mzero
{-# INLINE _fslash #-}

_ascii :: Word8 -> Quad -> A.Parser ()
_ascii w q = A.anyWord8 >>= \case
            0x5c -> void $ A.word8 0x75 >> _quad q
            w'   | w' == w
                 -> return ()
            _    -> mzero
{-# INLINE _ascii #-}


_ctr :: Word8 -> Quad -> A.Parser ()
_ctr w q = A.anyWord8 >>= \case
            0x5c -> A.anyWord8 >>= \case
                0x75 -> _quad q
                w' | w `escapesTo` w'
                   -> return ()
                _    -> mzero
            _    -> mzero
{-# INLINE _ctr #-}

_char :: UnconBS -> Quad -> A.Parser ()
_char v@(w,t) q
    = A.anyWord8 >>= \case
        0x5c -> A.anyWord8 >>= \case
            0x75 -> _quad q
            _    -> mzero
        x | x == w
             -> void $ A.string t
        _    -> mzero

_surr :: UnconBS -> QuadPair -> A.Parser ()
_surr v@(w,t) (h,l)
    = A.anyWord8 >>= \case
        0x5c -> A.anyWord8 >>= \case
            0x75 -> _qquad h l
            _    -> mzero
        x | x == w
             -> void $ A.string t
        _    -> mzero
    where
        _qquad :: Quad -> Quad -> A.Parser ()
        _qquad h l = do { _quad h; A.word8 0x5c; A.word8 0x75; _quad l }
        {-# INLINE _qquad #-}


depack :: Int -> ByteString -> ([Word8], ByteString)
depack 0 b = ([],b)
depack n b =
    let (h, t) = B.splitAt n b
     in (B.unpack h, t)
{-# INLINE depack #-}


-- | treats an arbitrary character as a parse directive for \u escaped hexadecimal sequences
_class :: ByteString -> (ParseClass, ByteString)
_class b = case B.uncons b of
    Just (w, bt) | w < 0x20
                 -> let q = "00" <> H.encode (B.singleton w)
                     in (Control w q, bt)
                 | w < 0x80
                 -> case w of
                        0x22 -> (VQuote, bt)
                        0x5c -> (BSlash, bt)
                        0x2f -> (FSlash, bt)
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
                        q1 = H.encode (B.singleton  hh)
                          <> H.encode (B.singleton  hl)
                        q2 = H.encode (B.singleton  lh)
                          <> H.encode (B.singleton  ll)
                     in (Surr (w,B.pack$w') (q1,q2), bt')
                 | otherwise -> _invalidUnicodeError
    _ -> undefined -- unchecked end-of-bytestring


_invalidUnicodeError = error "encountered invalid unicode byte in query string"

mapClass :: ByteString -> [ParseClass]
mapClass b | B.null b = mempty
           | otherwise =
                let (c, b') = _class b
                 in c : mapClass b'


-- | parseMatch : attempt to match against pre-classified query key,
--   skipping to end of current string if a non-match is found
parseMatch :: [ParseClass] -> A.Parser Bool
parseMatch x = matching x <|> (False <$ skipToEndQ)
    where
        matching = (True <$) . mapM_ (_match)
