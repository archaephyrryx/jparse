{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Specialized parser directives for matching all possible representations
-- of JSON-encoded string values.
--
-- Compatible with UTF-16 BMP characters and surrogates pairs.
module Parse.Match (mapClass, parseMatch, ParseClass) where

import           Prelude hiding (fail)
import           Control.Applicative ((<|>))
import           Control.Monad (mzero, void, when, MonadPlus(..))
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A (stringCI)
import           Data.Bits
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.Word (Word8)
import qualified Data.ByteString.Base16 as H (encode)

import Parse.Read (skipToEndQ)
import Parse.Symbol


type DeconBS = (Word8, [Word8])
type UnconBS = (Word8, ByteString)

-- | Bytestring consisting of 4 hexadecimal characters, to be matched case-insensitively
type Quad = ByteString
-- | sequence of two quads for surrogate pair hi-low escapes
type QuadPair = (Quad,Quad)


type EscapeSeq = Word8
type ControlChar = Word8

-- | binary predicate that matches control characters to escaped literals
escapesTo :: ControlChar -> EscapeSeq -> Bool
--{-
escapesTo 0x0a 0x6e = True
escapesTo 0x08 0x62 = True
escapesTo 0x0c 0x66 = True
escapesTo 0x0d 0x72 = True
escapesTo 0x09 0x74 = True
--}
{-
escapesTo Ctr_n Esc_n = True
escapesTo Ctr_b Esc_b = True
escapesTo Ctr_f Esc_f = True
escapesTo Ctr_r Esc_r = True
escapesTo Ctr_t Esc_t = True
-}
escapesTo    _    _ = False
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


-- | abstraction for parser result

type Res = Bool

pass :: (Monad m) => m Res
pass = return True
{-# INLINE pass #-}

mark :: (Monad m) => m a -> m Res
mark = (True <$)
{-# INLINE mark #-}

fail :: (MonadPlus m) => m Res
fail = return False
{-# INLINE fail #-}

_quad :: Quad -> A.Parser Res
_quad = mark . A.stringCI
{-# INLINE _quad #-}

-- | match : attempt to match parser output against a single parse-class character directive
_match :: ParseClass -> A.Parser Res
_match BSlash         = _bslash
_match VQuote         = _vquote
_match FSlash         = _fslash
_match (Direct  w q)  = _ascii w q
_match (Control w q)  = _ctr w q
_match (BMP  v q)     = _char v q
_match (Surr v q)     = _surr v q


-- | parse every valid JSON-string internal encoding of a backslash character
_bslash :: A.Parser Res
--{-
_bslash = A.anyWord8 >>= \case
            0x5c -> A.anyWord8 >>= \case
                0x5c -> pass -- \\
                0x75 -> _quad "005c" -- \u005c
                _    -> fail
            _    -> fail
--}
{-
_bslash = A.anyWord8 >>= \case
            Bslash -> A.anyWord8 >>= \case
                Bslash -> pass -- \\
                Quad_u -> _quad "005c" -- \u005c
                _    -> fail
            _    -> fail
-}
{-# INLINE _bslash #-}

-- because 0022 is digit-only, A.string is slightly better than A.stringCI
_vquote :: A.Parser Res
_vquote = A.anyWord8 >>= \case
--{-
            0x5c -> A.anyWord8 >>= \case
                0x22 -> pass -- \"
                0x75 -> mark $ A.string "0022" -- \u0022
--}
{-
            Bslash -> A.anyWord8 >>= \case
                Quote -> pass -- \"
                Hex_u -> mark $ A.string "0022" -- \u0022
-}
                _    -> fail
            _    -> fail
{-# INLINE _vquote #-}

_fslash :: A.Parser Res
_fslash = A.anyWord8 >>= \case
--{-
            0x2f -> pass
            0x5c -> A.anyWord8 >>= \case
                0x2f -> pass
                0x75 -> _quad "002f" -- \u002f
--}
{-
            Slash  -> pass
            Bslash -> A.anyWord8 >>= \case
                Slash -> pass
                Hex_u -> _quad "002f" -- \u002f
-}
                _    -> fail
            _    -> fail
{-# INLINE _fslash #-}

_ascii :: Word8 -> Quad -> A.Parser Res
_ascii w q = A.anyWord8 >>= \case
            0x5c -> A.word8 0x75 >> _quad q
--            Bslash -> A.word8 Hex_u >> _quad q
            w' | w' == w
                 -> pass
            _    -> fail
{-# INLINE _ascii #-}


_ctr :: Word8 -> Quad -> A.Parser Res
_ctr w q = A.anyWord8 >>= \case
--{-
            0x5c -> A.anyWord8 >>= \case
                0x75 -> _quad q
--}
{-
            Slash -> A.anyWord8 >>= \case
                Hex_u -> _quad q
-}
                w' | w `escapesTo` w' -> pass
                _  -> fail
            _    -> fail
{-# INLINE _ctr #-}

_char :: UnconBS -> Quad -> A.Parser Res
_char ~(w,t) q
    = A.anyWord8 >>= \case
--{-
        0x5c -> A.anyWord8 >>= \case
            0x75 -> _quad q
--}
{-
        Bslash -> A.anyWord8 >>= \case
            Hex_u -> _quad q
-}
            _    -> fail
        x | x == w -> mark $ A.string t
        _    -> fail

_surr :: DeconBS -> QuadPair -> A.Parser Res
_surr ~(w,t) (h,l)
    = A.anyWord8 >>= \case
--{-
        0x5c -> A.anyWord8 >>= \case
            0x75 -> _qquad h l
--}
{-
        Bslash -> A.anyWord8 >>= \case
            Hex_u -> _qquad h l
-}
            _    -> fail
        x | x == w -> mark $ mapM_ A.word8 t
        _    -> fail
    where
        _qquad :: Quad -> Quad -> A.Parser Res
        _qquad h l = do { _quad h; A.word8 0x5c; A.word8 0x75; _quad l }
        --_qquad h l = do { _quad h; A.word8 Bslash; A.word8 Hex_u; _quad l }
        {-# INLINE _qquad #-}


depack :: Int -> ByteString -> ([Word8], ByteString)
depack 0 b = ([],b)
depack n b =
    let (h, t) = B.splitAt n b
     in (B.unpack h, t)
{-# INLINE depack #-}


-- | Maps the first character-token of a ByteString to a ParseClass directive that matches against all
--   JSON-encoded representations of that character, either as a literal or escaped ASCII character,
--   a literal
--   or as \u-escaped hexadecimal sequences
_class :: ByteString -> (ParseClass, ByteString)
_class b = case B.uncons b of
    Just (w, bt) | w < 0x20
                 -> let q = "00" <> H.encode (B.singleton w)
                     in (Control w q, bt)
                 | w < 0x80
                 -> case w of
                      --{-
                        0x22 -> (VQuote, bt)
                        0x5c -> (BSlash, bt)
                        0x2f -> (FSlash, bt)
                      --}
                      {-
                        Quote  -> (VQuote, bt)
                        Bslash -> (BSlash, bt)
                        Slash  -> (FSlash, bt)
                       -}
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

_invalidUnicodeError = error "encountered invalid unicode byte in query string"

mapClass :: ByteString -> [ParseClass]
mapClass b | B.null b = mempty
           | otherwise =
             let (c, b') = _class b
              in c : mapClass b'

-- | parseMatch : attempt to match against pre-classified query key,
--   skipping to end of current string if a non-match is found
parseMatch :: [ParseClass] -> A.Parser Bool
parseMatch [] = A.anyWord8 >>= \case
    0x22 -> pure True
--  Quote -> pure True
    _    -> False <$ skipToEndQ
parseMatch (x:xs) = _match x >>= \case
    True -> parseMatch xs
    False -> False <$ skipToEndQ
