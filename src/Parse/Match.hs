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

-- | (attempt to) consume an input 'Quad' case-insensitively and return success result
_quad :: Quad -> A.Parser Res
_quad = mark . A.stringCI
{-# INLINE _quad #-}

-- ** Low-level parser primitives for abstract character-sequences represented by 'ParseClass' values

-- | Alias for a parser whose output value is a result indicating a successful or failed match
--
-- Each @Matcher@ defined in this module is implicitly a Parser that returns success if it was able
-- to consume some 'Word8'-sequence that is a valid JSON-string internal encoding of a specific
-- ASCII or UTF-16 entity corresponding to a particular 'ParseClass'-typed value. Each 'ParseClass'
-- constructor has its own respective @Matcher@, which is explicitly named in per-function documentation
-- when not implied by naming convention.
type Matcher = A.Parser Res

-- | _match : generically match against any 'ParseClass' by associating each constructor with its respective @Matcher@
_match :: ParseClass -> Matcher
_match BSlash         = _bslash
_match VQuote         = _vquote
_match FSlash         = _fslash
_match (Direct  w q)  = _ascii w q
_match (Control w q)  = _ctr w q
_match (BMP  v q)     = _char v q
_match (Surr v q)     = _surr v q

-- | @Matcher@ that accepts all valid representations of a backslash character
_bslash :: Matcher
_bslash = A.anyWord8 >>= \case
            Bslash -> A.anyWord8 >>= \case
                Bslash -> pass -- \\
                Hex_u -> _quad "005c" -- \u005c
                _    -> fail
            _    -> fail
{-# INLINE _bslash #-}


-- | @Matcher@ that accepts all valid representations of a double-quote character
--
-- As the hexadecimal encoding of @\"@ (@0x22@) is inherently case-insensitive, it is
-- matched using the regular 'A.string' function rather than '_quad', which uses
-- 'A.stringCI' internally.
_vquote :: Matcher
_vquote = A.anyWord8 >>= \case
            Bslash -> A.anyWord8 >>= \case
                Quote -> pass -- \"
                Hex_u -> mark $ A.string "0022" -- \u0022
                _    -> fail
            _    -> fail
{-# INLINE _vquote #-}

-- | @Matcher@ that accepts all valid representations of a forward-slash character
--
-- While this character has no inherent significance that would normally merit a separate
-- parser, it is unique in that it can be encoded in JSON both by its ASCII representation (@\/@)
-- and by its backslash-escaped ASCII representation (@\\\/@), which is true for no other character
_fslash :: Matcher
_fslash = A.anyWord8 >>= \case
            Slash  -> pass
            Bslash -> A.anyWord8 >>= \case
                Slash -> pass
                Hex_u -> _quad "002f" -- \u002f
                _    -> fail
            _    -> fail
{-# INLINE _fslash #-}


-- | @Matcher@ that accepts all valid representations of a directly-representable ASCII character
_ascii :: Word8 -> Quad -> Matcher
_ascii w q = A.anyWord8 >>= \case
            w' | w' == w -> pass
            Bslash -> A.word8 Hex_u >> _quad q
            _      -> fail
{-# INLINE _ascii #-}

-- | @Matcher@ that accepts all valid representations of an ASCII control character
_ctr :: Word8 -> Quad -> Matcher
_ctr w q = A.anyWord8 >>= \case
            Slash -> A.anyWord8 >>= \case
                w' | w `escapesTo` w' -> pass
                Hex_u -> _quad q
                _  -> fail
            _    -> fail
{-# INLINE _ctr #-}

-- | @Matcher@ that accepts all valid representations of UTF-16 characters within the BMP
_char :: UnconBS -> Quad -> Matcher
_char ~(w,t) q
    = A.anyWord8 >>= \case
        x | x == w -> mark $ A.string t
        Bslash -> A.anyWord8 >>= \case
            Hex_u -> _quad q
            _    -> fail
        _    -> fail

-- | @Matcher@ that accepts all valid representations of UTF-16 surrogate pairs
_surr :: DeconBS -> QuadPair -> Matcher
_surr ~(w,t) (h,l)
    = A.anyWord8 >>= \case
        x | x == w -> mark $ mapM_ A.word8 t
        Bslash -> A.anyWord8 >>= \case
            Hex_u -> _qquad h l
            _    -> fail
        _    -> fail
    where
        _qquad :: Quad -> Quad -> A.Parser Res
        _qquad h l = do { _quad h; A.word8 Bslash; A.word8 Hex_u; _quad l }
        {-# INLINE _qquad #-}

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

_invalidUnicodeError = error "encountered invalid unicode byte in query string"

-- | Maps every codepoint in a ByteString to a corresponding 'ParseClass' value
mapClass :: ByteString -> [ParseClass]
mapClass b | B.null b = []
           | otherwise =
             let (c, b') = _class b
              in c : mapClass b'

-- | parseMatch : attempt to match against pre-classified query key,
--   skipping to end of current string if a non-match is found
parseMatch :: [ParseClass] -> A.Parser Bool
parseMatch [] = A.anyWord8 >>= \case
    Quote -> pure True
    _     -> False <$ skipToEndQ
parseMatch (x:xs) = _match x >>= \case
    True -> parseMatch xs
    False -> False <$ skipToEndQ
