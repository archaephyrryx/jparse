{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Specialized parser directives for matching all possible representations
-- of JSON-encoded string values.
--
-- Compatible with UTF-16 BMP characters and surrogates pairs.
module Parse.Match
  (mapClass, parseMatch, parseMatchAlt, ParseClass)
  where

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

import Parse.Match.Internal

import Parse.Read (skipToEndQ)
import qualified Parse.ReadAlt as R (skipToEndQ)
import Parse.Symbol

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

-- | parseMatch : attempt to match against pre-classified query key,
--   skipping to end of current string if a non-match is found
parseMatch :: [ParseClass] -> A.Parser Bool
parseMatch [] = A.anyWord8 >>= \case
    Quote -> pure True
    _     -> False <$ skipToEndQ
parseMatch (x:xs) = _match x >>= \case
    True -> parseMatch xs
    False -> False <$ skipToEndQ

-- | parseMatch : attempt to match against pre-classified query key,
--   skipping to end of current string if a non-match is found
parseMatchAlt :: [ParseClass] -> A.Parser Bool
parseMatchAlt [] = A.anyWord8 >>= \case
    Quote -> pure True
    _     -> False <$ R.skipToEndQ
parseMatchAlt (x:xs) = _match x >>= \case
    True -> parseMatchAlt xs
    False -> False <$ R.skipToEndQ
