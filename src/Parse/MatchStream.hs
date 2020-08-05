{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Specialized parser directives for matching all possible representations
-- of JSON-encoded string values.
--
-- Compatible with UTF-16 BMP characters and surrogates pairs.
module Parse.MatchStream
  (mapClass, parseMatch, ParseClass)
  where

import           Prelude hiding (fail)
import           Data.Word (Word8)

import Parse.Match.Internal

import qualified Parse.Parser.ZeptoStream as P

import Parse.ReadStream (skipToEndQ)
import Parse.Symbol

-- | (attempt to) consume an input 'Quad' case-insensitively and return success result
_quad :: Monad m => Quad -> P.Parser m Res
_quad = mark . P.stringCI
{-# INLINE _quad #-}

-- ** Low-level parser primitives for abstract character-sequences represented by 'ParseClass' values

-- | Alias for a parser whose output value is a result indicating a successful or failed match
--
-- Each @Matcher@ defined in this module is implicitly a Parser that returns success if it was able
-- to consume some 'Word8'-sequence that is a valid JSON-string internal encoding of a specific
-- ASCII or UTF-16 entity corresponding to a particular 'ParseClass'-typed value. Each 'ParseClass'
-- constructor has its own respective @Matcher@, which is explicitly named in per-function documentation
-- when not implied by naming convention.
type Matcher m = P.Parser m Res

-- | _match : generically match against any 'ParseClass' by associating each constructor with its respective @Matcher@
_match :: Monad m => ParseClass -> Matcher m
_match BSlash         = _bslash
_match VQuote         = _vquote
_match FSlash         = _fslash
_match (Direct  w q)  = _ascii w q
_match (Control w q)  = _ctr w q
_match (BMP  v q)     = _char v q
_match (Surr v q)     = _surr v q

-- | @Matcher@ that accepts all valid representations of a backslash character
_bslash :: Monad m => Matcher m
_bslash = P.pop >>= \case
            Bslash -> P.pop >>= \case
                Bslash -> pass -- \\
                Hex_u -> _quad "005c" -- \u005c
                _    -> fail
            _    -> fail
{-# INLINE _bslash #-}


-- | @Matcher@ that accepts all valid representations of a double-quote character
--
-- As the hexadecimal encoding of @\"@ (@0x22@) is inherently case-insensitive, it is
-- matched using the regular 'P.string' function rather than '_quad', which uses
-- 'P.stringCI' internally.
_vquote :: Monad m => Matcher m
_vquote = P.pop >>= \case
            Bslash -> P.pop >>= \case
                Quote -> pass -- \"
                Hex_u -> mark $ P.string "0022" -- \u0022
                _    -> fail
            _    -> fail
{-# INLINE _vquote #-}

-- | @Matcher@ that accepts all valid representations of a forward-slash character
--
-- While this character has no inherent significance that would normally merit a separate
-- parser, it is unique in that it can be encoded in JSON both by its ASCII representation (@\/@)
-- and by its backslash-escaped ASCII representation (@\\\/@), which is true for no other character
_fslash :: Monad m => Matcher m
_fslash = P.pop >>= \case
            Slash  -> pass
            Bslash -> P.pop >>= \case
                Slash -> pass
                Hex_u -> _quad "002f" -- \u002f
                _    -> fail
            _    -> fail
{-# INLINE _fslash #-}


-- | @Matcher@ that accepts all valid representations of a directly-representable ASCII character
_ascii :: Monad m => Word8 -> Quad -> Matcher m
_ascii w q = P.pop >>= \case
            w' | w' == w -> pass
            Bslash -> P.word8 Hex_u >> _quad q
            _      -> fail
{-# INLINE _ascii #-}

-- | @Matcher@ that accepts all valid representations of an ASCII control character
_ctr :: Monad m => Word8 -> Quad -> Matcher m
_ctr w q = P.pop >>= \case
            Slash -> P.pop >>= \case
                w' | w `escapesTo` w' -> pass
                Hex_u -> _quad q
                _  -> fail
            _    -> fail
{-# INLINE _ctr #-}

-- | @Matcher@ that accepts all valid representations of UTF-16 characters within the BMP
_char :: Monad m => UnconBS -> Quad -> Matcher m
_char ~(w,t) q
    = P.pop >>= \case
        x | x == w -> mark $ P.string t
        Bslash -> P.pop >>= \case
            Hex_u -> _quad q
            _    -> fail
        _    -> fail

-- | @Matcher@ that accepts all valid representations of UTF-16 surrogate pairs
_surr :: Monad m => DeconBS -> QuadPair -> Matcher m
_surr ~(w,t) (h,l)
    = P.pop >>= \case
        x | x == w -> mark $ mapM_ P.word8 t
        Bslash -> P.pop >>= \case
            Hex_u -> _qquad h l
            _    -> fail
        _    -> fail
    where
        -- _qquad :: Quad -> Quad -> P.Parser m Res
        _qquad h l = do { _quad h; P.word8 Bslash; P.word8 Hex_u; _quad l }
        {-# INLINE _qquad #-}

-- | parseMatch : attempt to match against pre-classified query key,
--   skipping to end of current string if a non-match is found
parseMatch :: Monad m => [ParseClass] -> P.Parser m Bool
parseMatch [] = P.pop >>= \case
    Quote -> pure True
    _     -> False <$ skipToEndQ
parseMatch (x:xs) = _match x >>= \case
    True -> parseMatch xs
    False -> False <$ skipToEndQ
