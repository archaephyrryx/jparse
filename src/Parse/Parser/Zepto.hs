{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Data.ByteString.Unsafe
#endif
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}


-- | Heavily modified variant of "Data.Attoparsec.Zepto" optimized for performance,
--   including a number of additional combinators and reimplementation of the 'Parser' type.
module Parse.Parser.Zepto
  ( Parser
  , Result(..)
  , parseR
  , parse
  , atEnd
  , word8
  , string
  , stringCI
  , take
  , takeWhile
  , skip
  , skipWhile
  , skipEndQuote
  , peek
  , peekAll
  , pop
  , popAll
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import qualified Control.Monad.Fail as Fail
import Data.ByteString (ByteString)
import Data.ByteString.Seek (skipString)
import Data.Monoid as Mon (Monoid(..))
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

newtype S = S { input :: ByteString }

-- | Data type representing the outcome of running a parser, which is either
--  a briefly explained failure,
--  or a successful parse of a (strict) value and the leftover parser state.
data Result a = Fail String
              | OK !a S

-- | A simple parser.
--
-- This monad is strict in its state, and the monadic bind operator
-- ('>>=') evaluates each result to weak head normal form before
-- passing it along.
--
-- As there is no possibility of retrying any parser with additional input,
-- virtually all parser-combinators will fail if there is insufficient input
-- to complete a parse. This behavior is therefore implicitly noted for all
-- combinators defined in this module, unless otherwise specified.
newtype Parser a = Parser { runParser :: S -> Result a }

instance Functor Parser where
  fmap f m = Parser $ \s ->
    case runParser m s of
      OK a s'  -> OK (f a) s'
      Fail err -> Fail err
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser $ OK a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}
  m >>= k   = Parser $ \s ->
    case runParser m s of
      OK a s'  -> runParser (k a) s'
      Fail err -> Fail err
  {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
  fail = Fail
  {-# INLINE fail #-}
#endif

instance Fail.MonadFail Parser where
  fail msg = Parser $ \_ -> Fail msg
  {-# INLINE fail #-}

instance MonadPlus Parser where
  mzero = fail "mzero"
  {-# INLINE mzero #-}

  mplus a b = Parser $ \s ->
    case runParser a s of
      ok@(OK _ _) -> ok
      _           -> runParser b s
  {-# INLINE mplus #-}

gets :: (S -> a) -> Parser a
gets f = Parser $ \s -> OK (f s) s
{-# INLINE gets #-}

put :: S -> Parser ()
put s = Parser $ \_ -> OK () s
{-# INLINE put #-}

-- | Run a parser and return raw result
parseR :: Parser a -> ByteString -> Result a
parseR p bs = runParser p (S bs)
{-# INLINE parseR #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Either String a
parse p bs = case runParser p (S bs) of
               (OK a _)   -> Right a
               (Fail err) -> Left err
{-# INLINE parse #-}

instance Semigroup (Parser a) where
  (<>) = mplus
  {-# INLINE (<>) #-}

instance Mon.Monoid (Parser a) where
  mempty  = fail "mempty"
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance Alternative Parser where
  empty = fail "empty"
  {-# INLINE empty #-}
  (<|>) = mplus
  {-# INLINE (<|>) #-}

-- | Consume input while the predicate returns 'True'.
--
-- Terminates without failing if reached end of input.
takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile p = do
  (h,t) <- gets (B.span p . input)
  put (S t)
  pure h
{-# INLINE takeWhile #-}

-- | Skip input while the predicate returns 'True'
--
-- Terminates without failing if reached end of input.
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile p = do
  t <- gets (B.dropWhile p . input)
  put (S t)
{-# INLINE skipWhile #-}

-- | Consume @n@ bytes of input.
take :: Int -> Parser ByteString
take !n = do
  s <- gets input
  if B.length s >= n
    then put (S (B.unsafeDrop n s)) >> pure (B.unsafeTake n s)
    else fail "insufficient input"
{-# INLINE take #-}

-- | Skip @n@ bytes of input.
skip :: Int -> Parser ()
skip !n = do
  s <- gets input
  if B.length s >= n
    then put (S (B.unsafeDrop n s))
    else fail "insufficient input"
{-# INLINE skip #-}

-- | Return next byte without modifying state.
--
-- Returns 'Nothing' if at end of input.
peek :: Parser (Maybe Word8)
peek = do
  s <- gets input
  if not $! B.null s
     then pure (Just $ B.unsafeHead s)
     else pure Nothing
{-# INLINE peek #-}

-- | Consume and return next byte.
pop :: Parser Word8
pop = do
  s <- gets input
  if not $! B.null s
     then put (S (B.unsafeTail s)) >> pure (B.unsafeHead s)
     else fail "insufficient input"
{-# INLINE pop #-}

-- | Match a single byte exactly.
word8 :: Word8 -> Parser ()
word8 w = do
  i <- gets input
  if not $! B.null i
    then if w == B.unsafeHead i
      then put (S (B.unsafeTail i))
      else fail "word8"
    else fail "insufficient input"
{-# INLINE word8 #-}

-- | Match a string exactly.
string :: ByteString -> Parser ()
string s = do
  i <- gets input
  if s `B.isPrefixOf` i
    then put (S (B.unsafeDrop (B.length s) i))
    else fail "string"
{-# INLINE string #-}

-- | Match a string case-insensitively.
stringCI :: ByteString -> Parser ()
stringCI s = do
  i <- gets input
  let !n = B.length s
  if n <= B.length i
    then if s `lowEq` B.unsafeTake n i
      then put (S (B.unsafeDrop n i))
      else fail "stringCI"
    else fail "insufficient input"
{-# INLINE stringCI #-}

-- b1 and b2 must be of same non-zero length
lowEq :: ByteString -> ByteString -> Bool
lowEq b1 b2 = B.map toLower b1 == B.map toLower b2
  where
    toLower :: Word8 -> Word8
    toLower w | w >= 65 && w <= 90 = w + 32
              | otherwise          = w
{-# INLINE lowEq #-}

-- | Skips the \'payload\' and end-quote of a @"@-delimited string, requiring that
-- the leading double-quote has already been consumed.
skipEndQuote :: Parser ()
skipEndQuote = do
  i <- gets input
  case skipString i of
    Nothing -> fail "skipEndQuote"
    Just !s -> put $ S s
{-# INLINE skipEndQuote #-}

-- | Returns entirety of (remaining) input as a strict 'ByteString' without consuming
peekAll :: Parser ByteString
peekAll = gets input
{-# INLINE peekAll #-}

-- | Consumes and returns entirety of (remaining) input as a strict 'ByteString'
popAll :: Parser ByteString
popAll = gets input <* put (S B.empty)
{-# INLINE popAll #-}

-- | Indicate whether the end of the input has been reached.
atEnd :: Parser Bool
atEnd = do
  i <- gets input
  pure $! B.null i
{-# INLINE atEnd #-}
