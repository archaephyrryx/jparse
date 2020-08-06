{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Data.ByteString.Unsafe
#endif
-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}


-- Modified clone of Data.Attoparsec.Zepto for comparative benchmarking


module Parse.Parser.ZeptoStream
  (
    Parser
  , Result(..)
  , Zepto
  , fromState
  , parse
  , parseR
  , atEnd
  , word8
  , string
  , stringCI
  , take
  , skip
  , takeWhile
  , skipWhile
  , peek
  , pop
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Function (on)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Monoid as Mon (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

newtype S = S {
      input :: ByteString
    }

fromState :: S -> ByteString
fromState = input
{-# INLINE fromState #-}

data Result a = Fail String
              | Cont (ByteString -> Result a)
              | OK !a S

instance Functor Result where
    fmap f r@(OK a s) = OK (f a) s
    fmap f r@(Cont c) = Cont $ fmap f . c
    fmap f (Fail err) = Fail err
    {-# INLINE fmap #-}

-- | A simple parser.
--
-- This monad is strict in its state, and the monadic bind operator
-- ('>>=') evaluates each result to weak head normal form before
-- passing it along.
newtype Zepto a = Parser {
      runParser :: S -> Result a
    }

type Parser a = Zepto a

instance Functor Zepto where
    fmap f m = Parser $ \s -> fmap f $! runParser m s
    {-# INLINE fmap #-}

instance Monad Zepto where
    return = pure
    {-# INLINE return #-}

    m >>= k   = Parser $ \s ->
      let !result = runParser m s
       in case result of
           OK a s'  -> runParser (k a) s'
           Cont c -> Cont (go . c)
           Fail err -> Fail err
      where
        go !result = case result of
           OK a s'  -> runParser (k a) s'
           Cont c -> Cont (go . c)
           Fail err -> Fail err
    {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

instance Fail.MonadFail Zepto where
    fail msg = Parser $ \_ -> Fail msg
    {-# INLINE fail #-}

instance MonadPlus Zepto where
    mzero = fail "mzero"
    {-# INLINE mzero #-}

    mplus a b = Parser $ \s ->
      case runParser a s of
        ok@(OK _ _) -> ok
        Cont c -> (Cont (go s b . c))
        _      -> runParser b s
      where
        go s b = \case
          ok@(OK _ _) -> ok
          Cont c -> (Cont (go s b . c))
          _      -> runParser b s
    {-# INLINE mplus #-}

instance Applicative Zepto where
    pure a = Parser $ \s -> OK a s
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}

gets :: (S -> a) -> Zepto a
gets f = Parser $ \s -> OK (f s) s
{-# INLINE gets #-}

put :: S -> Zepto ()
put !s = Parser $ \_ -> OK () s
{-# INLINE put #-}

feed :: (ByteString -> Result a) -> Zepto a
feed cnt = Parser $ \_ -> Cont cnt
{-# INLINE feed #-}

-- | Run a parser and return raw result
parseR :: Parser a -> ByteString -> Result a
parseR p bs = runParser p (S bs)
{-# INLINE parseR #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Either String a
parse p bs = case runParser p (S bs) of
               (OK a _)   -> Right a
               (Cont c)   -> Left "input ended before parse"
               (Fail err) -> Left err
{-# INLINE parse #-}

instance Semigroup (Zepto a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Mon.Monoid (Zepto a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Alternative Zepto where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

-- | Consume input while the predicate returns 'True'.
takeWhile :: (Word8 -> Bool) -> Zepto ByteString
takeWhile p = do
  (!h,!t) <- gets (B.span p . input)
  if B.null t
     then
       let cnt b
             | B.null b = OK h (S B.empty)
             | otherwise = fmap (h <>) $ parseR (takeWhile p) b
        in feed cnt
     else put (S t) >> pure h
{-# INLINE takeWhile #-}

-- | Skip input while the predicate returns 'True'
skipWhile :: (Word8 -> Bool) -> Zepto ()
skipWhile p = do
  t <- gets (B.dropWhile p . input)
  if B.null t
     then let cnt b
                | B.null b = OK () (S B.empty)
                | otherwise = parseR (skipWhile p) b
           in feed cnt
     else put (S t)
{-# INLINE skipWhile #-}

-- | Consume @n@ bytes of input.
take :: Int -> Zepto ByteString
take !n = do
  s <- gets input
  let !l = B.length s
  if l >= n
    then put (S (B.unsafeDrop n s)) >> pure (B.unsafeTake n s)
    else let !m = n - l
             cnt b
               | B.null b = Fail "insufficient input"
               | otherwise = fmap (s <>) $ parseR (take m) b
          in feed cnt
{-# INLINE take #-}

-- | Skip @n@ bytes of input.
skip :: Int -> Zepto ()
skip !n = do
  s <- gets input
  let !l = B.length s
  if l >= n
     then put (S (B.unsafeDrop n s))
     else let !m = n - l
              cnt b
                | B.null b = Fail "insufficient input"
                | otherwise = parseR (skip m) b
           in feed cnt
{-# INLINE skip #-}

peek :: Zepto (Maybe Word8)
peek = do
  s <- gets input
  if not $! B.null s
     then pure (Just $! B.unsafeHead s)
     else let cnt b
                | B.null b = OK Nothing (S B.empty)
                | otherwise = parseR peek b
           in feed cnt
{-# INLINE peek #-}

pop :: Zepto Word8
pop = do
  s <- gets input
  if not $! B.null s
     then put (S (B.unsafeTail s)) >> pure (B.unsafeHead s)
     else let cnt b
                | B.null b = Fail "insufficient input"
                | otherwise = parseR pop b
           in feed cnt
{-# INLINE pop #-}

word8 :: Word8 -> Zepto ()
word8 w = do
  i <- gets input
  if not $! B.null i
    then if w == B.unsafeHead i
      then put (S (B.unsafeTail i))
      else fail "word8"
    else let cnt b
               | B.null b = Fail "insufficient input"
               | otherwise = parseR (word8 w) b
          in feed cnt
{-# INLINE word8 #-}

-- | Match a string exactly.
string :: ByteString -> Zepto ()
string s = do
  i <- gets input
  case s `commonPrefix` i of
    (True, r) | B.null r -> put (S (B.unsafeDrop (B.length s) i))
              | otherwise ->
                let cnt b
                      | B.null b = Fail "insufficient input"
                      | otherwise = parseR (string r) b
                 in feed cnt
    _ -> fail "string"
{-# INLINE string #-}

-- | returns a tuple whose first element is True iff the shorter of
-- two bytestrings is a prefix of the longer, and whose second
-- element is the remainder of the first bytestring after a common
-- prefix has been stripped (if such a prefix exists)
commonPrefix :: ByteString -> ByteString -> (Bool, ByteString)
commonPrefix b1 b2 =
  ( and $ B.zipWith (==) b1 b2
  , B.drop (B.length b2) b1
  )
{-# INLINE commonPrefix #-}

-- | Match a string case-insensitively.
stringCI :: ByteString -> Zepto ()
stringCI s = do
  i <- gets input
  case s `lowEq` i of
    (True, r)
      | B.null r -> put (S (B.unsafeDrop (B.length s) i))
      | otherwise -> let cnt b
                           | B.null b = Fail "insufficient input"
                           | otherwise = parseR (stringCI r) b
                      in feed cnt
    _ -> fail "stringCI"
{-# INLINE stringCI #-}


-- | returns a tuple whose first element is True iff the shorter of
-- two bytestrings is a prefix of the longer, and whose second
-- element is the remainder of the first bytestring after a common
-- prefix has been stripped (if such a prefix exists) [Case Insensitive]
lowEq :: ByteString -> ByteString -> (Bool, ByteString)
lowEq b1 b2 =
  ( and $ B.zipWith ((==) `on` toLower) b1 b2
  , B.drop (B.length b2) b1
  )
  where
    toLower :: Word8 -> Word8
    toLower w | w >= 65 && w <= 90 = w + 32
              | otherwise          = w
{-# INLINE lowEq #-}

-- | Indicate whether the end of the input has been reached.
atEnd :: Zepto Bool
atEnd = do
  i <- gets input
  pure $! B.null i
{-# INLINE atEnd #-}
