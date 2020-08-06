{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Data.ByteString.Unsafe
#endif
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}


-- Modified clone of Data.Attoparsec.Zepto for comparative benchmarking


module Parse.Parser.Zepto
  (
    Parser
  , Result
  , ZeptoT
  , parse
  , parseT
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

data Result a = Fail String
              | OK !a S

-- | A simple parser.
--
-- This monad is strict in its state, and the monadic bind operator
-- ('>>=') evaluates each result to weak head normal form before
-- passing it along.
newtype ZeptoT m a = Parser {
      runParser :: S -> m (Result a)
    }

type Parser a = ZeptoT Identity a

instance Monad m => Functor (ZeptoT m) where
    fmap f m = Parser $ \s -> do
      result <- runParser m s
      case result of
        OK a s'  -> return (OK (f a) s')
        Fail err -> return (Fail err)
    {-# INLINE fmap #-}

instance MonadIO m => MonadIO (ZeptoT m) where
  liftIO act = Parser $ \s -> do
    result <- liftIO act
    return (OK result s)
  {-# INLINE liftIO #-}

instance Monad m => Monad (ZeptoT m) where
    return = pure
    {-# INLINE return #-}

    m >>= k   = Parser $ \s -> do
      result <- runParser m s
      case result of
        OK a s'  -> runParser (k a) s'
        Fail err -> return (Fail err)
    {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

instance Monad m => Fail.MonadFail (ZeptoT m) where
    fail msg = Parser $ \_ -> return (Fail msg)
    {-# INLINE fail #-}

instance Monad m => MonadPlus (ZeptoT m) where
    mzero = fail "mzero"
    {-# INLINE mzero #-}

    mplus a b = Parser $ \s -> do
      result <- runParser a s
      case result of
        ok@(OK _ _) -> return ok
        _           -> runParser b s
    {-# INLINE mplus #-}

instance (Monad m) => Applicative (ZeptoT m) where
    pure a = Parser $ \s -> return (OK a s)
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}

gets :: Monad m => (S -> a) -> ZeptoT m a
gets f = Parser $ \s -> return (OK (f s) s)
{-# INLINE gets #-}

put :: Monad m => S -> ZeptoT m ()
put s = Parser $ \_ -> return (OK () s)
{-# INLINE put #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Either String a
parse p bs = case runIdentity (runParser p (S bs)) of
               (OK a _)   -> Right a
               (Fail err) -> Left err
{-# INLINE parse #-}

-- | Run a parser on top of the given base monad.
parseT :: Monad m => ZeptoT m a -> ByteString -> m (Either String a)
parseT p bs = do
  result <- runParser p (S bs)
  case result of
    OK a _   -> return (Right a)
    Fail err -> return (Left err)
{-# INLINE parseT #-}

instance Monad m => Semigroup (ZeptoT m a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monad m => Mon.Monoid (ZeptoT m a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Monad m => Alternative (ZeptoT m) where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

-- | Consume input while the predicate returns 'True'.
takeWhile :: Monad m => (Word8 -> Bool) -> ZeptoT m ByteString
takeWhile p = do
  (h,t) <- gets (B.span p . input)
  put (S t)
  pure h
{-# INLINE takeWhile #-}

-- | Skip input while the predicate returns 'True'
skipWhile :: Monad m => (Word8 -> Bool) -> ZeptoT m ()
skipWhile p = do
  t <- gets (B.dropWhile p . input)
  put (S t)
{-# INLINE skipWhile #-}

-- | Consume @n@ bytes of input.
take :: Monad m => Int -> ZeptoT m ByteString
take !n = do
  s <- gets input
  if B.length s >= n
    then put (S (B.unsafeDrop n s)) >> pure (B.unsafeTake n s)
    else fail "insufficient input"
{-# INLINE take #-}

-- | Skip @n@ bytes of input.
skip :: Monad m => Int -> ZeptoT m ()
skip !n = do
  s <- gets input
  if B.length s >= n
    then put (S (B.unsafeDrop n s))
    else fail "insufficient input"
{-# INLINE skip #-}

peek :: Monad m => ZeptoT m (Maybe Word8)
peek = do
  s <- gets input
  if not $! B.null s
     then pure (Just $ B.unsafeHead s)
     else pure Nothing
{-# INLINE peek #-}

pop :: Monad m => ZeptoT m Word8
pop = do
  s <- gets input
  if not $! B.null s
     then put (S (B.unsafeTail s)) >> pure (B.unsafeHead s)
     else fail "insufficient input"
{-# INLINE pop #-}


word8 :: Monad m => Word8 -> ZeptoT m ()
word8 w = do
  i <- gets input
  if not $! B.null i
    then if w == B.unsafeHead i
      then put (S (B.unsafeTail i))
      else fail "word8"
    else fail "insufficient input"
{-# INLINE word8 #-}

-- | Match a string exactly.
string :: Monad m => ByteString -> ZeptoT m ()
string s = do
  i <- gets input
  if s `B.isPrefixOf` i
    then put (S (B.unsafeDrop (B.length s) i))
    else fail "string"
{-# INLINE string #-}

-- | Match a string case-insensitively.
stringCI :: Monad m => ByteString -> ZeptoT m ()
stringCI s = do
  i <- gets input
  let n = B.length s
  if n <= B.length i
    then if s `lowEq` (B.unsafeTake n i)
      then put (S (B.unsafeDrop (B.length s) i))
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

-- | Indicate whether the end of the input has been reached.
atEnd :: Monad m => ZeptoT m Bool
atEnd = do
  i <- gets input
  pure $! B.null i
{-# INLINE atEnd #-}
