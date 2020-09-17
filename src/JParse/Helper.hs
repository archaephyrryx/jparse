module JParse.Helper
  ( if_
  , fi
  , cond
  , elseId
  , refold
  , doJust
  ) where

import Control.Monad (void)

if_ :: Bool -> a -> a -> a
if_ p x y = if p then x else y
{-# INLINE if_ #-}

fi :: a -> a -> Bool -> a
fi x y p = if_ p x y
{-# INLINE fi #-}

cond :: (a -> Bool) -> (a -> b) -> (a -> b) -> (a -> b)
cond p f g x = if p x then f x else g x
{-# INLINE cond #-}

elseId :: Bool -> (a -> a) -> a -> a
elseId True  f x = f x
elseId False _ x = x
{-# INLINE elseId #-}

refold :: (b -> Maybe (a, b)) -> (a -> c -> c) -> c -> b -> c
refold g f z = go
  where
    go x = case g x of
      Nothing -> z
      Just (a, x') -> f a $ go x'
{-# INLINE refold #-}

doJust :: Monad m => (a -> m b) -> Maybe a -> m ()
doJust f (Just x) = void $ f x
doJust _ _ = return ()
{-# INLINE doJust #-}
{-# SPECIALIZE INLINE doJust :: Monad m => (a -> m ()) -> Maybe a -> m () #-}
