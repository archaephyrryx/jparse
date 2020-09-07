module Helper
  ( if_
  , fi
  , cond
  , elseId
  ) where


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