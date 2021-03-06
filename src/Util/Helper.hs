-- | Assorted helper functions that depend only on base
-- and are non-application-specific.
module Util.Helper
  ( if_
  , fi
  , cond
  , elseId
  , refold
  , doJust
  ) where

import Control.Monad (void)

-- | Inlined if-then-else
if_ :: Bool -- ^ predicate value
    -> a -- ^ value if predicate is 'True'
    -> a -- ^ value if predicate is 'False'
    -> a
if_ p x y = if p then x else y
{-# INLINE if_ #-}

-- | Variant of 'if_' that takes True-branch and False-branch arguments before predicate value.
fi :: a -- ^ value if predicate is 'True'
   -> a -- ^ value if predicate is 'False'
   -> Bool -- ^ predicate value
   -> a
fi x y p = if_ p x y
{-# INLINE fi #-}

-- | Functional 'if_' using predicate and branch functions rather than constant values
cond :: (a -> Bool) -- ^ predicate function
     -> (a -> b) -- ^ function to apply if predicate holds
     -> (a -> b) -- ^ function to apply if predicate does not hold
     -> (a -> b)
cond p f g x = if p x then f x else g x
{-# INLINE cond #-}

-- | Conditionally applies an endomorphism if a predicate value is 'True', or 'id' otherwise
elseId :: Bool -> (a -> a) -> a -> a
elseId True = ($)
elseId _    = const id
{-# INLINE elseId #-}

-- | Apply a right-fold to values iteratively generated from an initial seed
refold :: (b -> Maybe (a, b)) -- ^ generator of next value and new seed
       -> (a -> c -> c) -- ^ accumulator function
       -> c -- ^ initial accumulator value
       -> b -- ^ initial seed
       -> c
refold g f z = go
  where
    go x = case g x of
      Nothing -> z
      Just (a, x') -> f a $ go x'
{-# INLINE refold #-}

-- | 'mapM_' specialized for 'Maybe'
--
-- While it is not strictly necessary to define this function, it offers more
-- transparent semantics than inlining a call to @mapM_@ over the Maybe monad
doJust :: Monad m => (a -> m b) -> Maybe a -> m ()
doJust = mapM_
{-# INLINE doJust #-}
