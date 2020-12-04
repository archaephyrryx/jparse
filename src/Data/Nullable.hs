{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-|
Module      : Data.Nullable
Description : Typeclass for partial equality against sentinel null-value
Copyright   : (c) Peter Duchovni, 2020
License     : BSD-3
Maintainer  : caufeminecraft+github@gmail.com

The 'Nullable' type-class defines an \'empty\' value of every instance type,
along with a predicate that determines whether a given value is \'empty\'.
This applies most naturally to \'container\' types, as well as types
that already have a 'Monoid' instance with a distinguished
(i.e. identifiable) value of 'mempty'.
-}
module Data.Nullable where

-- | Type-class for types with a distinguished \"null\" or
-- \"empty\" value and some method of determining whether a given
-- value is \"null\".
--
-- All instances of this class should have the property
--
-- prop> isNull nullValue = True
class Nullable w where
  -- | Test against \"empty\" or \"null\" value
  isNull    :: w -> Bool
  -- | \"empty\" or \"null\" value
  nullValue :: w

instance {-# OVERLAPPING #-} Nullable [a] where
  isNull [] = True
  isNull _  = False
  {-# INLINE isNull #-}
  nullValue = []
  {-# INLINE nullValue #-}

instance {-# OVERLAPPING #-} Nullable (Maybe a) where
  isNull Nothing = True
  isNull _       = False
  {-# INLINE isNull #-}
  nullValue = Nothing
  {-# INLINE nullValue #-}

instance {-# INCOHERENT #-} (Monoid (f a), Foldable f) => Nullable (f a) where
  isNull = foldr (\_ _ -> False) True
  {-# INLINE isNull #-}
  nullValue = mempty
  {-# INLINE nullValue #-}

instance {-# OVERLAPPABLE #-} (Eq a, Monoid a) => Nullable a where
  isNull = (==) mempty
  {-# INLINE isNull #-}
  nullValue = mempty
  {-# INLINE nullValue #-}
