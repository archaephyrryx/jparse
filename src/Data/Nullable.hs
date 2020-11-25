{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- | Declaration and instances for type-class 'Nullable', which associates an \'empty\' value to
-- a given type and provides a function for testing partial equality against this value. This applies
-- most naturally to \'container\' types, as well as types that already have a 'Monoid' instance with
-- a distinguished (i.e. identifiable) value of 'mempty'.
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
