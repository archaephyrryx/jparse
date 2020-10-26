{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- | 'Nullable' type-class and instances.
--
-- Defines a method 'null' which shadows the Prelude export 'Prelude.null', meaning that either
-- "Prelude" should be explicitly imported @hiding (null)@, or this module should
-- be imported qualified.
module Data.Nullable where

-- | Minimal type-class for types with a distinguished
-- \"null\" or \"empty\" value and some method of determining whether a given
-- value is \"null\".
--
-- All instances of this class should have the property
--
-- prop> isNull null = True
class Nullable w where
  -- | Test against \"empty\" or \"null\" value
  isNull :: w -> Bool
  -- | \"empty\" or \"null\" value
  null   :: w

instance {-# OVERLAPPING #-} Nullable [a] where
  isNull [] = True
  isNull _  = False
  {-# INLINE isNull #-}
  null = []
  {-# INLINE null #-}

instance {-# OVERLAPPING #-} Nullable (Maybe a) where
  isNull Nothing = True
  isNull _       = False
  {-# INLINE isNull #-}
  null = Nothing
  {-# INLINE null #-}

instance {-# INCOHERENT #-} (Monoid (f a), Foldable f) => Nullable (f a) where
  isNull = foldr (\_ _ -> False) True
  {-# INLINE isNull #-}
  null   = mempty
  {-# INLINE null #-}

instance {-# OVERLAPPABLE #-} (Eq a, Monoid a) => Nullable a where
  isNull = (==) mempty
  {-# INLINE isNull #-}
  null   = mempty
  {-# INLINE null #-}
