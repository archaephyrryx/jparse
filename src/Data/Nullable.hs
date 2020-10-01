{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- | 'Nullable' type-class and instances.
--
-- Defines a method 'null' which shadows 'Prelude.null', meaning that either
-- "Prelude" should be explicitly imported @hiding (null)@, or this module should
-- be imported qualified.
module Data.Nullable where

-- | Zero-constraint type-class for concrete types with some notion of a
-- \"null\" or \"empty\" value and some method of determining whether an
-- arbitrary value is \"null\".
--
-- This is specifically useful for \'container\' types parametrized
-- over polymorphic types without 'Eq' constraints, such as '[]' or 'Maybe',
-- as typically such types derive
--
-- > instance Eq a => Eq (t a)
--
-- and cannot compare equality on their \'empty\' constructors without such
-- type constraints.
-- In particular, @instance Nullable [a]@ and @instance Nullable (Maybe a)@
-- are defined without @Eq a@ constraints to allow generic comparison to 'Nil'
-- and 'Nothing' as their respective 'null' values.
class Nullable w where 
  -- | Test against \"empty\" or \"null\" value
  isNull :: w -> Bool
  -- | \"empty\" or \"null\" value
  null   :: w

instance {-# OVERLAPPABLE #-} Nullable [a] where
  isNull [] = True
  isNull _  = False
  null = []

instance {-# OVERLAPPABLE #-} Nullable (Maybe a) where
  isNull Nothing = True
  isNull _       = False
  null = Nothing


instance {-# INCOHERENT #-} Foldable f => Nullable (f a) where
  isNull = foldr (\_ _ -> False) True
  null   = error "no generic null for Foldable"

instance {-# OVERLAPPING #-} (Eq a, Monoid a) => Nullable a where
  isNull = (==) mempty
  null   = mempty
