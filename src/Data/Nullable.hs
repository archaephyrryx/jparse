{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Data.Nullable where

class Nullable w where 
  isNull :: w -> Bool
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
