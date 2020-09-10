module Bundle where

import Data.Vector (Vector, cons, snoc)
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as L

import Helper (refold)

data Bundle a
  = VectorOf (Vector a)
  | SingleOf a
  | MemptyOf

instance Semigroup a => Semigroup (Bundle a) where
  x            <> MemptyOf     = x
  MemptyOf     <> y            = y
  (VectorOf v) <> (VectorOf w) = VectorOf (v<>w)
  (SingleOf x) <> (SingleOf y) = SingleOf (x<>y)
  (VectorOf v) <> (SingleOf y) = VectorOf (v`snoc`y)
  (SingleOf x) <> (VectorOf w) = VectorOf (x`cons`w)

instance Semigroup a => Monoid (Bundle a) where
  mempty = MemptyOf

foldrBundle :: (a -> b -> b) -> b -> Bundle a -> b
foldrBundle f z b = case b of
  VectorOf v -> V.foldr f z v
  SingleOf x -> f x z
  MemptyOf   -> z
{-# INLINE foldrBundle #-}

refoldBundle :: (a -> Maybe (a, a)) -> (a -> b -> b) -> b -> Bundle a -> b
refoldBundle g f z b = case b of
  VectorOf v -> V.foldr f z v
  SingleOf x -> refold g f z x
  MemptyOf   -> z
{-# INLINE refoldBundle #-}

nullBundle :: Bundle L.ByteString -> Bool
nullBundle (VectorOf v) = V.null v
nullBundle (SingleOf x) = L.null x
nullBundle _            = True
{-# INLINE nullBundle #-}