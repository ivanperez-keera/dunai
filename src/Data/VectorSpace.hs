{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE UndecidableInstances   #-}
-- |
-- Module      :  Data.VectorSpace
-- Copyright   :  (c) Ivan Perez and Manuel Bärenz
-- License     :  See the LICENSE file in the distribution.
--
-- Maintainer  :  ivan.perez@keera.co.uk
-- Stability   :  provisional
-- Portability :  non-portable (GHC extensions)
--
-- Vector space type relation and basic instances.
-- Heavily inspired by Yampa's @FRP.Yampa.VectorSpace@ module.

module Data.VectorSpace where

------------------------------------------------------------------------------
-- Vector space type relation
------------------------------------------------------------------------------

infixr 6 *^
infixl 6 ^/
infix 6 `dot`
infixl 5 ^+^, ^-^

-- TODO Add laws this should satisfy


-- | A module @v@ over a ring @Groundring v@
--   is an abelian group with a linear multiplication.
--   The hat @^@ denotes the side of an operation
--   on which the vector stands,
--   i.e. @a *^ v@ for @v@ a vector.
--
--   The following laws must be satisfied:
--
--   * @v1 ^+^ v2 == v2 ^+^ v1@
--   * @a *^ zeroVector == zeroVector@
--   * @a *^ (v1 ^+^ v2) == a *^ v1 ^+^ a*^ v2
--   * @a *^ v == v ^* a@
--   * @negateVector v == (-1) *^ v@
--   * @v1 ^-^ v2 == v1 ^+^ negateVector v2@
class Num (Groundring v) => RModule v where
    type Groundring v
    zeroVector   :: v

    (*^)         :: Groundring v -> v -> v
    (*^) = flip (^*)

    (^*)         :: v -> Groundring v -> v
    (^*) = flip (*^)

    negateVector :: v -> v
    negateVector v = (-1) *^ v

    (^+^)        :: v -> v -> v

    (^-^)        :: v -> v -> v
    v1 ^-^ v2     = v1 ^+^ negateVector v2


-- Maybe norm and normalize should not be class methods, in which case
-- the constraint on the coefficient space (a) should (or, at least, could)
-- be Fractional (roughly a Field) rather than Floating.

-- Minimal instance: zeroVector, (*^), (^+^), dot
-- class Fractional (Groundfield v) => VectorSpace v where

-- | A vector space is a module over a field,
--   i.e. a commutative ring with inverses.
--
--   It needs to satisfy the axiom
--   @v ^/ a == (1/a) *^ v@,
--   which is the default implementation.
class (Fractional (Groundring v), RModule v) => VectorSpace v where
    (^/) :: v -> Groundfield v -> v
    v ^/ a = (1/a) *^ v

-- TODO Why is this not a type synonym?
type family Groundfield v :: *
type instance Groundfield v = Groundring v

-- | An inner product space is a module with an inner product,
--   i.e. a map @dot@ satisfying
--
--   * @v1 `dot` v2 == v2 `dot` v1@
--   * @(v1 ^+^ v2) `dot` v3 == v1 `dot` v3 ^+^ v2 `dot` v3@
--   * @(a *^ v1) `dot` v2 == a *^ v1 `dot` v2@
class RModule v => InnerProductSpace v where
  dot :: v -> v -> Groundfield v

-- | A normed space is a module with a norm,
--   i.e. a function @norm@ satisfying
--
--   * @norm (a ^* v) = a ^* norm v@
--   * @norm (v1 ^+^ v2) <= norm v1 ^+^ norm v2@
--     (the "triangle inequality")
--
--   A typical example is @sqrt (v `dot` v)@,
--   for an inner product space.
class RModule v => NormedSpace v  where
  norm :: v -> Groundfield v

{-
instance (Floating (Groundfield v), VectorSpace v, InnerProductSpace v) => NormedSpace v where
    norm v = sqrt (v `dot` v)
-}
{- I'd like to know why this won't work
normalize :: (Eq a, NormedSpace v a) => v -> v
normalize v = if nv /= 0 then v ^/ nv else error "normalize: zero vector"
    where nv = norm v
    -}
