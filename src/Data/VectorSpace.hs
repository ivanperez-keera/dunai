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

module Data.VectorSpace where

------------------------------------------------------------------------------
-- Vector space type relation
------------------------------------------------------------------------------

infixr 6 *^
infixl 6 ^/
infix 6 `dot`
infixl 5 ^+^, ^-^

-- | R-modules.
--
-- A minimal definition should include the type 'Groundring' and the
-- implementations of 'zeroVector', '^+^', and one of '*^' or '^*'.

-- TODO Add laws this should satisfy
class Num (Groundring v) => RModule v where
    type Groundring v
    zeroVector   :: v

    (*^)         :: Groundring v -> v -> v
    (*^)         = flip (^*)

    (^*)         :: v -> Groundring v -> v
    (^*)         = flip (*^)

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
class (Fractional (Groundring v), RModule v) => VectorSpace v where
    (^/)         :: v -> Groundfield v -> v
    v ^/ a = (1/a) *^ v

-- TODO Why is this not a type synonym?
type family Groundfield v :: *
type instance Groundfield v = Groundring v

class RModule v => InnerProductSpace v where
    dot          :: v -> v -> Groundfield v

class RModule v => NormedSpace v  where
    norm         :: v -> Groundfield v

{-
instance (Floating (Groundfield v), VectorSpace v, InnerProductSpace v) => NormedSpace v where
    norm v = sqrt (v `dot` v)
-}
{- I'd like to know why this won't work
normalize :: (Eq a, NormedSpace v a) => v -> v
normalize v = if nv /= 0 then v ^/ nv else error "normalize: zero vector"
    where nv = norm v
    -}
