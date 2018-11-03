{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
-- * Vector space classes
------------------------------------------------------------------------------

infixr 6 *^
infixl 6 ^/
infix 6 `dot`
infixl 5 ^+^, ^-^

-- | R-modules.
--   A module @v@ over a ring @Groundring v@
--   is an abelian group with a linear multiplication.
--   The hat @^@ denotes the side of an operation
--   on which the vector stands,
--   i.e. @a *^ v@ for @v@ a vector.
--
-- A minimal definition should include the type 'Groundring' and the
-- implementations of 'zeroVector', '^+^', and one of '*^' or '^*'.
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

-- | A vector space is a module over a field,
--   i.e. a commutative ring with inverses.
--
--   It needs to satisfy the axiom
--   @v ^/ a == (1/a) *^ v@,
--   which is the default implementation.
class (Fractional (Groundring v), RModule v) => VectorSpace v where
    (^/) :: v -> Groundfield v -> v
    v ^/ a = (1/a) *^ v

-- | The ground ring of a vector space is required to be commutative
--   and to possess inverses.
--   It is then called the "ground field".
--   Commutativity amounts to the law @a * b = b * a@,
--   and the existence of inverses is given
--   by the requirement of the 'Fractional' type class.
type Groundfield v = Groundring v

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
class (Floating (Groundfield v), InnerProductSpace v, VectorSpace v) => NormedSpace v  where
  norm :: v -> Groundfield v
  norm v = sqrt $ v `dot` v

-- | Divides a vector by its norm, resulting in a vector of norm 1.
--   Throws an error on vectors with norm 0.
normalize :: (Eq (Groundfield v), NormedSpace v) => v -> v
normalize v = if nv /= 0 then v ^/ nv else error "normalize: zero vector"
  where nv = norm v


-----------------------------
-- Instances for scalar types
-----------------------------


instance RModule Int where
    type Groundring Int = Int
    (^+^) = (+)
    (^*) = (*)
    zeroVector = 0

instance RModule Integer where
    type Groundring Integer = Integer
    (^+^) = (+)
    (^*) = (*)
    zeroVector = 0

instance RModule Double where
    type Groundring Double = Double
    (^+^) = (+)
    (^*) = (*)
    zeroVector = 0

instance RModule Float where
    type Groundring Float = Float
    (^+^) = (+)
    (^*) = (*)
    zeroVector = 0

instance VectorSpace Double where

instance VectorSpace Float where

-----------------------
-- Instances for tuples
-----------------------


instance
  ( Groundring a ~ Groundring b
  , RModule a, RModule b
  ) => RModule (a, b) where
    type Groundring (a, b) = Groundring a
    zeroVector = (zeroVector, zeroVector)
    (a, b) ^* x = (a ^* x, b ^* x)
    (a1, b1) ^+^ (a2, b2) = (a1 ^+^ a2, b1 ^+^ b2)

instance
  (Groundfield a ~ Groundfield b
  , VectorSpace a, VectorSpace b
  ) => VectorSpace (a, b) where
    (a, b) ^/ x = (a ^/ x, b ^/ x)

instance (Groundfield a ~ Groundfield b, InnerProductSpace a, InnerProductSpace b) => InnerProductSpace (a, b) where
    (a1, b1) `dot` (a2, b2) = (a1 `dot` a2) + (b1 `dot` b2)

instance (Groundfield a ~ Groundfield b, NormedSpace a, NormedSpace b) => NormedSpace (a, b) where

-- ** Utilities to work with n-tuples for n = 3, 4, 5

break3Tuple :: (a, b, c) -> ((a, b), c)
break3Tuple    (a, b, c) =  ((a, b), c)

join3Tuple  :: ((a, b), c) -> (a, b, c)
join3Tuple     ((a, b), c) =  (a, b, c)

break4Tuple :: (a, b, c, d) -> ((a, b), (c, d))
break4Tuple    (a, b, c, d) =  ((a, b), (c, d))

join4Tuple  :: ((a, b), (c, d)) -> (a, b, c, d)
join4Tuple     ((a, b), (c, d)) =  (a, b, c, d)

break5Tuple :: (a, b, c, d, e) -> ((a, b), (c, d, e))
break5Tuple    (a, b, c, d, e) =  ((a, b), (c, d, e))

join5Tuple  :: ((a, b), (c, d, e)) -> (a, b, c, d, e)
join5Tuple     ((a, b), (c, d, e)) =  (a, b, c, d, e)



instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , RModule a, RModule b, RModule c
  ) => RModule (a, b, c) where
    type Groundring (a, b, c) = Groundring a
    zeroVector = join3Tuple zeroVector
    a *^ v = join3Tuple $ a *^ (break3Tuple v)
    v1 ^+^ v2 = join3Tuple $ break3Tuple v1 ^+^ break3Tuple v2

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , VectorSpace a, VectorSpace b, VectorSpace c
  ) => VectorSpace (a, b, c) where

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , InnerProductSpace a, InnerProductSpace b, InnerProductSpace c
  ) => InnerProductSpace (a, b, c) where
  v1 `dot` v2 = break3Tuple v1 `dot` break3Tuple v2

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , NormedSpace a, NormedSpace b, NormedSpace c
  ) => NormedSpace (a, b, c) where



instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , Groundring a ~ Groundring d
  , RModule a, RModule b, RModule c, RModule d
  ) => RModule (a, b, c, d) where
    type Groundring (a, b, c, d) = Groundring a
    zeroVector = join4Tuple zeroVector
    a *^ v = join4Tuple $ a *^ (break4Tuple v)
    v1 ^+^ v2 = join4Tuple $ break4Tuple v1 ^+^ break4Tuple v2

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , Groundring a ~ Groundring d
  , VectorSpace a, VectorSpace b, VectorSpace c, VectorSpace d
  ) => VectorSpace (a, b, c, d) where

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , Groundring a ~ Groundring d
  , InnerProductSpace a, InnerProductSpace b
  , InnerProductSpace c, InnerProductSpace d
  ) => InnerProductSpace (a, b, c, d) where
  v1 `dot` v2 = break4Tuple v1 `dot` break4Tuple v2

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , Groundring a ~ Groundring d
  , NormedSpace a, NormedSpace b, NormedSpace c, NormedSpace d
  ) => NormedSpace (a, b, c, d) where



instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , Groundring a ~ Groundring d
  , Groundring a ~ Groundring e
  , RModule a, RModule b, RModule c, RModule d, RModule e
  ) => RModule (a, b, c, d, e) where
    type Groundring (a, b, c, d, e) = Groundring a
    zeroVector = join5Tuple zeroVector
    a *^ v = join5Tuple $ a *^ (break5Tuple v)
    v1 ^+^ v2 = join5Tuple $ break5Tuple v1 ^+^ break5Tuple v2

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , Groundring a ~ Groundring d
  , Groundring a ~ Groundring e
  , VectorSpace a, VectorSpace b, VectorSpace c, VectorSpace d, VectorSpace e
  ) => VectorSpace (a, b, c, d, e) where

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , Groundring a ~ Groundring d
  , Groundring a ~ Groundring e
  , InnerProductSpace a, InnerProductSpace b, InnerProductSpace c
  , InnerProductSpace d, InnerProductSpace e
  ) => InnerProductSpace (a, b, c, d, e) where
  v1 `dot` v2 = break5Tuple v1 `dot` break5Tuple v2

instance
  ( Groundring a ~ Groundring b
  , Groundring a ~ Groundring c
  , Groundring a ~ Groundring d
  , Groundring a ~ Groundring e
  , NormedSpace a, NormedSpace b, NormedSpace c, NormedSpace d, NormedSpace e
  ) => NormedSpace (a, b, c, d, e) where


-- * Vector spaces from arbitrary 'Fractional's

-- | Wrap an arbitrary 'Fractional' in this newtype
--   in order to get 'VectorSpace', and related instances.
newtype FractionalVectorSpace a = FractionalVectorSpace { getFractional :: a }
  deriving (Num, Fractional)


instance Num a => RModule (FractionalVectorSpace a) where
  type Groundring (FractionalVectorSpace a) = a
  v1 ^+^ v2 = FractionalVectorSpace $ getFractional v1 + getFractional v2
  v ^* a = FractionalVectorSpace $ getFractional v * a
  zeroVector = FractionalVectorSpace 0

instance Fractional a => VectorSpace (FractionalVectorSpace a) where

instance Num a => InnerProductSpace (FractionalVectorSpace a) where
  v1 `dot` v2 = getFractional v1 * getFractional v2

instance Floating a => NormedSpace (FractionalVectorSpace a) where
