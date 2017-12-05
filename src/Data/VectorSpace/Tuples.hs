{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
-- | Vector space instances for small tuples of 'Fractional'.
--
-- This module contains 'RModule', 'VectorSpace' and 'InnerProductSpace' for
-- tuples of up to five elements.

module Data.VectorSpace.Tuples where

import Data.VectorSpace

-- | R-module instance for tuples.
instance (Groundring a ~ Groundring b, RModule a, RModule b) => RModule (a, b) where
    type Groundring (a, b) = Groundring a
    zeroVector = (zeroVector, zeroVector)
    (a, b) ^* x = (a ^* x, b ^* x)
    (a1, b1) ^+^ (a2, b2) = (a1 ^+^ a2, b1 ^+^ b2)

-- | Vector-space instance for tuples.
instance (Groundfield a ~ Groundfield b, VectorSpace a, VectorSpace b) => VectorSpace (a, b) where
    (a, b) ^/ x = (a ^/ x, b ^/ x)

-- | Inner Product Space instance for tuples.
instance (Groundfield a ~ Groundfield b, InnerProductSpace a, InnerProductSpace b) => InnerProductSpace (a, b) where
    (a1, b1) `dot` (a2, b2) = (a1 `dot` a2) + (b1 `dot` b2)

instance (Groundfield a ~ Groundfield b, NormedSpace a, NormedSpace b) => NormedSpace (a, b) where


{-
instance Num a => RModule (a,a) where
    type Groundring (a,a) = a
    zeroVector = (0,0)
    a *^ (x,y) = (a * x, a * y)
    negateVector (x,y) = (-x, -y)
    (x1,y1) ^+^ (x2,y2) = (x1 + x2, y1 + y2)
    (x1,y1) ^-^ (x2,y2) = (x1 - x2, y1 - y2)

instance Fractional a => VectorSpace (a,a) where
    (x,y) ^/ a = (x / a, y / a)


instance Fractional a => InnerProductSpace (a,a) where
    (x1,y1) `dot` (x2,y2) = x1 * x2 + y1 * y2

-}

-- | R-module instance for tuples with 3 elements.
instance Num a => RModule (a,a,a) where
    type Groundring (a,a,a) = a
    zeroVector = (0,0,0)
    a *^ (x,y,z) = (a * x, a * y, a * z)
    negateVector (x,y,z) = (-x, -y, -z)
    (x1,y1,z1) ^+^ (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
    (x1,y1,z1) ^-^ (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

-- | Vector-space instance for tuples with 3 elements.
instance Fractional a => VectorSpace (a,a,a) where
    (x,y,z) ^/ a = (x / a, y / a, z / a)

-- | Inner Product Space instance for tuples with 3 elements.
instance Num a => InnerProductSpace (a,a,a) where
    (x1,y1,z1) `dot` (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2

instance Floating a => NormedSpace (a, a, a) where


-- | R-module instance for tuples with 4 elements.
instance Num a => RModule (a,a,a,a) where
    type Groundring (a,a,a,a) = a
    zeroVector = (0,0,0,0)
    a *^ (x,y,z,u) = (a * x, a * y, a * z, a * u)
    negateVector (x,y,z,u) = (-x, -y, -z, -u)
    (x1,y1,z1,u1) ^+^ (x2,y2,z2,u2) = (x1+x2, y1+y2, z1+z2, u1+u2)
    (x1,y1,z1,u1) ^-^ (x2,y2,z2,u2) = (x1-x2, y1-y2, z1-z2, u1-u2)

-- | Vector-space instance for tuples with 4 elements.
instance Fractional a => VectorSpace (a,a,a,a) where
    (x,y,z,u) ^/ a = (x / a, y / a, z / a, u / a)

-- | Inner Product Space instance for tuples with 4 elements.
instance Num a => InnerProductSpace (a,a,a,a) where
    (x1,y1,z1,u1) `dot` (x2,y2,z2,u2) = x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2

instance Floating a => NormedSpace (a, a, a, a) where


-- | R-module instance for tuples with 5 elements.
instance Num a => RModule (a,a,a,a,a) where
    type Groundring (a,a,a,a,a) = a
    zeroVector = (0,0,0,0,0)
    a *^ (x,y,z,u,v) = (a * x, a * y, a * z, a * u, a * v)
    negateVector (x,y,z,u,v) = (-x, -y, -z, -u, -v)
    (x1,y1,z1,u1,v1) ^+^ (x2,y2,z2,u2,v2) = (x1+x2, y1+y2, z1+z2, u1+u2, v1+v2)
    (x1,y1,z1,u1,v1) ^-^ (x2,y2,z2,u2,v2) = (x1-x2, y1-y2, z1-z2, u1-u2, v1-v2)

-- | Vector-space instance for tuples with 5 elements.
instance Fractional a => VectorSpace (a,a,a,a,a) where
    (x,y,z,u,v) ^/ a = (x / a, y / a, z / a, u / a, v / a)

-- | Inner Product Space instance for tuples with 5 elements.
instance Num a => InnerProductSpace (a,a,a,a,a) where
    (x1,y1,z1,u1,v1) `dot` (x2,y2,z2,u2,v2) =
        x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2 + v1 * v2

instance Floating a => NormedSpace (a, a, a, a, a) where
