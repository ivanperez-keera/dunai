{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module Data.VectorSpace.Tuples where

import Data.VectorSpace

------------------------------------------------------------------------------
-- Vector space instances for small tuples of Fractional
------------------------------------------------------------------------------



instance (Groundring a ~ Groundring b, RModule a, RModule b) => RModule (a, b) where
    type Groundring (a, b) = Groundring a
    zeroVector = (zeroVector, zeroVector)
    (a, b) ^* x = (a ^* x, b ^* x)
    (a1, b1) ^+^ (a2, b2) = (a1 ^+^ a2, b1 ^+^ b2)

instance (Groundfield a ~ Groundfield b, VectorSpace a, VectorSpace b) => VectorSpace (a, b) where
    (a, b) ^/ x = (a ^/ x, b ^/ x)

instance (Groundfield a ~ Groundfield b, InnerProductSpace a, InnerProductSpace b) => InnerProductSpace (a, b) where
    (a1, b1) `dot` (a2, b2) = (a1 `dot` a2) + (b1 `dot` b2)

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

instance Num a => RModule (a,a,a) where
    type Groundring (a,a,a) = a
    zeroVector = (0,0,0)
    a *^ (x,y,z) = (a * x, a * y, a * z)
    negateVector (x,y,z) = (-x, -y, -z)
    (x1,y1,z1) ^+^ (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
    (x1,y1,z1) ^-^ (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

instance Fractional a => VectorSpace (a,a,a) where
    (x,y,z) ^/ a = (x / a, y / a, z / a)


instance Num a => InnerProductSpace (a,a,a) where
    (x1,y1,z1) `dot` (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2


instance Num a => RModule (a,a,a,a) where
    type Groundring (a,a,a,a) = a
    zeroVector = (0,0,0,0)
    a *^ (x,y,z,u) = (a * x, a * y, a * z, a * u)
    negateVector (x,y,z,u) = (-x, -y, -z, -u)
    (x1,y1,z1,u1) ^+^ (x2,y2,z2,u2) = (x1+x2, y1+y2, z1+z2, u1+u2)
    (x1,y1,z1,u1) ^-^ (x2,y2,z2,u2) = (x1-x2, y1-y2, z1-z2, u1-u2)

instance Fractional a => VectorSpace (a,a,a,a) where
    (x,y,z,u) ^/ a = (x / a, y / a, z / a, u / a)


instance Num a => InnerProductSpace (a,a,a,a) where
    (x1,y1,z1,u1) `dot` (x2,y2,z2,u2) = x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2


instance Num a => RModule (a,a,a,a,a) where
    type Groundring (a,a,a,a,a) = a
    zeroVector = (0,0,0,0,0)
    a *^ (x,y,z,u,v) = (a * x, a * y, a * z, a * u, a * v)
    negateVector (x,y,z,u,v) = (-x, -y, -z, -u, -v)
    (x1,y1,z1,u1,v1) ^+^ (x2,y2,z2,u2,v2) = (x1+x2, y1+y2, z1+z2, u1+u2, v1+v2)
    (x1,y1,z1,u1,v1) ^-^ (x2,y2,z2,u2,v2) = (x1-x2, y1-y2, z1-z2, u1-u2, v1-v2)

instance Fractional a => VectorSpace (a,a,a,a,a) where
    (x,y,z,u,v) ^/ a = (x / a, y / a, z / a, u / a, v / a)


instance Num a => InnerProductSpace (a,a,a,a,a) where
    (x1,y1,z1,u1,v1) `dot` (x2,y2,z2,u2,v2) =
        x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2 + v1 * v2
