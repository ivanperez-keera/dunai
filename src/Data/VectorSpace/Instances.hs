{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module Data.VectorSpace.Instances where

import Data.VectorSpace


instance Num a => RModule a where
    type Groundring a = a
    zeroVector     = 0
    a *^ x         = a * x
    negateVector x = -x
    x1 ^+^ x2      = x1 + x2
    x1 ^-^ x2      = x1 - x2


instance Fractional a => VectorSpace a where
    a ^/ x = a / x

instance Num a => InnerProductSpace a where
    x1 `dot` x2 = x1 * x2
