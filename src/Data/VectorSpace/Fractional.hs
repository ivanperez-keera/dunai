{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
-- | 'VectorSpace' instances for 'Num'/'Fractional' types.
--
-- This module includes instances for:
--
--    * 'InnerProductSpace' and 'RModule' for 'Num'
--
--    * 'VectorSpace' for 'Fractional's
module Data.VectorSpace.Fractional where

-- These sometimes clash with user-defined instances.
-- (See https://github.com/ivanperez-keera/dunai/issues/11, where this
-- module used to be called Data.VectorSpace.Instances)

import Data.VectorSpace

-- | 'RModule' instance for any number, where '^+^' is '+' and multiplication is
-- normal multiplication.
instance Num a => RModule a where
    type Groundring a = a
    zeroVector     = 0
    a *^ x         = a * x
    negateVector x = -x
    x1 ^+^ x2      = x1 + x2
    x1 ^-^ x2      = x1 - x2

-- | 'VectorSpace' instance for any 'Fractional', where vectorial division is
-- normal number division.
instance Fractional a => VectorSpace a where
    a ^/ x = a / x

-- | Inner-product instance for any number.
instance Num a => InnerProductSpace a where
    x1 `dot` x2 = x1 * x2
