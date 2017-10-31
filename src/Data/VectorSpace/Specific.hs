{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Vector space instances for concrete/specific types.
--
-- This module contains:
--
--     * 'RModule' instances for 'Int', 'Integer', 'Double' and 'Float'.
--
--     * 'VectorSpace' for 'Double' and 'Float'.

module Data.VectorSpace.Specific where

import Data.VectorSpace


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
