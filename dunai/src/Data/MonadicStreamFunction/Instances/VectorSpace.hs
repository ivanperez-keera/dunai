{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'VectorSpace' instances for 'MSF's that produce vector spaces. This allows
-- you to use vector operators with 'MSF's that output vectors, for example,
-- you can write:
--
-- @
-- msf1 :: MSF Input (Double, Double) -- defined however you want
-- msf2 :: MSF Input (Double, Double) -- defined however you want
-- msf3 :: MSF Input (Double, Double)
-- msf3 = msf1 ^+^ msf2
-- @
--
-- instead of
--
-- @
-- msf3 = (msf1 &&& msf2) >>> arr (uncurry (^+^))
-- @
--
--
-- Instances are provided for the type class 'VectorSpace'.

-- Note: This module uses undecidable instances, because GHC does not know
-- enough to assert that it will be able to determine the type of 's' from the
-- type of 'v', because 'v' only appears under 'MSF' in the instance head and
-- it cannot determine what 'MSF' will do to 'v' and whether the type can be
-- resolved.
module Data.MonadicStreamFunction.Instances.VectorSpace where

-- External imports
import Control.Arrow    ((>>^))
import Data.VectorSpace (VectorSpace (..))

-- Internal imports
import Control.Arrow.Util              (constantly, elementwise2)
import Data.MonadicStreamFunction.Core (MSF)

-- | Vector-space instance for 'MSF's.
instance (Monad m, Eq s, Num s, VectorSpace v s, Floating (MSF m a s))
      => VectorSpace (MSF m a v) (MSF m a s)
  where
    zeroVector   = constantly zeroVector
    (*^)         = elementwise2 (*^)
    (^/)         = elementwise2 (^/)
    (^+^)        = elementwise2 (^+^)
    (^-^)        = elementwise2 (^-^)
    negateVector = (>>^ negateVector)
    dot          = elementwise2 dot
    normalize v  = elementwise2 f v (norm v)
      where
        f v' nv'
          | nv' /= 0  = v' ^/ nv'
          | otherwise = error "normalize: zero vector"
