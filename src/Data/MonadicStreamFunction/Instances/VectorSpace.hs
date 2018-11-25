{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | 'VectorSpace' instances for 'MSF's that produce vector spaces. This allows
-- you to use vector operators with 'MSF's that output vectors, for example, you
-- can write:
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
-- Instances are provided for the type classes 'RModule' and 'VectorSpace'.
module Data.MonadicStreamFunction.Instances.VectorSpace where

import Control.Arrow
import Control.Arrow.Util
import Data.MonadicStreamFunction.Core
import Data.VectorSpace

-- These conflict with Data.VectorSpace.Instances

-- | R-module instance for 'MSF's.
instance (Monad m, RModule v) => RModule (MSF m a v) where
  type Groundring (MSF m a v) = Groundring v
  zeroVector   = constantly zeroVector
  r *^ msf     = msf >>^ (r *^)
  negateVector = (>>^ negateVector)
  (^+^)        = elementwise2 (^+^)
  (^-^)        = elementwise2 (^-^)

-- | Vector-space instance for 'MSF's.
instance (Monad m, VectorSpace v) => VectorSpace (MSF m a v) where
  msf ^/ r = msf >>^ (^/ r)
