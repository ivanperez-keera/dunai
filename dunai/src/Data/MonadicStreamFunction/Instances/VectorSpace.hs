{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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

-- Note: This module uses undecidable instances, because GHC does not know
-- enough to assert that it will be able to determine the type of 's' from the
-- type of 'v', because 'v' only appears under 'MSF' in the instance head and
-- it cannot determine what 'MSF' will do to 'v' and whether the type can be
-- resolved.
module Data.MonadicStreamFunction.Instances.VectorSpace where

import Control.Arrow
import Control.Arrow.Util
import Data.MonadicStreamFunction.Core
import Data.VectorSpace

-- | Vector-space instance for 'MSF's.
instance (Monad m, VectorSpace v s) => VectorSpace (MSF m a v) s where
  zeroVector   = constantly zeroVector
  r   *^ msf   = msf >>^ (r *^)
  msf ^/ r     = msf >>^ (^/ r)
  (^+^)        = elementwise2 (^+^)
  (^-^)        = elementwise2 (^-^)
  negateVector = (>>^ negateVector)
