{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MonadicStreamFunction.Instances.VectorSpace where

import Control.Arrow
import Control.Arrow.Util
import Data.MonadicStreamFunction.Core
import Data.VectorSpace

-- These conflict with Data.VectorSpace.Instances
instance (Monad m, RModule v) => RModule (MSF m a v) where
  type Groundring (MSF m a v) = Groundring v
  zeroVector   = constantly zeroVector
  r *^ msf     = msf >>^ (r *^)
  negateVector = (>>^ negateVector)
  (^+^)        = elementwise2 (^+^)
  (^-^)        = elementwise2 (^-^)

instance (Monad m, VectorSpace v) => VectorSpace (MSF m a v) where
  msf ^/ r = msf >>^ (^/ r)
