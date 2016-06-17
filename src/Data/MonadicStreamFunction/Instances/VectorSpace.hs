{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MonadicStreamFunction.Instances.VectorSpace where

import Control.Arrow.Util
import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.Instances
import Data.VectorSpace

-- These conflict with Data.VectorSpace.Instances
instance (Monad m, RModule v) => RModule (MStreamF m a v) where
  type Groundring (MStreamF m a v) = Groundring v
  zeroVector   = constantly zeroVector
  r *^ msf     = elementwise  (r *^) msf
  negateVector = elementwise  negateVector
  (^+^)        = elementwise2 (^+^)
  (^-^)        = elementwise2 (^-^)

instance (Monad m, VectorSpace v) => VectorSpace (MStreamF m a v) where
  msf ^/ r = elementwise (^/ r) msf
