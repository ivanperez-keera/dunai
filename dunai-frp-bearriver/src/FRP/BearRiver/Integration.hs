{-# LANGUAGE Arrows #-}
-- |
-- Copyright  : (c) Ivan Perez, 2019-2023
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Implementation of integrals and derivatives using Monadic Stream Processing
-- library.
module FRP.BearRiver.Integration
    (
      -- * Integration
      integral

      -- * Differentiation
    , derivative
    , iterFrom
    )
  where

-- External imports
import Control.Arrow    (returnA)
import Data.VectorSpace (VectorSpace, zeroVector, (*^), (^+^), (^-^), (^/))

-- Internal imports (dunai)
import Control.Monad.Trans.MSF                 (ask)
import Data.MonadicStreamFunction              (accumulateWith, constM, iPre)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF))

-- Internal imports
import FRP.BearRiver.InternalCore (DTime, SF)

-- * Integration and differentiation

-- | Integration using the rectangle rule.
integral :: (Monad m, Fractional s, VectorSpace a s) => SF m a a
integral = integralFrom zeroVector

-- | Integrate using an auxiliary function that takes the current and the last
-- input, the time between those samples, and the last output, and returns a
-- new output.
integralFrom :: (Monad m, Fractional s, VectorSpace a s) => a -> SF m a a
integralFrom a0 = proc a -> do
  dt <- constM ask        -< ()
  accumulateWith (^+^) a0 -< realToFrac dt *^ a

-- | A very crude version of a derivative. It simply divides the value
-- difference by the time difference. Use at your own risk.
derivative :: (Monad m, Fractional s, VectorSpace a s) => SF m a a
derivative = derivativeFrom zeroVector

-- | A very crude version of a derivative. It simply divides the value
-- difference by the time difference. Use at your own risk.
--
-- Starts from a given value for the input signal at time zero.
derivativeFrom :: (Monad m, Fractional s, VectorSpace a s) => a -> SF m a a
derivativeFrom a0 = proc a -> do
  dt   <- constM ask  -< ()
  aOld <- iPre a0     -< a
  returnA             -< (a ^-^ aOld) ^/ realToFrac dt

-- | Integrate using an auxiliary function that takes the current and the last
-- input, the time between those samples, and the last output, and returns a
-- new output.

-- NOTE: BUG in this function, it needs two a's but we can only provide one
iterFrom :: Monad m => (a -> a -> DTime -> b -> b) -> b -> SF m a b
iterFrom f b = MSF $ \a -> do
  dt <- ask
  let b' = f a a dt b
  return (b, iterFrom f b')
