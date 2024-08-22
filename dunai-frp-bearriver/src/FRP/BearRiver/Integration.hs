{-# LANGUAGE Arrows #-}
-- |
-- Copyright  : (c) Ivan Perez, 2019-2023
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Integration and derivation of input signals.
--
-- In continuous time, these primitives define SFs that integrate/derive the
-- input signal. Since this is subject to the sampling resolution, simple
-- versions are implemented (like the rectangle rule for the integral).
--
-- In discrete time, all we do is count the number of events.
--
-- The combinator 'iterFrom' gives enough flexibility to program your own
-- leak-free integration and derivation SFs.
--
-- Many primitives and combinators in this module require instances of
-- simple-affine-spaces's 'VectorSpace'. BearRiver does not enforce the use of a
-- particular vector space implementation, meaning you could use 'integral' for
-- example with other vector types like V2, V1, etc. from the library linear.
-- For an example, see
-- <https://gist.github.com/walseb/1e0a0ca98aaa9469ab5da04e24f482c2 this gist>.
module FRP.BearRiver.Integration
    (
      -- * Integration
      integral
    , imIntegral
    , trapezoidIntegral
    , impulseIntegral
    , count

      -- * Differentiation
    , derivative
    , iterFrom
    )
  where

-- External imports
import Control.Arrow    (returnA, (***), (>>^))
import Data.VectorSpace (VectorSpace, zeroVector, (*^), (^+^), (^-^), (^/))

-- Internal imports (dunai)
import Control.Monad.Trans.MSF                 (ask)
import Data.MonadicStreamFunction              (accumulateWith, constM, iPre)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF))

-- Internal imports
import FRP.BearRiver.Event        (Event)
import FRP.BearRiver.Hybrid       (accumBy, accumHoldBy)
import FRP.BearRiver.InternalCore (DTime, SF)

-- * Integration

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

-- | \"Immediate\" integration (using the function's value at the current time).
imIntegral :: (Fractional s, VectorSpace a s, Monad m)
           => a -> SF m a a
imIntegral = ((\_ a' dt v -> v ^+^ realToFrac dt *^ a') `iterFrom`)

-- | Trapezoid integral (using the average between the value at the last time
-- and the value at the current time).
trapezoidIntegral :: (Fractional s, VectorSpace a s, Monad m) => SF m a a
trapezoidIntegral =
  iterFrom (\a a' dt v -> v ^+^ (realToFrac dt / 2) *^ (a ^+^ a')) zeroVector

-- | Integrate the first input signal and add the /discrete/ accumulation (sum)
-- of the second, discrete, input signal.
impulseIntegral :: (Fractional k, VectorSpace a k, Monad m)
                => SF m (a, Event a) a
impulseIntegral = (integral *** accumHoldBy (^+^) zeroVector) >>^ uncurry (^+^)

-- | Count the occurrences of input events.
--
-- >>> embed count (deltaEncode 1 [Event 'a', NoEvent, Event 'b'])
-- [Event 1,NoEvent,Event 2]
count :: (Integral b, Monad m) => SF m (Event a) (Event b)
count = accumBy (\n _ -> n + 1) 0

-- * Differentiation

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
