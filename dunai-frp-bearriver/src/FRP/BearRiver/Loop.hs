-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Well-initialised loops.
module FRP.BearRiver.Loop
    (
      -- * Loops with guaranteed well-defined feedback
      loopPre
    , loopIntegral
    )
  where

-- External imports
import Control.Arrow     (loop, second, (>>>))
import Control.Monad.Fix (MonadFix)
import Data.VectorSpace  (VectorSpace)

-- Internal imports
import FRP.BearRiver.Delays       (iPre)
import FRP.BearRiver.Integration  (integral)
import FRP.BearRiver.InternalCore (SF)

-- * Loops with guaranteed well-defined feedback

-- | Loop with an initial value for the signal being fed back.
loopPre :: MonadFix m => c -> SF m (a, c) (b, c) -> SF m a b
loopPre cInit sf = loop (second (iPre cInit) >>> sf)

-- | Loop by integrating the second value in the pair and feeding the result
-- back. Because the integral at time 0 is zero, this is always well defined.
loopIntegral :: (MonadFix m, Fractional s, VectorSpace c s) => SF m (a, c) (b, c) -> SF m a b
loopIntegral sf = loop (second integral >>> sf)
