-- |
-- Module      : FRP.BearRiver.Random
-- Copyright   : (c) Ivan Perez, 2014-2024
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD3
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Signals and signal functions with noise and randomness.
--
-- The Random number generators are re-exported from "System.Random".
module FRP.BearRiver.Random
    (
      -- * Random number generators
      RandomGen(..)
    , Random(..)

      -- * Noise, random signals, and stochastic event sources
    , noise
    , noiseR
    , occasionally
    )
  where

-- External imports
import System.Random (Random (..), RandomGen (..))

-- Internal imports (dunai)
import Control.Monad.Trans.MSF.Except (dSwitch)
import Control.Monad.Trans.MSF.Reader (readerS)
import Data.MonadicStreamFunction     (MSF, constM, feedback)

-- Internal imports
import FRP.BearRiver.Event        (Event (..))
import FRP.BearRiver.InternalCore (DTime, SF, Time, arr)

-- * Noise (i.e. random signal generators) and stochastic processes

-- | Noise (random signal) with default range for type in question; based on
-- "randoms".
noise :: (RandomGen g, Random b, Monad m) => g -> SF m a b
noise g0 = streamToSF (randoms g0)

-- | Noise (random signal) with specified range; based on "randomRs".
noiseR :: (RandomGen g, Random b, Monad m) => (b, b) -> g -> SF m a b
noiseR range g0 = streamToSF (randomRs range g0)

-- | Turn an infinite list of elements into an SF producing those elements. The
-- SF ignores its input.
streamToSF :: Monad m => [b] -> SF m a b
streamToSF ls = feedback ls $ arr $ fAux . snd
  where
    fAux []     = error "BearRiver: streamToSF: Empty list!"
    fAux (b:bs) = (b, bs)

-- | Stochastic event source with events occurring on average once every tAvg
-- seconds. However, no more than one event results from any one sampling
-- interval in the case of relatively sparse sampling, thus avoiding an "event
-- backlog" should sampling become more frequent at some later point in time.
occasionally :: (RandomGen g, Monad m) => g -> Time -> b -> SF m a (Event b)
occasionally g tAvg x | tAvg > 0  = tf0
                      | otherwise = error $ "BearRiver: occasionally: "
                                         ++ "Non-positive average interval."
  where
    -- Generally, if events occur with an average frequency of f, the
    -- probability of at least one event occurring in an interval of t is given
    -- by (1 - exp (-f*t)). The goal in the following is to decide whether at
    -- least one event occurred in the interval of size dt preceding the current
    -- sample point. For the first point, we can think of the preceding interval
    -- as being 0, implying no probability of an event occurring.
    tf0 = dSwitch
            (constM $ return (NoEvent, Just ()))
            (const $ feedback (randoms g :: [Time]) $ readerS $ arr occAux)

    -- occAux :: (DTime, (a, [Time])) -> (Event b, [Time])
    occAux (_, (_, []))    = error "BearRiver: occasionally: Empty list!"
    occAux (dt, (_, r:rs)) =
        (if r < p then Event x else NoEvent, rs)
      where
        p = 1 - exp (- (dt / tAvg)) -- Probability for at least one event.
