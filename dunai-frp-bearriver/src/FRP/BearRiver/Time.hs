-- |
-- Copyright  : (c) Ivan Perez, 2019-2023
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- SF primitives that producing the current running time.
--
-- Time is global for an 'SF', so, every constituent 'SF' will use the same
-- global clock. However, when used in combination with
-- 'FRP.BearRiver.Switches.switch'ing, the SF switched into will be started at
-- the time of switching, so any reference to 'localTime' or 'time' from that
-- 'SF' will count using the time of switching as the start time.
--
-- Take also into account that, because 'FRP.BearRiver.Integration.derivative'
-- is the derivative of a signal /over time/, differentiating 'localTime' will
-- always produce the value one (@1@). If you really, really, really need to
-- know the time delta, and need to abandon the hybrid\/FRP abstraction, see
-- 'FRP.BearRiver.Integration.iterFrom'.
module FRP.BearRiver.Time
    ( localTime
    , time
    )
  where

-- External imports
import Control.Arrow ((>>>))

-- Internal imports
import FRP.BearRiver.Basic        (constant)
import FRP.BearRiver.Integration  (integral)
import FRP.BearRiver.InternalCore (SF, Time)

-- | Outputs the time passed since the signal function instance was started.
localTime :: Monad m => SF m a Time
localTime = constant 1.0 >>> integral

-- | Alternative name for localTime.
time :: Monad m => SF m a Time
time = localTime
