{-# LANGUAGE Arrows #-}
-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Discrete to continuous-time signal functions.
module FRP.BearRiver.Hybrid where

-- External imports
import Control.Arrow (arr, returnA, (<<<))

-- Internal imports (dunai)
import Data.MonadicStreamFunction (accumulateWith, feedback)

-- Internal imports (bearriver)
import FRP.BearRiver.Arrow        (dup)
import FRP.BearRiver.Event        (Event (..), event)
import FRP.BearRiver.InternalCore (SF)

-- * Discrete to continuous-time signal functions

-- ** Wave-form generation

-- | Zero-order hold.
--
-- Converts a discrete-time signal into a continuous-time signal, by holding
-- the last value until it changes in the input signal. The given parameter may
-- be used for time zero, and until the first event occurs in the input signal,
-- so hold is always well-initialized.
--
-- >>> embed (hold 1) (deltaEncode 0.1 [NoEvent, NoEvent, Event 2, NoEvent, Event 3, NoEvent])
-- [1,1,2,2,3,3]
hold :: Monad m => a -> SF m (Event a) a
hold a = feedback a $ arr $ \(e, a') ->
  dup (event a' id e)

-- ** Accumulators

-- | Accumulator parameterized by the accumulation function.
accumBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) (Event b)
accumBy f b = mapEventS $ accumulateWith (flip f) b

-- | Zero-order hold accumulator parameterized by the accumulation function.
accumHoldBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) b
accumHoldBy f b = feedback b $ arr $ \(a, b') ->
  let b'' = event b' (f b') a
  in (b'', b'')

-- * Events

-- | Apply an 'SF' to every input. Freezes temporarily if the input is
-- 'NoEvent', and continues as soon as an 'Event' is received.
mapEventS :: Monad m => SF m a b -> SF m (Event a) (Event b)
mapEventS msf = proc eventA -> case eventA of
  Event a -> arr Event <<< msf -< a
  NoEvent -> returnA           -< NoEvent
