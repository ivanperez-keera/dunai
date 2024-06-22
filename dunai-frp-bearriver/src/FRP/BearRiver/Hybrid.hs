{-# LANGUAGE Arrows #-}
-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Discrete to continuous-time signal functions.
module FRP.BearRiver.Hybrid
    (
      -- * Wave-form generation
      hold
    , dHold
    , trackAndHold
    , dTrackAndHold

      -- * Accumulators
    , accum
    , accumHold
    , dAccumHold
    , accumBy
    , accumHoldBy
    , dAccumHoldBy
    , accumFilter
    )
  where

-- External imports
import Control.Arrow (arr, returnA, (<<<), (>>>))

-- Internal imports (dunai)
import Data.MonadicStreamFunction (accumulateWith, feedback)

-- Internal imports (bearriver)
import FRP.BearRiver.Arrow        (dup)
import FRP.BearRiver.Delays       (iPre)
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

-- | Zero-order hold with a delay.
--
-- Converts a discrete-time signal into a continuous-time signal, by holding the
-- last value until it changes in the input signal. The given parameter is used
-- for time zero (until the first event occurs in the input signal), so 'dHold'
-- shifts the discrete input by an infinitesimal delay.
--
-- >>> embed (dHold 1) (deltaEncode 0.1 [NoEvent, NoEvent, Event 2, NoEvent, Event 3, NoEvent])
-- [1,1,1,2,2,3]
dHold :: Monad m => a -> SF m (Event a) a
dHold a0 = hold a0 >>> iPre a0

-- | Tracks input signal when available, holding the last value when the input
-- is 'Nothing'.
--
-- This behaves similarly to 'hold', but there is a conceptual difference, as it
-- takes a signal of input @Maybe a@ (for some @a@) and not @Event@.
--
-- >>> embed (trackAndHold 1) (deltaEncode 0.1 [Nothing, Nothing, Just 2, Nothing, Just 3, Nothing])
-- [1,1,2,2,3,3]
trackAndHold :: Monad m => a -> SF m (Maybe a) a
trackAndHold aInit = arr (maybe NoEvent Event) >>> hold aInit

-- | Tracks input signal when available, holding the last value when the input
-- is 'Nothing', with a delay.
--
-- This behaves similarly to 'hold', but there is a conceptual difference, as it
-- takes a signal of input @Maybe a@ (for some @a@) and not @Event@.
--
-- >>> embed (dTrackAndHold 1) (deltaEncode 0.1 [Nothing, Nothing, Just 2, Nothing, Just 3, Nothing])
-- [1,1,1,2,2,3]
dTrackAndHold :: Monad m => a -> SF m (Maybe a) a
dTrackAndHold aInit = trackAndHold aInit >>> iPre aInit

-- ** Accumulators

-- | Given an initial value in an accumulator, it returns a signal function that
-- processes an event carrying transformation functions. Every time an 'Event'
-- is received, the function inside it is applied to the accumulator, whose new
-- value is outputted in an 'Event'.
accum :: Monad m => a -> SF m (Event (a -> a)) (Event a)
accum aInit = feedback aInit $ arr $ \(f, a) -> case f of
    NoEvent -> (NoEvent, a)
    Event f' -> let a' = f' a
                in (Event a', a')

-- | Zero-order hold accumulator (always produces the last outputted value until
-- an event arrives).
accumHold :: Monad m => a -> SF m (Event (a -> a)) a
accumHold aInit = feedback aInit $ arr $ \(f, a) -> case f of
    NoEvent -> (a, a)
    Event f' -> let a' = f' a
                in (a', a')

-- | Zero-order hold accumulator with delayed initialization (always produces
-- the last outputted value until an event arrives, but the very initial output
-- is always the given accumulator).
dAccumHold :: Monad m => a -> SF m (Event (a -> a)) a
dAccumHold aInit = accumHold aInit >>> iPre aInit

-- | Accumulator parameterized by the accumulation function.
accumBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) (Event b)
accumBy f b = mapEventS $ accumulateWith (flip f) b

-- | Zero-order hold accumulator parameterized by the accumulation function.
accumHoldBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) b
accumHoldBy f b = feedback b $ arr $ \(a, b') ->
  let b'' = event b' (f b') a
  in (b'', b'')

-- | Zero-order hold accumulator parameterized by the accumulation function with
-- delayed initialization (initial output sample is always the given
-- accumulator).
dAccumHoldBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) b
dAccumHoldBy f aInit = accumHoldBy f aInit >>> iPre aInit

-- | Accumulator parameterized by the accumulator function with filtering,
-- possibly discarding some of the input events based on whether the second
-- component of the result of applying the accumulation function is 'Nothing' or
-- 'Just' x for some x.
accumFilter :: Monad m
            => (c -> a -> (c, Maybe b)) -> c -> SF m (Event a) (Event b)
accumFilter g cInit = feedback cInit $ arr $ \(a, c) ->
  case a of
    NoEvent -> (NoEvent, c)
    Event a' -> case g c a' of
                  (c', Nothing) -> (NoEvent, c')
                  (c', Just b)  -> (Event b, c')

-- * Events

-- | Apply an 'SF' to every input. Freezes temporarily if the input is
-- 'NoEvent', and continues as soon as an 'Event' is received.
mapEventS :: Monad m => SF m a b -> SF m (Event a) (Event b)
mapEventS msf = proc eventA -> case eventA of
  Event a -> arr Event <<< msf -< a
  NoEvent -> returnA           -< NoEvent
