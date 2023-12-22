{-# LANGUAGE Arrows     #-}
{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
-- The following warning is disabled so that we do not see warnings due to
-- using ListT on an MSF to implement parallelism with broadcasting.
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#else
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif
{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Implementation of Yampa using Monadic Stream Processing library.
module FRP.BearRiver
    (module FRP.BearRiver, module X)
  where

-- External imports
import Control.Arrow         as X
import Control.Monad.Random  (MonadRandom)
import Data.Functor.Identity (Identity (..))
import Data.Maybe            (fromMaybe)
import Data.VectorSpace      as X

-- Internal imports (dunai)
import           Control.Monad.Trans.MSF                 hiding (dSwitch)
import qualified Control.Monad.Trans.MSF                 as MSF
import           Data.MonadicStreamFunction              as X hiding (iPre,
                                                               once, reactimate,
                                                               repeatedly,
                                                               switch, trace)
import qualified Data.MonadicStreamFunction              as MSF
import           Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))
import           FRP.BearRiver.Arrow                     as X
import           FRP.BearRiver.Basic                     as X
import           FRP.BearRiver.Conditional               as X
import           FRP.BearRiver.Delays                    as X
import           FRP.BearRiver.Event                     as X
import           FRP.BearRiver.EventS                    as X
import           FRP.BearRiver.Integration               as X
import           FRP.BearRiver.InternalCore              as X
import           FRP.BearRiver.Scan                      as X
import           FRP.BearRiver.Switches                  as X

-- Internal imports (dunai, instances)
import Data.MonadicStreamFunction.Instances.ArrowLoop () -- not needed, just
                                                         -- re-exported

-- * Signal functions

-- ** Basic signal functions

-- | Outputs the time passed since the signal function instance was started.
localTime :: Monad m => SF m a Time
localTime = constant 1.0 >>> integral

-- | Alternative name for localTime.
time :: Monad m => SF m a Time
time = localTime

-- * Events

-- | Apply an 'MSF' to every input. Freezes temporarily if the input is
-- 'NoEvent', and continues as soon as an 'Event' is received.
mapEventS :: Monad m => MSF m a b -> MSF m (Event a) (Event b)
mapEventS msf = proc eventA -> case eventA of
  Event a -> arr Event <<< msf -< a
  NoEvent -> returnA           -< NoEvent

-- ** Relation to other types

-- | Convert an 'Event' into a 'Maybe' value.
--
-- Both types are isomorphic, where a value containing an event is mapped to a
-- 'Just', and 'NoEvent' is mapped to 'Nothing'. There is, however, a semantic
-- difference: a signal carrying a Maybe may change constantly, but, for a
-- signal carrying an 'Event', there should be a bounded frequency such that
-- sampling the signal faster does not render more event occurrences.
eventToMaybe :: Event a -> Maybe a
eventToMaybe = event Nothing Just

-- | Create an event if a 'Bool' is 'True'.
boolToEvent :: Bool -> Event ()
boolToEvent True  = Event ()
boolToEvent False = NoEvent

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

-- * State keeping combinators

-- ** Loops with guaranteed well-defined feedback

-- | Loop with an initial value for the signal being fed back.
loopPre :: Monad m => c -> SF m (a, c) (b, c) -> SF m a b
loopPre = feedback

-- * Noise (random signal) sources and stochastic event sources

-- | Stochastic event source with events occurring on average once every tAvg
-- seconds. However, no more than one event results from any one sampling
-- interval in the case of relatively sparse sampling, thus avoiding an "event
-- backlog" should sampling become more frequent at some later point in time.
occasionally :: MonadRandom m
             => Time -- ^ The time /q/ after which the event should be produced
                     -- on average
             -> b    -- ^ Value to produce at time of event
             -> SF m a (Event b)
occasionally tAvg b
    | tAvg <= 0
    = error "bearriver: Non-positive average interval in occasionally."

    | otherwise = proc _ -> do
        r   <- getRandomRS (0, 1) -< ()
        dt  <- timeDelta          -< ()
        let p = 1 - exp (-(dt / tAvg))
        returnA -< if r < p then Event b else NoEvent
  where
    timeDelta :: Monad m => SF m a DTime
    timeDelta = constM ask

-- * Execution/simulation

-- ** Reactimation

-- | Convenience function to run a signal function indefinitely, using a IO
-- actions to obtain new input and process the output.
--
-- This function first runs the initialization action, which provides the
-- initial input for the signal transformer at time 0.
--
-- Afterwards, an input sensing action is used to obtain new input (if any) and
-- the time since the last iteration. The argument to the input sensing
-- function indicates if it can block. If no new input is received, it is
-- assumed to be the same as in the last iteration.
--
-- After applying the signal function to the input, the actuation IO action is
-- executed. The first argument indicates if the output has changed, the second
-- gives the actual output). Actuation functions may choose to ignore the first
-- argument altogether. This action should return True if the reactimation must
-- stop, and False if it should continue.
--
-- Note that this becomes the program's /main loop/, which makes using this
-- function incompatible with GLUT, Gtk and other graphics libraries. It may
-- also impose a sizeable constraint in larger projects in which different
-- subparts run at different time steps. If you need to control the main loop
-- yourself for these or other reasons, use 'reactInit' and 'react'.
reactimate :: Monad m
           => m a
           -> (Bool -> m (DTime, Maybe a))
           -> (Bool -> b -> m Bool)
           -> SF Identity a b
           -> m ()
reactimate senseI sense actuate sf = do
    MSF.reactimateB $ senseSF >>> sfIO >>> actuateSF
    return ()
  where
    sfIO = morphS (return.runIdentity) (runReaderS sf)

    -- Sense
    senseSF = MSF.dSwitch senseFirst senseRest

    -- Sense: First sample
    senseFirst = constM senseI >>> arr (\x -> ((0, x), Just x))

    -- Sense: Remaining samples
    senseRest a = constM (sense True) >>> (arr id *** keepLast a)

    keepLast :: Monad m => a -> MSF m (Maybe a) a
    keepLast a = MSF $ \ma ->
      let a' = fromMaybe a ma
      in a' `seq` return (a', keepLast a')

    -- Consume/render
    actuateSF = arr (\x -> (True, x)) >>> arrM (uncurry actuate)

-- * Debugging / Step by step simulation

-- | Evaluate an SF, and return an output and an initialized SF.
--
-- /WARN/: Do not use this function for standard simulation. This function is
-- intended only for debugging/testing. Apart from being potentially slower and
-- consuming more memory, it also breaks the FRP abstraction by making samples
-- discrete and step based.
evalAtZero :: SF Identity a b -> a -> (b, SF Identity a b)
evalAtZero sf a = runIdentity $ runReaderT (unMSF sf a) 0

-- | Evaluate an initialized SF, and return an output and a continuation.
--
-- /WARN/: Do not use this function for standard simulation. This function is
-- intended only for debugging/testing. Apart from being potentially slower and
-- consuming more memory, it also breaks the FRP abstraction by making samples
-- discrete and step based.
evalAt :: SF Identity a b -> DTime -> a -> (b, SF Identity a b)
evalAt sf dt a = runIdentity $ runReaderT (unMSF sf a) dt

-- | Given a signal function and time delta, it moves the signal function into
-- the future, returning a new uninitialized SF and the initial output.
--
-- While the input sample refers to the present, the time delta refers to the
-- future (or to the time between the current sample and the next sample).
--
-- /WARN/: Do not use this function for standard simulation. This function is
-- intended only for debugging/testing. Apart from being potentially slower and
-- consuming more memory, it also breaks the FRP abstraction by making samples
-- discrete and step based.
evalFuture :: SF Identity a b -> a -> DTime -> (b, SF Identity a b)
evalFuture sf = flip (evalAt sf)
