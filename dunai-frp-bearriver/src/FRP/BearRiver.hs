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
import Data.Functor.Identity (Identity (..))
import Data.Maybe            (fromMaybe)
import Data.VectorSpace      as X

-- Internal imports (dunai)
import           Control.Monad.Trans.MSF                 hiding (dSwitch)
import qualified Control.Monad.Trans.MSF                 as MSF
import           Data.MonadicStreamFunction              as X hiding (count,
                                                               iPre, once,
                                                               reactimate,
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
import           FRP.BearRiver.Hybrid                    as X
import           FRP.BearRiver.Integration               as X
import           FRP.BearRiver.InternalCore              as X
import           FRP.BearRiver.Scan                      as X
import           FRP.BearRiver.Switches                  as X
import           FRP.BearRiver.Time                      as X

-- Internal imports (dunai, instances)
import Data.MonadicStreamFunction.Instances.ArrowLoop () -- not needed, just
                                                         -- re-exported

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

-- * State keeping combinators

-- ** Loops with guaranteed well-defined feedback

-- | Loop with an initial value for the signal being fed back.
loopPre :: Monad m => c -> SF m (a, c) (b, c) -> SF m a b
loopPre = feedback

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
