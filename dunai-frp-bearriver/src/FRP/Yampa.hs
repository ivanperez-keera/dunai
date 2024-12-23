-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
module FRP.Yampa (module X, SF, FutureSF, embed, reactimate) where

-- External imports
import Control.Monad.Reader  (mapReaderT)
import Data.Functor.Identity (Identity, runIdentity)

-- Internal imports
import           FRP.BearRiver      as X hiding (FutureSF, SF, embed, loopPre,
                                          reactimate)
import qualified FRP.BearRiver      as BR
import           FRP.BearRiver.Loop as X

-- | Signal function (conceptually, a function between signals that respects
-- causality).
type SF = BR.SF Identity

-- | Future signal function (conceptually, a function between future signals
-- that respects causality).
--
-- A future signal is a signal that is only defined for positive times.
type FutureSF = BR.SF Identity

-- | Given a signal function and a pair with an initial input sample for the
-- input signal, and a list of sampling times, possibly with new input samples
-- at those times, it produces a list of output samples.
--
-- This is a simplified, purely-functional version of 'reactimate'.
embed :: SF a b -> (a, [(DTime, Maybe a)]) -> [b]
embed sf = runIdentity . BR.embed sf

-- * Reactimation

-- | Convenience function to run a signal function indefinitely, using actions
-- to obtain new input and process the output.
--
-- This function first runs the initialization action, which provides the
-- initial input for the signal transformer at time 0.
--
-- Afterwards, an input sensing action is used to obtain new input (if any) and
-- the time since the last iteration. The argument to the input sensing function
-- indicates if it can block. If no new input is received, it is assumed to be
-- the same as in the last iteration.
--
-- After applying the signal function to the input, the actuation action is
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
           => m a                          -- ^ Initialization action
           -> (Bool -> m (DTime, Maybe a)) -- ^ Input sensing action
           -> (Bool -> b -> m Bool)        -- ^ Actuation (output processing)
                                           --   action
           -> SF a b                       -- ^ Signal function
           -> m ()
reactimate senseI sense actuate sf =
    BR.reactimate senseI sense actuate (foldIdentity sf)
  where
    foldIdentity :: Monad m => SF a b -> BR.SF m a b
    foldIdentity = morphS $ mapReaderT $ return . runIdentity
