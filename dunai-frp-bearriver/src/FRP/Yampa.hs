-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
module FRP.Yampa (module X, SF, FutureSF, embed) where

-- External imports
import Data.Functor.Identity (Identity, runIdentity)

-- Internal imports
import           FRP.BearRiver as X hiding (FutureSF, SF, embed)
import qualified FRP.BearRiver as BR

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
