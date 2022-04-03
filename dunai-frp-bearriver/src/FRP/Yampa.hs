-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
module FRP.Yampa (module X, SF, FutureSF) where

-- External imports
import           Data.Functor.Identity

-- Internal imports
import           FRP.BearRiver as X hiding (SF, andThen)
import qualified FRP.BearRiver as BR

-- | Signal function (conceptually, a function between signals that respects
-- causality).
type SF       = BR.SF Identity

-- | Future signal function (conceptually, a function between fugure signals
-- that respects causality).
--
-- A future signal is a signal that is only defined for positive times.
type FutureSF = BR.SF Identity
