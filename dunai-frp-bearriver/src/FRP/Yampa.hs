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

type SF       = BR.SF Identity
type FutureSF = BR.SF Identity
