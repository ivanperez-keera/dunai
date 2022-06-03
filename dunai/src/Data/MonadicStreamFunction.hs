-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Monadic Stream Functions are synchronized stream functions with side
-- effects.
--
-- 'MSF's are defined by a function
-- @unMSF :: MSF m a b -> a -> m (b, MSF m a b)@
-- that executes one step of a simulation, and produces an output in a monadic
-- context, and a continuation to be used for future steps.
--
-- See the module "Data.MonadicStreamFunction.Core" for details.
--
-- 'MSF's are a generalisation of the implementation mechanism used by Yampa,
-- Wormholes and other FRP and reactive implementations.
--
-- When combined with different monads, they produce interesting effects. For
-- example, when combined with the 'Maybe' monad, they become transformations
-- that may stop producing outputs (and continuations). The 'Either' monad
-- gives rise to 'MSF's that end with a result (akin to Tasks in Yampa, and
-- Monadic FRP).
--
-- Flattening, that is, going from some structure @MSF (t m) a b@ to @MSF m a
-- b@ for a specific transformer @t@ often gives rise to known FRP constructs.
-- For instance, flattening with 'EitherT' gives rise to switching, and
-- flattening with 'ListT' gives rise to parallelism with broadcasting.
--
-- 'MSF's can be used to implement many FRP variants, including Arrowized FRP,
-- Classic FRP, and plain reactive programming. Arrowized and applicative
-- syntax are both supported.
--
-- For a very detailed introduction to 'MSF's, see:
-- <http://dl.acm.org/citation.cfm?id=2976010>
-- (mirror: <http://www.cs.nott.ac.uk/~psxip1/#FRPRefactored>).
--
-- Apart from the modules exported, this module exports instances from:
--
-- - "Data.MonadicStreamFunction.Instances.ArrowChoice"
-- - "Data.MonadicStreamFunction.Instances.ArrowLoop"
-- - "Data.MonadicStreamFunction.Instances.ArrowPlus"
module Data.MonadicStreamFunction
  ( module Control.Arrow
  , module Data.MonadicStreamFunction.Core
  , module Data.MonadicStreamFunction.Util
  )
 where

-- External imports
import Control.Arrow

-- Internal imports
import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.Util

-- Internal imports (instances)
import Data.MonadicStreamFunction.Instances.ArrowChoice ()
import Data.MonadicStreamFunction.Instances.ArrowLoop   ()
import Data.MonadicStreamFunction.Instances.ArrowPlus   ()
