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
-- 'MSF's are a generalisation of the implementation mechanism used by Yampa,
-- Wormholes and other FRP and reactive implementations.
--
-- This modules defines only the minimal core. By default, you should import
-- "Data.MonadicStreamFunction.Core" or "Data.MonadicStreamFunction" whenever
-- possible, and define your functions without accessing the MSF constuctor.
-- Those modules, as well as other modules in dunai, also provide convenient
-- instances. This module may be useful if you are extending dunai with
-- functionality that cannot be (conveniently) expressed using the existing
-- high-level API.

-- NOTE TO IMPLEMENTORS:
--
-- This module contains the core. Only the core. It should be possible to
-- define every function and type outside this module, except for the instances
-- for ArrowLoop, ArrowChoice, etc., without access to the internal constructor
-- for MSF and the function 'unMSF'.
--
-- It's very hard to know what IS essential to framework and if we start adding
-- all the functions and instances that *may* be useful in one module.
--
-- By separating some instances and functions in other modules, we can easily
-- understand what is the essential idea and then analyse how it is affected by
-- an extension. It also helps demonstrate that something works for MSFs +
-- ArrowChoice, or MSFs + ArrowLoop, etc.
--
-- To address potential violations of basic design principles (like 'not having
-- orphan instances'), the main module Data.MonadicStreamFunction exports
-- everything. Users should *never* import this module here individually, but
-- the main module instead.
module Data.MonadicStreamFunction.InternalCore
    {-# DEPRECATED "Use Data.MonadicStreamFunction.Core.Internal instead" #-}
    ( MSF(..)
    , morphGS
    , feedback
    , embed
    , reactimate
    )
  where

-- Internal imports
import Data.MonadicStreamFunction.Core.Internal
