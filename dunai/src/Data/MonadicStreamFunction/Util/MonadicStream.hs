-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Useful auxiliary functions and definitions.
module Data.MonadicStreamFunction.Util.MonadicStream where

-- Internal imports
import Data.MonadicStreamFunction.Core (MSF)

-- * Streams and sinks

-- | A stream is an 'MSF' that produces outputs, while ignoring the input. It
-- can obtain the values from a monadic context.
type MStream m a = MSF m () a

-- | A sink is an 'MSF' that consumes inputs, while producing no output. It
-- can consume the values with side effects.
type MSink m a = MSF m a ()
