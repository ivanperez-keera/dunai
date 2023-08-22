-- |
-- Module      : FRP.Dunai.InternalCore
-- Copyright   : (c) Ivan Perez, 2014-2022
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Domain-specific language embedded in Haskell for programming hybrid (mixed
-- discrete-time and continuous-time) systems, extended with Monads.
--
-- Bearriver (a tributary to the Yampa river) provides the same API as Yampa,
-- but implemented using Monadic Stream Functions underneath. SFs in BearRiver
-- take an additional monad as argument. The introduction of time is done by
-- means of an additional Reader layer.
module FRP.BearRiver.InternalCore
    ( module Control.Arrow

      -- * Basic definitions
      -- ** Time
    , Time
    , DTime

      -- ** Signal Functions
    , SF
    , ClockInfo
    )
  where

-- External imports
import Control.Arrow (Arrow (..), ArrowChoice (..), ArrowLoop (..), (>>>))

-- Internal imports (dunai)
import Control.Monad.Trans.MSF    (ReaderT)
import Data.MonadicStreamFunction (MSF)

-- * Basic type definitions with associated utilities

-- | Time is used both for time intervals (duration), and time w.r.t. some
-- agreed reference point in time.
type Time = Double

-- | DTime is the time type for lengths of sample intervals. Conceptually,
-- DTime = R+ = { x in R | x > 0 }. Don't assume Time and DTime have the same
-- representation.
type DTime = Double

-- | Extensible signal function (signal function with a notion of time, but
-- which can be extended with actions).
--
-- Signal function that transforms a signal carrying values of some type 'a'
-- into a signal carrying values of some type 'b'. You can think of it as
-- (Signal a -> Signal b). A signal is, conceptually, a function from 'Time' to
-- value.
type SF m = MSF (ClockInfo m)

-- | Information on the progress of time.
type ClockInfo m = ReaderT DTime m
