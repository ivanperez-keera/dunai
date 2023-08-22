-- |
-- Module      : FRP.BearRiver.Basic
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
-- Defines basic signal functions, and elementary ways of altering them.
--
-- This module defines very basic ways of creating and modifying signal
-- functions. In particular, it defines ways of creating constant output
-- producing SFs, and SFs that just pass the signal through unmodified.
--
-- It also defines ways of altering the input and the output signal only by
-- inserting one value in the signal, or by transforming it.
module FRP.BearRiver.Basic
    (
      -- * Basic signal functions
      identity
    , constant

      -- ** Initialization
    , (-->)
    , (-:>)
    , (>--)
    , (-=>)
    , (>=-)
    , initially
    )
  where

-- External imports
import qualified Control.Category as Category

-- Internal imports (dunai)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- Internal imports
import FRP.BearRiver.InternalCore (SF, arr)

infixr 0 -->, -:>, >--, -=>, >=-

-- * Basic signal functions

-- | Identity: identity = arr id
--
-- Using 'identity' is preferred over lifting id, since the arrow combinators
-- know how to optimise certain networks based on the transformations being
-- applied.
identity :: Monad m => SF m a a
identity = Category.id

-- | Identity: constant b = arr (const b)
--
-- Using 'constant' is preferred over lifting const, since the arrow combinators
-- know how to optimise certain networks based on the transformations being
-- applied.
constant :: Monad m => b -> SF m a b
constant = arr . const

-- * Initialization

-- | Initialization operator (cf. Lustre/Lucid Synchrone).
--
-- The output at time zero is the first argument, and from that point on it
-- behaves like the signal function passed as second argument.
(-->) :: Monad m => b -> SF m a b -> SF m a b
b0 --> sf = MSF $ \a -> do
  (_b, sf') <- unMSF sf a
  return (b0, sf')

-- | Output pre-insert operator.
--
-- Insert a sample in the output, and from that point on, behave like the given
-- sf.
(-:>) :: Monad m => b -> SF m a b -> SF m a b
b -:> sf = MSF $ \_a -> return (b, sf)

-- | Input initialization operator.
--
-- The input at time zero is the first argument, and from that point on it
-- behaves like the signal function passed as second argument.
(>--) :: Monad m => a -> SF m a b -> SF m a b
a0 >-- sf = MSF $ \_ -> unMSF sf a0

-- | Transform initial output value.
--
-- Applies a transformation 'f' only to the first output value at time zero.
(-=>) :: Monad m => (b -> b) -> SF m a b -> SF m a b
f -=> sf = MSF $ \a -> do
  (b, sf') <- unMSF sf a
  return (f b, sf')

-- | Transform initial input value.
--
-- Applies a transformation 'f' only to the first input value at time zero.
(>=-) :: Monad m => (a -> a) -> SF m a b -> SF m a b
f >=- sf = MSF $ \a -> do
  (b, sf') <- unMSF sf (f a)
  return (b, sf')

-- | Override initial value of input signal.
initially :: Monad m => a -> SF m a a
initially = (--> identity)
