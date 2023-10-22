-- |
-- Module      : FRP.Yampa.Scan
-- Copyright   : (c) Ivan Perez, 2014-2023
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Simple, stateful signal processing.
--
-- Scanning implements elementary, step-based accumulating over signal functions
-- by means of an auxiliary function applied to each input and to an
-- accumulator. For comparison with other FRP libraries and with stream
-- processing abstractions, think of fold.
module FRP.BearRiver.Scan
    ( sscan
    , sscanPrim
    )
  where

-- Internal imports (dunai)
import Data.MonadicStreamFunction.InternalCore (MSF (..))

-- Internal imports
import FRP.BearRiver.InternalCore (SF (..))

-- * Simple, stateful signal processing

-- | Applies a function point-wise, using the last output as next input. This
-- creates a well-formed loop based on a pure, auxiliary function.
sscan :: Monad m => (b -> a -> b) -> b -> SF m a b
sscan f bInit = sscanPrim f' bInit bInit
  where
    f' b a = Just (b', b')
      where
        b' = f b a

-- | Generic version of 'sscan', in which the auxiliary function produces an
-- internal accumulator and an "held" output.
--
-- Applies a function point-wise, using the last known 'Just' output to form the
-- output, and next input accumulator. If the output is 'Nothing', the last
-- known accumulators are used. This creates a well-formed loop based on a pure,
-- auxiliary function.
sscanPrim :: Monad m => (c -> a -> Maybe (c, b)) -> c -> b -> SF m a b
sscanPrim f cInit bInit = MSF $ \a -> do
  let o = f cInit a
  case o of
    Nothing       -> return (bInit, sscanPrim f cInit bInit)
    Just (c', b') -> return (b',    sscanPrim f c' b')
