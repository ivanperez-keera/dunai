-- |
-- Module      : FRP.Yampa
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
-- Apply SFs only under certain conditions.
module FRP.BearRiver.Conditional
    (
      -- * Guards and automata-oriented combinators
      provided

      -- * Variable pause
    , pause
    )
  where

-- External imports
import Control.Arrow ((&&&), (^>>))

import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- Internal imports
import FRP.BearRiver.Basic        (constant)
import FRP.BearRiver.EventS       (edge, snap)
import FRP.BearRiver.InternalCore (SF (..))
import FRP.BearRiver.Switches     (switch)

-- * Guards and automata-oriented combinators

-- | Runs a signal function only when a given predicate is satisfied, otherwise
-- runs the other signal function.
--
-- This is similar to 'ArrowChoice', except that this resets the SFs after each
-- transition.
--
-- For example, the following integrates the incoming input numbers, using one
-- integral if the numbers are even, and another if the input numbers are odd.
-- Note how, every time we "switch", the old value of the integral is discarded.
--
-- >>> embed (provided (even . round) integral integral) (deltaEncode 1 [1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2 :: Double])
-- [0.0,1.0,2.0,0.0,2.0,4.0,0.0,1.0,2.0,0.0,2.0,4.0]
provided :: Monad m => (a -> Bool) -> SF m a b -> SF m a b -> SF m a b
provided p sft sff =
    switch (constant undefined &&& snap) $ \a0 ->
      if p a0 then stt else stf
  where
    stt = switch (sft &&& (not . p ^>> edge)) (const stf)
    stf = switch (sff &&& (p ^>> edge)) (const stt)

-- * Variable pause

-- | Given a value in an accumulator (b), a predicate signal function (sfC),
-- and a second signal function (sf), pause will produce the accumulator b if
-- sfC input is True, and will transform the signal using sf otherwise. It acts
-- as a pause with an accumulator for the moments when the transformation is
-- paused.
pause :: Monad m => b -> SF m a Bool -> SF m a b -> SF m a b
pause b sfC sf = MSF $ \a0 -> do
   (p, sfC') <- unMSF sfC a0
   case p of
     True  -> return (b, pause b sfC' sf)
     False -> do (b', sf') <- unMSF sf a0
                 return (b', pause b' sfC' sf')
