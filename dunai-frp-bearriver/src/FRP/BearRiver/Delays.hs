-- |
-- Module      : FRP.BearRiver.Delays
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
-- SF primitives and combinators to delay signals, introducing new values in
-- them.
module FRP.BearRiver.Delays
    (
      -- * Basic delays
      pre
    , iPre
    , fby

      -- * Timed delays
    , delay
    )
  where

-- External imports
import Control.Arrow ((>>>))

-- Internal imports (dunai)
import Control.Monad.Trans.MSF                 (ask)
import Data.MonadicStreamFunction.InternalCore (MSF (..))

-- Internal imports
import FRP.BearRiver.Basic        (identity, (-->))
import FRP.BearRiver.InternalCore (SF (..), Time)
import FRP.BearRiver.Scan         (sscanPrim)

infixr 0 `fby`

-- * Delays

-- | Uninitialized delay operator.
--
-- The output has an infinitesimal delay (1 sample), and the value at time zero
-- is undefined.
pre :: Monad m => SF m a a
pre = sscanPrim f uninit uninit
  where
    f c a = Just (a, c)
    uninit = error "bearriver: pre: Uninitialized pre operator."

-- | Initialized delay operator.
--
-- Creates an SF that delays the input signal, introducing an infinitesimal
-- delay (one sample), using the given argument to fill in the initial output at
-- time zero.
iPre :: Monad m => a -> SF m a a
iPre = (--> pre)

-- | Lucid-Synchrone-like initialized delay (read "followed by").
--
-- Initialized delay combinator, introducing an infinitesimal delay (one sample)
-- in given 'SF', using the given argument to fill in the initial output at time
-- zero.
--
-- The difference with 'iPre' is that 'fby' takes an 'SF' as argument.
fby :: Monad m => b -> SF m a b -> SF m a b
b0 `fby` sf = b0 --> sf >>> pre

-- * Timed delays

-- | Delay a signal by a fixed time 't', using the second parameter to fill in
-- the initial 't' seconds.
delay :: Monad m => Time -> a -> SF m a a
delay q aInit | q < 0     = error "bearriver: delay: Negative delay."
              | q == 0    = identity
              | otherwise = MSF tf0
  where
    tf0 a0 = return (aInit, delayAux [] [(q, a0)] 0 aInit)

    -- Invariants:
    -- tDiff measure the time since the latest output sample ideally should have
    -- been output. Whenever that equals or exceeds the time delta for the next
    -- buffered sample, it is time to output a new sample (although not
    -- necessarily the one first in the queue: it might be necessary to "catch
    -- up" by discarding samples.  0 <= tDiff < bdt, where bdt is the buffered
    -- time delta for the sample on the front of the buffer queue.
    --
    -- Sum of time deltas in the queue >= q.
    delayAux _ [] _ _ = undefined
    delayAux rbuf buf@((bdt, ba) : buf') tDiff aPrev = MSF tf -- True
      where
        tf a = do
          dt <- ask
          let tDiff' = tDiff + dt
              rbuf'  = (dt, a) : rbuf
          if (tDiff' < bdt)
            then return (aPrev, delayAux rbuf' buf tDiff' aPrev)
            else nextSmpl rbuf' buf' (tDiff' - bdt) ba
          where

            nextSmpl rbuf [] tDiff a =
              nextSmpl [] (reverse rbuf) tDiff a
            nextSmpl rbuf buf@((bdt, ba) : buf') tDiff a
              | tDiff < bdt = return (a, delayAux rbuf buf tDiff a)
              | otherwise   = nextSmpl rbuf buf' (tDiff - bdt) ba
