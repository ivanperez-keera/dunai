{-# LANGUAGE CPP #-}
-- The following warning is disabled so that we do not see warnings due to
-- using ListT on an MSF to implement parallelism with broadcasting.
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#else
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif

-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Switches allow you to change the signal function being applied.
--
-- The basic idea of switching is formed by combining a subordinate signal
-- function and a signal function continuation parameterised over some initial
-- data.
module FRP.BearRiver.Switches
    (
      -- * Basic switching
      switch,  dSwitch

      -- * Parallel composition\/switching (collections)
      -- ** With broadcasting
    , parB
    , dpSwitchB

      -- * Parallel composition\/switching (lists)


      -- ** With replication
    , parC
    )
  where

-- External imports
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif
import Data.Traversable as T

-- Internal imports (dunai)
import Control.Monad.Trans.MSF                 (local)
import Control.Monad.Trans.MSF.List            (sequenceS, widthFirst)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- Internal imports
import FRP.BearRiver.Event        (Event (..))
import FRP.BearRiver.InternalCore (SF)

-- * Basic switches

-- | Basic switch.
--
-- By default, the first signal function is applied. Whenever the second value
-- in the pair actually is an event, the value carried by the event is used to
-- obtain a new signal function to be applied *at that time and at future
-- times*. Until that happens, the first value in the pair is produced in the
-- output signal.
--
-- Important note: at the time of switching, the second signal function is
-- applied immediately. If that second SF can also switch at time zero, then a
-- double (nested) switch might take place. If the second SF refers to the
-- first one, the switch might take place infinitely many times and never be
-- resolved.
--
-- Remember: The continuation is evaluated strictly at the time
-- of switching!
switch :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
switch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (_, Event c) -> local (const 0) (unMSF (sfC c) a)
    (b, NoEvent) -> return (b, switch ct sfC)

-- | Switch with delayed observation.
--
-- By default, the first signal function is applied.
--
-- Whenever the second value in the pair actually is an event, the value
-- carried by the event is used to obtain a new signal function to be applied
-- *at future times*.
--
-- Until that happens, the first value in the pair is produced in the output
-- signal.
--
-- Important note: at the time of switching, the second signal function is used
-- immediately, but the current input is fed by it (even though the actual
-- output signal value at time 0 is discarded).
--
-- If that second SF can also switch at time zero, then a double (nested)
-- switch might take place. If the second SF refers to the first one, the
-- switch might take place infinitely many times and never be resolved.
--
-- Remember: The continuation is evaluated strictly at the time
-- of switching!
dSwitch :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
dSwitch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (b, Event c) -> do (_, ct') <- local (const 0) (unMSF (sfC c) a)
                       return (b, ct')
    (b, NoEvent) -> return (b, dSwitch ct sfC)

-- * Parallel composition and switching

-- ** Parallel composition and switching over collections with broadcasting

#if MIN_VERSION_base(4,8,0)
parB :: Monad m => [SF m a b] -> SF m a [b]
#else
parB :: (Functor m, Monad m) => [SF m a b] -> SF m a [b]
#endif
-- ^ Spatial parallel composition of a signal function collection. Given a
-- collection of signal functions, it returns a signal function that broadcasts
-- its input signal to every element of the collection, to return a signal
-- carrying a collection of outputs. See 'par'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
parB = widthFirst . sequenceS

-- | Decoupled parallel switch with broadcasting (dynamic collection of signal
-- functions spatially composed in parallel). See 'dpSwitch'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
dpSwitchB :: (Functor m, Monad m, Traversable col)
          => col (SF m a b)
          -> SF m (a, col b) (Event c)
          -> (col (SF m a b) -> c -> SF m a (col b))
          -> SF m a (col b)
dpSwitchB sfs sfF sfCs = MSF $ \a -> do
  res <- T.mapM (`unMSF` a) sfs
  let bs   = fmap fst res
      sfs' = fmap snd res
  (e, sfF') <- unMSF sfF (a, bs)
  ct <- case e of
          Event c -> snd <$> unMSF (sfCs sfs c) a
          NoEvent -> return (dpSwitchB sfs' sfF' sfCs)
  return (bs, ct)

-- ** Parallel composition over collections

-- | Apply an SF to every element of a list.
--
-- Example:
--
-- >>> embed (parC integral) (deltaEncode 0.1 [[1, 2], [2, 4], [3, 6], [4.0, 8.0 :: Float]])
-- [[0.0,0.0],[0.1,0.2],[0.3,0.6],[0.6,1.2]]
--
-- The number of SFs or expected inputs is determined by the first input list,
-- and not expected to vary over time.
--
-- If more inputs come in a subsequent list, they are ignored.
--
-- >>> embed (parC (arr (+1))) (deltaEncode 0.1 [[0], [1, 1], [3, 4], [6, 7, 8], [1, 1], [0, 0], [1, 9, 8]])
-- [[1],[2],[4],[7],[2],[1],[2]]
--
-- If less inputs come in a subsequent list, an exception is thrown.
--
-- >>> embed (parC (arr (+1))) (deltaEncode 0.1 [[0, 0], [1, 1], [3, 4], [6, 7, 8], [1, 1], [0, 0], [1, 9, 8]])
-- [[1,1],[2,2],[4,5],[7,8],[2,2],[1,1],[2,10]]
parC :: Monad m => SF m a b -> SF m [a] [b]
parC = parC0
  where
    parC0 :: Monad m => SF m a b -> SF m [a] [b]
    parC0 sf0 = MSF $ \as -> do
      os <- T.mapM (\(a, sf) -> unMSF sf a) $
              zip as (replicate (length as) sf0)

      let bs  = fmap fst os
          cts = fmap snd os
      return (bs, parC' cts)

    parC' :: Monad m => [SF m a b] -> SF m [a] [b]
    parC' sfs = MSF $ \as -> do
      os <- T.mapM (\(a, sf) -> unMSF sf a) $ zip as sfs
      let bs  = fmap fst os
          cts = fmap snd os
      return (bs, parC' cts)
