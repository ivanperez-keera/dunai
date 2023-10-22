{-# LANGUAGE Arrows #-}
-- |
-- Copyright  : (c) Ivan Perez, 2017
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Past-time LTL using MSFs.
--
-- This module provides ways of defining past-, discrete-time temporal
-- predicates with MSFs.
--
-- There are two ways of doing so: piping the results of Boolean-carrying MSFs
-- into other MSFs (Past-time LTL using MSFs), or wrapping MSFs into other MSFs
-- (Past-time LTL as MSF combinators).
module FRP.Dunai.LTLPast where

-- External imports
import Control.Monad.Trans.MSF.Maybe (MaybeT, catchMaybe, inMaybeT)
import Data.MonadicStreamFunction    (MSF, arr, feedback, iPre, liftTransS,
                                      returnA, (&&&), (>>>), (^>>))

-- * Past-time linear temporal logic using MSFs.

-- ** Propositional MSFs

-- | Output True when both inputs are True.
andSF :: Monad m => MSF m (Bool, Bool) Bool
andSF = arr (uncurry (&&))

-- | Output True when at least one input is True.
orSF :: Monad m => MSF m (Bool, Bool) Bool
orSF = arr (uncurry (||))

-- | Output True when the input is False.
notSF :: Monad m => MSF m Bool Bool
notSF = arr not

-- | Output True when the second input is True or the first one is False.
impliesSF :: Monad m => MSF m (Bool, Bool) Bool
impliesSF = arr $ \(i, p) -> not i || p

-- ** Temporal MSFs

-- | Output True when every input up until the current time has been True.
--
-- This corresponds to Historically, or the past-time version of Globally or
-- Always.
sofarSF :: Monad m => MSF m Bool Bool
sofarSF = feedback True $ arr $ \(n, o) -> let n' = o && n in (n', n')

-- | Output True when at least one input up until the current time has been
-- True.
--
-- This corresponds to Ever, or the past-time version of Eventually.
everSF :: Monad m => MSF m Bool Bool
everSF = feedback False $ arr $ \(n, o) -> let n' = o || n in (n', n')

-- | Output True if the first element has always been True, or the second has
-- been True ever since the first one became False.
untilSF :: (Functor m, Monad m) => MSF m (Bool, Bool) Bool
untilSF =
    catchMaybe (untilMaybeB (feedback True $ arr cond))
               (snd ^>> sofarSF)

  where

    untilMaybeB :: Monad m => MSF m a (b, Bool) -> MSF (MaybeT m) a b
    untilMaybeB msf = proc a -> do
      (b, c) <- liftTransS msf -< a
      inMaybeT -< if c then Nothing else Just b

    cond ((i, u), o) = ((n, o && u), n)
      where
        n = o && i

-- | Output True if the input was True at the last time.
--
-- False at time zero.
lastSF :: Monad m => MSF m Bool Bool
lastSF = iPre False

-- * Past-time linear temporal logic as MSF combinators.

-- | A signal predicate is an MSF whose output is a Boolean value.
type SPred m a = MSF m a Bool

-- ** Propositional MSFs

-- | Output True at times when the input is False.
{-# DEPRECATED notSF' "Use notSF instead" #-}
notSF' :: Monad m => SPred m a -> SPred m a
notSF' sf = sf >>> arr not

-- | Output True at times when both inputs are True.
{-# DEPRECATED andSF' "Use andSF instead" #-}
andSF' :: Monad m => SPred m a -> SPred m a -> SPred m a
andSF' sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry (&&))

-- | Output True at times when at least one of the inputs is True.
{-# DEPRECATED orSF' "Use orSF instead" #-}
orSF' :: Monad m => SPred m a -> SPred m a -> SPred m a
orSF' sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry (||))

-- | Output True at times when the first input stream is False or the second
-- one is True.
{-# DEPRECATED implySF' "Use impliesSF instead" #-}
implySF' :: Monad m => SPred m a -> SPred m a -> SPred m a
implySF' sf1 sf2 = orSF' sf2 (notSF' sf1)

-- ** Temporal MSFs

-- | Output True at a time if the input has always been True up until that
-- time.
--
-- This corresponds to Historically, or the past-time version of Globally or
-- Always.
{-# DEPRECATED history' "Use sofarSF instead" #-}
history' :: Monad m => SPred m a -> SPred m a
history' sf = feedback True $ proc (a, last) -> do
  b <- sf -< a
  let cur = last && b
  returnA -< (cur, cur)

-- | Output True at a time if the input has ever been True up until that time.
--
-- This corresponds to Ever, or the past-time version of Eventually.
{-# DEPRECATED ever' "Use everSF instead" #-}
ever' :: Monad m => SPred m a -> SPred m a
ever' sf = feedback False $ proc (a, last) -> do
  b <- sf -< a
  let cur = last || b
  returnA -< (cur, cur)

-- | Output True at a time if the input at the last time was True.
{-# DEPRECATED prev' "Use lastSF instead" #-}
prev' :: Monad m => SPred m a -> SPred m a
prev' sf = feedback True $ proc (a, last) -> do
  b <- sf -< a
  returnA -< (last, b)
