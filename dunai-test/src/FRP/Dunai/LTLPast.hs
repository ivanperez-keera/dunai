{-# LANGUAGE Arrows #-}
-- | Past-time LTL using MSFs.
--
-- This module provides ways of defining past-, discrete-time temporal
-- predicates with MSFs.
--
-- There are two ways of doing so: piping the results of Boolean-carrying MSFs
-- into other MSFs (Past-time LTL using MSFs), or wrapping MSFs into other MSFs
-- (Past-time LTL as MSF combinators).
module FRP.Dunai.LTLPast where

import Control.Monad.Trans.MSF.Maybe
import Data.Maybe
import Data.MonadicStreamFunction

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
impliesSF = arr $ \(i,p) -> not i || p

-- ** Temporal MSFs

-- | Output True when every input up until the current time has been True.
--
-- This corresponds to Historically, or the past-time version of Globally
-- or Always.
sofarSF :: Monad m => MSF m Bool Bool
sofarSF = feedback True $ arr $ \(n,o) -> let n' = o && n in (n', n')

-- | Output True when at least one input up until the current time has been
-- True.
--
-- This corresponds to Ever, or the past-time version of Eventually.
everSF :: Monad m => MSF m Bool Bool
everSF = feedback False $ arr $ \(n,o) -> let n' = o || n in (n', n')

-- | Output True if the first element has always been True, or the second has
-- been True ever since the first one became False.
untilSF :: (Functor m, Monad m) => MSF m (Bool, Bool) Bool
untilSF =
    catchMaybe (untilMaybeB (feedback True $ arr cond))
               (snd ^>> sofarSF)

  where

    untilMaybeB :: Monad m => MSF m a (b, Bool) -> MSF (MaybeT m) a b
    untilMaybeB msf = proc a -> do
      (b,c) <- liftTransS msf -< a
      inMaybeT -< if c then Nothing else Just b

    cond ((i, u), o) = ((n, o && u), n)
      where
        n = o && i

-- | Output True if the input was True at the last time.
--
-- False at time zero.
lastSF :: Monad m => MSF m Bool Bool
lastSF = iPre False

-- data UnclearResult = Possibly Bool | Definitely Bool
--
-- causally :: SF a Bool -> SF a UnclearResult
-- causally = (>>> arr Definitely)
--
-- data TSF a = NonCausal (SF a UnclearResult)
--            | Causal    (SF a Bool)
--
-- evalTSF :: TSF a -> SignalSampleStream a -> Bool
-- evalTSF (Causal sf)    ss = firstSample $ fst $ evalSF sf ss
-- evalTSF (NonCausal sf) ss = clarifyResult $ lastSample $ fst $ evalSF sf ss
--
-- clarifyResult :: UnclearResult -> Bool
-- clarifyResult (Possibly x)   = x
-- clarifyResult (Definitely x) = x

-- * Past-time linear temporal logic as MSF combinators.

-- | A signal predicate is an MSF whose output is a Boolean value.
type SPred m a = MSF m a Bool

-- ** Propositional MSFs

-- | Output True at times when the input is False.
notSF' :: Monad m => SPred m a -> SPred m a
notSF' sf = sf >>> arr not

-- | Output True at times when both inputs are True.
andSF' :: Monad m => SPred m a -> SPred m a -> SPred m a
andSF' sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry (&&))

-- | Output True at times when at least one of the inputs is True.
orSF' :: Monad m => SPred m a -> SPred m a -> SPred m a
orSF' sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry (||))

-- | Output True at times when the first input stream is False or the second
-- one is True.
implySF' :: Monad m => SPred m a -> SPred m a -> SPred m a
implySF' sf1 sf2 = orSF' sf2 (notSF' sf1)

-- ** Temporal MSFs

-- | Output True at a time if the input has always been True up until that
-- time.
--
-- This corresponds to Historically, or the past-time version of Globally
-- or Always.
history' :: Monad m => SPred m a -> SPred m a
history' sf = feedback True $ proc (a, last) -> do
  b <- sf -< a
  let cur = last && b
  returnA -< (cur, cur)

-- | Output True at a time if the input has ever been True up until that
-- time.
--
-- This corresponds to Ever, or the past-time version of Eventually.
ever' :: Monad m => SPred m a -> SPred m a
ever' sf = feedback False $ proc (a, last) -> do
  b <- sf -< a
  let cur = last || b
  returnA -< (cur, cur)

-- | Output True at a time if the input at the last time was True.
prev' :: Monad m => SPred m a -> SPred m a
prev' = prev True

-- | Delay output of an MSF by one sample, using the provided argument for the
-- first sample.
{-# DEPRECATED prev "This function is deprecated in dunai-test 0.9 and will be removed." #-}
prev :: Monad m => b -> MSF m a b -> MSF m a b
prev b sf = feedback b $ proc (a, last) -> do
  b <- sf -< a
  returnA -< (last, b)
