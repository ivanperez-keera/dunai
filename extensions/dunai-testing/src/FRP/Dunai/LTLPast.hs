{-# LANGUAGE Arrows #-}
module FRP.Dunai.LTLPast where

------------------------------------------------------------------------------
import Data.MonadicStreamFunction

-- * SFs that implement temporal combinators

sofarSF :: Monad m => MSF m Bool Bool
sofarSF = feedback True $ arr $ \(n,o) -> let n' = o && n in (n', n')

everSF :: Monad m => MSF m Bool Bool
everSF = feedback False $ arr $ \(n,o) -> let n' = o || n in (n', n')

untilSF :: Monad m => MSF m (Bool, Bool) Bool
untilSF = switch
  (feedback True $ arr (\((i,u),o) -> let n = o && i
                                      in ((n, if (o && u) then Just () else Nothing), n)))
  (\_ -> arr snd >>> sofarSF)

lastSF :: Monad m => MSF m Bool Bool
lastSF = iPre False

andSF :: Monad m => MSF m (Bool, Bool) Bool
andSF = arr (uncurry (&&))

orSF :: Monad m => MSF m (Bool, Bool) Bool
orSF = arr (uncurry (||))

notSF :: Monad m => MSF m Bool Bool
notSF = arr not

impliesSF :: Monad m => MSF m (Bool, Bool) Bool
impliesSF = arr $ \(i,p) -> not i || p

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

-- * SF combinators that implement temporal combinators

type SPred m a = MSF m a Bool

notSF' :: Monad m => SPred m a -> SPred m a
notSF' sf = sf >>> arr (not)

andSF' :: Monad m => SPred m a -> SPred m a -> SPred m a
andSF' sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry (&&))

orSF' :: Monad m => SPred m a -> SPred m a -> SPred m a
orSF' sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry (||))

implySF' :: Monad m => SPred m a -> SPred m a -> SPred m a
implySF' sf1 sf2 = orSF' sf2 (notSF' sf1)

history' :: Monad m => SPred m a -> SPred m a
history' sf = feedback True $ proc (a, last) -> do
  b <- sf -< a
  let cur = last && b
  returnA -< (cur, cur)

ever' :: Monad m => SPred m a -> SPred m a
ever' sf = feedback False $ proc (a, last) -> do
  b <- sf -< a
  let cur = last || b
  returnA -< (cur, cur)

prev' :: Monad m => SPred m a -> SPred m a
prev' = prev True

prev :: Monad m => b -> MSF m a b -> MSF m a b
prev b sf = feedback b $ proc (a, last) -> do
  b <- sf -< a
  returnA -< (last, b)
