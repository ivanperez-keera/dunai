{-# LANGUAGE Arrows     #-}
{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
module FRP.BearRiver
  (module FRP.BearRiver, module X)
 where
-- This is an implementation of Yampa using our Monadic Stream Processing
-- library. We focus only on core Yampa. We will use this module later to
-- reimplement an example of a Yampa system.
--
-- While we may not introduce all the complexity of Yampa today (all kinds of
-- switches, etc.) our goal is to show that the approach is promising and that
-- there do not seem to exist any obvious limitations.

import           Control.Applicative
import           Control.Arrow                                  as X
import qualified Control.Category                               as Category
import           Control.Monad                                  (mapM)
import           Control.Monad.Random
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.MSF                        hiding (switch)
import qualified Control.Monad.Trans.MSF                        as MSF
import           Control.Monad.Trans.MSF.Except                 as MSF hiding
                                                                        (switch)
import           Control.Monad.Trans.MSF.List                   (sequenceS,
                                                                 widthFirst)
import           Control.Monad.Trans.MSF.Random
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MonadicStreamFunction                     as X hiding (reactimate,
                                                                      repeatedly,
                                                                      sum,
                                                                      switch,
                                                                      trace)
import qualified Data.MonadicStreamFunction                     as MSF
import           Data.MonadicStreamFunction.Instances.ArrowLoop
import           Data.MonadicStreamFunction.InternalCore
import           Data.Traversable                               as T
import           Data.VectorSpace                               as X

infixr 0 -->, -:>, >--, >=-

-- * Basic definitions

type Time  = Double

type DTime = Double

type SF m        = MSF (ClockInfo m)

type ClockInfo m = ReaderT DTime m

data Event a = Event a | NoEvent
 deriving (Eq, Show)

-- | The type 'Event' is isomorphic to 'Maybe'. The 'Functor' instance of
-- 'Event' is analogous to the 'Functo' instance of 'Maybe', where the given
-- function is applied to the value inside the 'Event', if any.
instance Functor Event where
  fmap _ NoEvent   = NoEvent
  fmap f (Event c) = Event (f c)

-- | The type 'Event' is isomorphic to 'Maybe'. The 'Applicative' instance of
-- 'Event' is analogous to the 'Applicative' instance of 'Maybe', where the
-- lack of a value (i.e., 'NoEvent') causes '(<*>)' to produce no value
-- ('NoEvent').
instance Applicative Event where
  pure = Event

  Event f <*> Event x = Event (f x)
  _       <*> _       = NoEvent

-- | The type 'Event' is isomorphic to 'Maybe'. The 'Monad' instance of 'Event'
-- is analogous to the 'Monad' instance of 'Maybe', where the lack of a value
-- (i.e., 'NoEvent') causes bind to produce no value ('NoEvent').
instance Monad Event where
  return = pure

  Event x >>= f = f x
  NoEvent >>= _ = NoEvent

-- ** Lifting
arrPrim :: Monad m => (a -> b) -> SF m a b
arrPrim = arr

arrEPrim :: Monad m => (Event a -> b) -> SF m (Event a) b
arrEPrim = arr

-- * Signal functions

-- ** Basic signal functions

identity :: Monad m => SF m a a
identity = Category.id

constant :: Monad m => b -> SF m a b
constant = arr . const

localTime :: Monad m => SF m a Time
localTime = constant 1.0 >>> integral

time :: Monad m => SF m a Time
time = localTime

-- ** Initialization

-- | Initialization operator (cf. Lustre/Lucid Synchrone).
--
-- The output at time zero is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.
(-->) :: Monad m => b -> SF m a b -> SF m a b
b0 --> sf = sf >>> replaceOnce b0

-- | Output pre-insert operator.
--
-- Insert a sample in the output, and from that point on, behave
-- like the given sf.
(-:>) :: Monad m => b -> SF m a b -> SF m a b
b -:> sf = iPost b sf

-- | Input initialization operator.
--
-- The input at time zero is the first argument, and from
-- that point on it behaves like the signal function passed as
-- second argument.
(>--) :: Monad m => a -> SF m a b -> SF m a b
a0 >-- sf = replaceOnce a0 >>> sf

(>=-) :: Monad m => (a -> a) -> SF m a b -> SF m a b
f >=- sf = MSF $ \a -> do
  (b, sf') <- unMSF sf (f a)
  return (b, sf')

initially :: Monad m => a -> SF m a a
initially = (--> identity)

-- * Simple, stateful signal processing
sscan :: Monad m => (b -> a -> b) -> b -> SF m a b
sscan f b_init = feedback b_init u
  where u = undefined -- (arr f >>^ dup)

sscanPrim :: Monad m => (c -> a -> Maybe (c, b)) -> c -> b -> SF m a b
sscanPrim f c_init b_init = MSF $ \a -> do
  let o = f c_init a
  case o of
    Nothing       -> return (b_init, sscanPrim f c_init b_init)
    Just (c', b') -> return (b',     sscanPrim f c' b')


-- | Event source that never occurs.
never :: Monad m => SF m a (Event b)
never = constant NoEvent

-- | Event source with a single occurrence at time 0. The value of the event
-- is given by the function argument.
now :: Monad m => b -> SF m a (Event b)
now b0 = Event b0 --> never

after :: Monad m
      => Time -- ^ The time /q/ after which the event should be produced
      -> b    -- ^ Value to produce at that time
      -> SF m a (Event b)
after q x = feedback q go
 where go = MSF $ \(_, t) -> do
              dt <- ask
              let t' = t - dt
                  e  = if t > 0 && t' < 0 then Event x else NoEvent
                  ct = if t' < 0 then constant (NoEvent, t') else go
              return ((e, t'), ct)

repeatedly :: Monad m => Time -> b -> SF m a (Event b)
repeatedly q x
    | q > 0     = afterEach $ qxs q
    | otherwise = error "bearriver: repeatedly: Non-positive period."
  where
    qxs q'= (q',x):qxs (q' + q)

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.

-- After all, after, repeatedly etc. are defined in terms of afterEach.
afterEach :: Monad m => [(Time,b)] -> SF m a (Event b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)

-- | Event source with consecutive occurrences at the given intervals.
-- Should more than one event be scheduled to occur in any sampling interval,
-- the output list will contain all events produced during that interval.
afterEachCat :: Monad m => [(Time,b)] -> SF m a (Event [b])
afterEachCat = afterEachCat' 0
  where
    afterEachCat' :: Monad m => Time -> [(Time,b)] -> SF m a (Event [b])
    afterEachCat' _ []  = never
    afterEachCat' t qxs = MSF $ \_ -> do
      dt <- ask
      let t' = t + dt
          (qxsNow, qxsLater) = span (\p -> fst p <= t') qxs
          ev = if null qxsNow then NoEvent else Event (map snd qxsNow)
      return (ev, afterEachCat' t' qxsLater)


-- * Events

-- | Apply an 'MSF' to every input. Freezes temporarily if the input is
-- 'NoEvent', and continues as soon as an 'Event' is received.
mapEventS :: Monad m => MSF m a b -> MSF m (Event a) (Event b)
mapEventS msf = proc eventA -> case eventA of
  Event a -> arr Event <<< msf -< a
  NoEvent -> returnA           -< NoEvent

-- ** Relation to other types

eventToMaybe = event Nothing Just

boolToEvent :: Bool -> Event ()
boolToEvent True  = Event ()
boolToEvent False = NoEvent

-- * Hybrid SF m combinators

edge :: Monad m => SF m Bool (Event ())
edge = edgeFrom True

iEdge :: Monad m => Bool -> SF m Bool (Event ())
iEdge = edgeFrom

-- | Like 'edge', but parameterized on the tag value.
--
-- From Yampa
edgeTag :: Monad m => a -> SF m Bool (Event a)
edgeTag a = edge >>> arr (`tag` a)

-- | Edge detector particularized for detecting transtitions
--   on a 'Maybe' signal from 'Nothing' to 'Just'.
--
-- From Yampa

-- !!! 2005-07-09: To be done or eliminated
-- !!! Maybe could be kept as is, but could be easy to implement directly
-- !!! in terms of sscan?
edgeJust :: Monad m => SF m (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
    where
        isJustEdge Nothing  Nothing     = Nothing
        isJustEdge Nothing  ma@(Just _) = ma
        isJustEdge (Just _) (Just _)    = Nothing
        isJustEdge (Just _) Nothing     = Nothing

edgeBy :: Monad m => (a -> a -> Maybe b) -> a -> SF m a (Event b)
edgeBy isEdge a_prev = MSF $ \a ->
  return (maybeToEvent (isEdge a_prev a), edgeBy isEdge a)

maybeToEvent :: Maybe a -> Event a
maybeToEvent = maybe NoEvent Event

edgeFrom :: Monad m => Bool -> SF m Bool (Event())
edgeFrom prev = MSF $ \a -> do
  let res | prev      = NoEvent
          | a         = Event ()
          | otherwise = NoEvent
      ct  = edgeFrom a
  return (res, ct)

-- * Stateful event suppression

-- | Suppression of initial (at local time 0) event.
notYet :: Monad m => SF m (Event a) (Event a)
notYet = feedback False $ arr (\(e,c) ->
  if c then (e, True) else (NoEvent, True))

-- | Suppress all but the first event.
once :: Monad m => SF m (Event a) (Event a)
once = takeEvents 1

-- | Suppress all but the first n events.
takeEvents :: Monad m => Int -> SF m (Event a) (Event a)
takeEvents n | n <= 0 = never
takeEvents n = dSwitch (arr dup) (const (NoEvent >-- takeEvents (n - 1)))

-- | Suppress first n events.

-- Here dSwitch or switch does not really matter.
dropEvents :: Monad m => Int -> SF m (Event a) (Event a)
dropEvents n | n <= 0  = identity
dropEvents n = dSwitch (never &&& identity)
                             (const (NoEvent >-- dropEvents (n - 1)))

-- * Pointwise functions on events

noEvent :: Event a
noEvent = NoEvent

-- | Suppress any event in the first component of a pair.
noEventFst :: (Event a, b) -> (Event c, b)
noEventFst (_, b) = (NoEvent, b)


-- | Suppress any event in the second component of a pair.
noEventSnd :: (a, Event b) -> (a, Event c)
noEventSnd (a, _) = (a, NoEvent)

event :: a -> (b -> a) -> Event b -> a
event _ f (Event x) = f x
event x _ NoEvent   = x

fromEvent (Event x) = x
fromEvent _         = error "fromEvent NoEvent"

isEvent (Event _) = True
isEvent _         = False

isNoEvent (Event _) = False
isNoEvent _         = True

tag :: Event a -> b -> Event b
tag NoEvent   _ = NoEvent
tag (Event _) b = Event b

-- | Tags an (occurring) event with a value ("replacing" the old value). Same
-- as 'tag' with the arguments swapped.
--
-- Applicative-based definition:
-- tagWith = (<$)
tagWith :: b -> Event a -> Event b
tagWith = flip tag

-- | Attaches an extra value to the value of an occurring event.
attach :: Event a -> b -> Event (a, b)
e `attach` b = fmap (\a -> (a, b)) e

-- | Left-biased event merge (always prefer left event, if present).
lMerge :: Event a -> Event a -> Event a
lMerge = mergeBy (\e1 _ -> e1)

-- | Right-biased event merge (always prefer right event, if present).
rMerge :: Event a -> Event a -> Event a
rMerge = flip lMerge

merge :: Event a -> Event a -> Event a
merge = mergeBy $ error "Bearriver: merge: Simultaneous event occurrence."

mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeBy _       NoEvent      NoEvent      = NoEvent
mergeBy _       le@(Event _) NoEvent      = le
mergeBy _       NoEvent      re@(Event _) = re
mergeBy resolve (Event l)    (Event r)    = Event (resolve l r)

-- | A generic event merge-map utility that maps event occurrences,
-- merging the results. The first three arguments are mapping functions,
-- the third of which will only be used when both events are present.
-- Therefore, 'mergeBy' = 'mapMerge' 'id' 'id'
--
-- Applicative-based definition:
-- mapMerge lf rf lrf le re = (f <$> le <*> re) <|> (lf <$> le) <|> (rf <$> re)
mapMerge :: (a -> c) -> (b -> c) -> (a -> b -> c)
            -> Event a -> Event b -> Event c
mapMerge _  _  _   NoEvent   NoEvent   = NoEvent
mapMerge lf _  _   (Event l) NoEvent   = Event (lf l)
mapMerge _  rf _   NoEvent   (Event r) = Event (rf r)
mapMerge _  _  lrf (Event l) (Event r) = Event (lrf l r)

-- | Merge a list of events; foremost event has priority.
--
-- Foldable-based definition:
-- mergeEvents :: Foldable t => t (Event a) -> Event a
-- mergeEvents =  asum
mergeEvents :: [Event a] -> Event a
mergeEvents = foldr lMerge NoEvent

-- | Collect simultaneous event occurrences; no event if none.
--
-- Traverable-based definition:
-- catEvents :: Foldable t => t (Event a) -> Event (t a)
-- carEvents e  = if (null e) then NoEvent else (sequenceA e)
catEvents :: [Event a] -> Event [a]
catEvents eas = case [ a | Event a <- eas ] of
                    [] -> NoEvent
                    as -> Event as

-- | Join (conjunction) of two events. Only produces an event
-- if both events exist.
--
-- Applicative-based definition:
-- joinE = liftA2 (,)
joinE :: Event a -> Event b -> Event (a,b)
joinE NoEvent   _         = NoEvent
joinE _         NoEvent   = NoEvent
joinE (Event l) (Event r) = Event (l,r)

-- | Split event carrying pairs into two events.
splitE :: Event (a,b) -> (Event a, Event b)
splitE NoEvent       = (NoEvent, NoEvent)
splitE (Event (a,b)) = (Event a, Event b)

------------------------------------------------------------------------------
-- Event filtering
------------------------------------------------------------------------------

-- | Filter out events that don't satisfy some predicate.
filterE :: (a -> Bool) -> Event a -> Event a
filterE p e@(Event a) = if p a then e else NoEvent
filterE _ NoEvent     = NoEvent


-- | Combined event mapping and filtering. Note: since 'Event' is a 'Functor',
-- see 'fmap' for a simpler version of this function with no filtering.
mapFilterE :: (a -> Maybe b) -> Event a -> Event b
mapFilterE _ NoEvent   = NoEvent
mapFilterE f (Event a) = case f a of
                            Nothing -> NoEvent
                            Just b  -> Event b


-- | Enable/disable event occurences based on an external condition.
gate :: Event a -> Bool -> Event a
_ `gate` False = NoEvent
e `gate` True  = e

-- * Switching

-- ** Basic switchers

switch :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
switch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (_, Event c) -> local (const 0) (unMSF (sfC c) a)
    (b, NoEvent) -> return (b, switch ct sfC)

dSwitch ::  Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
dSwitch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (b, Event c) -> do (_,ct') <- local (const 0) (unMSF (sfC c) a)
                       return (b, ct')
    (b, NoEvent) -> return (b, dSwitch ct sfC)


-- * Parallel composition and switching

-- ** Parallel composition and switching over collections with broadcasting

#if MIN_VERSION_base(4,8,0)
parB :: (Monad m) => [SF m a b] -> SF m a [b]
#else
parB :: (Functor m, Monad m) => [SF m a b] -> SF m a [b]
#endif
parB = widthFirst . sequenceS

dpSwitchB :: (Monad m , Traversable col)
          => col (SF m a b) -> SF m (a, col b) (Event c) -> (col (SF m a b) -> c -> SF m a (col b))
          -> SF m a (col b)
dpSwitchB sfs sfF sfCs = MSF $ \a -> do
  res <- T.mapM (`unMSF` a) sfs
  let bs   = fmap fst res
      sfs' = fmap snd res
  (e,sfF') <- unMSF sfF (a, bs)
  let ct = case e of
             Event c -> sfCs sfs' c
             NoEvent -> dpSwitchB sfs' sfF' sfCs
  return (bs, ct)

-- ** Parallel composition over collections

parC :: Monad m => SF m a b -> SF m [a] [b]
parC sf = parC0 sf
  where
    parC0 :: Monad m => SF m a b -> SF m [a] [b]
    parC0 sf0 = MSF $ \as -> do
      os <- T.mapM (\(a,sf) -> unMSF sf a) $ zip as (replicate (length as) sf0)
      let bs  = fmap fst os
          cts = fmap snd os
      return (bs, parC' cts)

    parC' :: Monad m => [SF m a b] -> SF m [a] [b]
    parC' sfs = MSF $ \as -> do
      os <- T.mapM (\(a,sf) -> unMSF sf a) $ zip as sfs
      let bs  = fmap fst os
          cts = fmap snd os
      return (bs, parC' cts)

-- * Discrete to continuous-time signal functions

-- ** Wave-form generation

hold :: Monad m => a -> SF m (Event a) a
hold a = feedback a $ arr $ \(e,a') ->
    dup (event a' id e)
  where
    dup x = (x,x)

-- ** Accumulators

-- | Accumulator parameterized by the accumulation function.
accumBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) (Event b)
accumBy f b = mapEventS $ accumulateWith (flip f) b

accumHoldBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) b
accumHoldBy f b = feedback b $ arr $ \(a, b') ->
  let b'' = event b' (f b') a
  in (b'', b'')

-- * State keeping combinators

-- ** Loops with guaranteed well-defined feedback
loopPre :: Monad m => c -> SF m (a, c) (b, c) -> SF m a b
loopPre = feedback

-- * Integration and differentiation

integral :: (Monad m, VectorSpace a s) => SF m a a
integral = integralFrom zeroVector

integralFrom :: (Monad m, VectorSpace a s) => a -> SF m a a
integralFrom a0 = proc a -> do
  dt <- constM ask         -< ()
  accumulateWith (^+^) a0 -< realToFrac dt *^ a

derivative :: (Monad m, VectorSpace a s) => SF m a a
derivative = derivativeFrom zeroVector

derivativeFrom :: (Monad m, VectorSpace a s) => a -> SF m a a
derivativeFrom a0 = proc a -> do
  dt   <- constM ask   -< ()
  aOld <- MSF.iPre a0 -< a
  returnA             -< (a ^-^ aOld) ^/ realToFrac dt

-- NOTE: BUG in this function, it needs two a's but we
-- can only provide one
iterFrom :: Monad m => (a -> a -> DTime -> b -> b) -> b -> SF m a b
iterFrom f b = MSF $ \a -> do
  dt <- ask
  let b' = f a a dt b
  return (b, iterFrom f b')

-- * Noise (random signal) sources and stochastic event sources

occasionally :: MonadRandom m
             => Time -- ^ The time /q/ after which the event should be produced on average
             -> b    -- ^ Value to produce at time of event
             -> SF m a (Event b)
occasionally tAvg b
  | tAvg <= 0 = error "bearriver: Non-positive average interval in occasionally."
  | otherwise = proc _ -> do
      r   <- getRandomRS (0, 1) -< ()
      dt  <- timeDelta          -< ()
      let p = 1 - exp (-(dt / tAvg))
      returnA -< if r < p then Event b else NoEvent
 where
  timeDelta :: Monad m => SF m a DTime
  timeDelta = constM ask

-- * Execution/simulation

-- ** Reactimation

reactimate :: Monad m => m a -> (Bool -> m (DTime, Maybe a)) -> (Bool -> b -> m Bool) -> SF Identity a b -> m ()
reactimate senseI sense actuate sf = do
  -- runMaybeT $ MSF.reactimate $ liftMSFTrans (senseSF >>> sfIO) >>> actuateSF
  MSF.reactimateB $ senseSF >>> sfIO >>> actuateSF
  return ()
 where sfIO        = morphS (return.runIdentity) (runReaderS sf)

       -- Sense
       senseSF     = MSF.switch senseFirst senseRest
       senseFirst  = constM senseI >>> (arr $ \x -> ((0, x), Just x))
       senseRest a = constM (sense True) >>> (arr id *** keepLast a)

       keepLast :: Monad m => a -> MSF m (Maybe a) a
       keepLast a = MSF $ \ma -> let a' = fromMaybe a ma in a' `seq` return (a', keepLast a')

       -- Consume/render
       -- actuateSF :: MSF IO b ()
       -- actuateSF    = arr (\x -> (True, x)) >>> liftMSF (lift . uncurry actuate) >>> exitIf
       actuateSF    = arr (\x -> (True, x)) >>> arrM (uncurry actuate)

-- * Debugging / Step by step simulation

-- | Evaluate an SF, and return an output and an initialized SF.
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
evalAtZero :: SF Identity a b -> a -> (b, SF Identity a b)
evalAtZero sf a = runIdentity $ runReaderT (unMSF sf a) 0

-- | Evaluate an initialized SF, and return an output and a continuation.
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
evalAt :: SF Identity a b -> DTime -> a -> (b, SF Identity a b)
evalAt sf dt a = runIdentity $ runReaderT (unMSF sf a) dt

-- | Given a signal function and time delta, it moves the signal function into
--   the future, returning a new uninitialized SF and the initial output.
--
--   While the input sample refers to the present, the time delta refers to the
--   future (or to the time between the current sample and the next sample).
--
--   /WARN/: Do not use this function for standard simulation. This function is
--   intended only for debugging/testing. Apart from being potentially slower
--   and consuming more memory, it also breaks the FRP abstraction by making
--   samples discrete and step based.
--
evalFuture :: SF Identity a b -> a -> DTime -> (b, SF Identity a b)
evalFuture sf = flip (evalAt sf)

-- * Auxiliary functions

-- ** Event handling
replaceOnce :: Monad m => a -> SF m a a
replaceOnce a = dSwitch (arr $ const (a, Event ())) (const $ arr id)

-- ** Tuples
dup  x     = (x,x)
