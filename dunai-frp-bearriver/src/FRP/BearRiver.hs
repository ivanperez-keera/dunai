{-# LANGUAGE Arrows     #-}
{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
-- The following warning is disabled so that we do not see warnings due to
-- using ListT on an MSF to implement parallelism with broadcasting.
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#else
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif
{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Implementation of Yampa using Monadic Stream Processing library.
module FRP.BearRiver
  (module FRP.BearRiver, module X)
 where

-- External imports
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative       (Applicative (..), (<$>))
#endif
import           Control.Applicative       (Alternative (..))
import           Control.Arrow             as X
import qualified Control.Category          as Category
import           Control.DeepSeq           (NFData (..))
import qualified Control.Monad.Fail        as Fail
import           Control.Monad.Random      (MonadRandom)
import           Data.Functor.Identity     (Identity (..))
import           Data.Maybe                (fromMaybe)
import           Data.Traversable          as T
import           Data.VectorSpace          as X

-- Internal imports (dunai)
import           Control.Monad.Trans.MSF                 hiding (dSwitch,
                                                          switch)
import qualified Control.Monad.Trans.MSF                 as MSF
import           Control.Monad.Trans.MSF.List            (sequenceS, widthFirst)
import           Data.MonadicStreamFunction              (iPre)
import           Data.MonadicStreamFunction              as X hiding
                                                              (reactimate,
                                                               repeatedly,
                                                               trace)
import           Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- Internal imports (dunai, instances)
import Data.MonadicStreamFunction.Instances.ArrowLoop () -- not needed, just
                                                         -- re-exported

infixr 0 -->, -:>, >--, >=-

-- * Basic definitions

-- | Time is used both for time intervals (duration), and time w.r.t. some
-- agreed reference point in time.
type Time  = Double

-- | DTime is the time type for lengths of sample intervals. Conceptually,
-- DTime = R+ = { x in R | x > 0 }. Don't assume Time and DTime have the
-- same representation.
type DTime = Double

-- | Extensible signal function (signal function with a notion of time, but
-- which can be extended with actions).
-- Signal function that transforms a signal carrying values of some type 'a'
-- into a signal carrying values of some type 'b'. You can think of it as
-- (Signal a -> Signal b). A signal is, conceptually, a
-- function from 'Time' to value.
type SF m        = MSF (ClockInfo m)

-- | Information on the progress of time.
type ClockInfo m = ReaderT DTime m

-- | A single possible event occurrence, that is, a value that may or may not
-- occur. Events are used to represent values that are not produced
-- continuously, such as mouse clicks (only produced when the mouse is clicked,
-- as opposed to mouse positions, which are always defined).
data Event a = Event a | NoEvent
 deriving (Eq, Ord, Show)

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

-- | MonadFail instance
instance Fail.MonadFail Event where
  -- | Fail with 'NoEvent'.
  fail _ = NoEvent

-- | Alternative instance
instance Alternative Event where
  -- | An empty alternative carries no event, so it is ignored.
  empty = NoEvent
  -- | Merge favouring the left event ('NoEvent' only if both are
  -- 'NoEvent').
  NoEvent <|> r = r
  l       <|> _ = l

-- | NFData instance
instance NFData a => NFData (Event a) where
  -- | Evaluate value carried by event.
  rnf NoEvent   = ()
  rnf (Event a) = rnf a `seq` ()

-- ** Lifting

-- | Lifts a pure function into a signal function (applied pointwise).
arrPrim :: Monad m => (a -> b) -> SF m a b
arrPrim = arr

-- | Lifts a pure function into a signal function applied to events (applied
-- pointwise).
arrEPrim :: Monad m => (Event a -> b) -> SF m (Event a) b
arrEPrim = arr

-- * Signal functions

-- ** Basic signal functions

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

-- | Outputs the time passed since the signal function instance was started.
localTime :: Monad m => SF m a Time
localTime = constant 1.0 >>> integral

-- | Alternative name for localTime.
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

-- | Applies a function point-wise, using the last output as next input. This
-- creates a well-formed loop based on a pure, auxiliary function.
sscan :: Monad m => (b -> a -> b) -> b -> SF m a b
sscan f b_init = feedback b_init u
  where u = undefined -- (arr f >>^ dup)

-- | Generic version of 'sscan', in which the auxiliary function produces
-- an internal accumulator and an "held" output.
--
-- Applies a function point-wise, using the last known 'Just' output to form
-- the output, and next input accumulator. If the output is 'Nothing', the last
-- known accumulators are used. This creates a well-formed loop based on a
-- pure, auxiliary function.
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

-- | Event source with a single occurrence at or as soon after (local) time /q/
-- as possible.
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

-- | Event source with repeated occurrences with interval q.
-- Note: If the interval is too short w.r.t. the sampling intervals,
-- the result will be that events occur at every sample. However, no more
-- than one event results from any sampling interval, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.
repeatedly :: Monad m => Time -> b -> SF m a (Event b)
repeatedly q x
    | q > 0     = afterEach qxs
    | otherwise = error "bearriver: repeatedly: Non-positive period."
  where
    qxs = (q,x):qxs

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
    afterEachCat' _ [] = never
    afterEachCat' t qxs = MSF $ \_ -> do
      dt <- ask
      let (ev, t', qxs') = fireEvents [] (t + dt) qxs
          ev' = if null ev
                  then NoEvent
                  else Event (reverse ev)

      return (ev', afterEachCat' t' qxs')

    fireEvents :: [b] -> Time -> [(Time,b)] -> ([b], Time, [(Time,b)])
    fireEvents ev t [] = (ev, t, [])
    fireEvents ev t (qx:qxs)
      | fst qx < 0 = error "bearriver: afterEachCat: Non-positive period."
      | otherwise =
          let overdue = t - fst qx in
          if overdue >= 0
            then fireEvents (snd qx:ev) overdue qxs
            else (ev, t, qx:qxs)

-- * Events

-- | Apply an 'MSF' to every input. Freezes temporarily if the input is
-- 'NoEvent', and continues as soon as an 'Event' is received.
mapEventS :: Monad m => MSF m a b -> MSF m (Event a) (Event b)
mapEventS msf = proc eventA -> case eventA of
  Event a -> arr Event <<< msf -< a
  NoEvent -> returnA           -< NoEvent

-- ** Relation to other types

eventToMaybe :: Event a -> Maybe a
eventToMaybe = event Nothing Just

boolToEvent :: Bool -> Event ()
boolToEvent True  = Event ()
boolToEvent False = NoEvent

-- * Hybrid SF m combinators

-- | A rising edge detector. Useful for things like detecting key presses.
-- It is initialised as /up/, meaning that events occurring at time 0 will
-- not be detected.
edge :: Monad m => SF m Bool (Event ())
edge = edgeFrom True

-- | A rising edge detector that can be initialized as up ('True', meaning
-- that events occurring at time 0 will not be detected) or down
-- ('False', meaning that events occurring at time 0 will be detected).
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

-- | Edge detector parameterized on the edge detection function and initial
-- state, i.e., the previous input sample. The first argument to the
-- edge detection function is the previous sample, the second the current one.
edgeBy :: Monad m => (a -> a -> Maybe b) -> a -> SF m a (Event b)
edgeBy isEdge a_prev = MSF $ \a ->
  return (maybeToEvent (isEdge a_prev a), edgeBy isEdge a)

-- | Convert a maybe value into a event ('Event' is isomorphic to 'Maybe').
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

-- | Make the NoEvent constructor available. Useful e.g. for initialization,
-- ((-->) & friends), and it's easily available anyway (e.g. mergeEvents []).
noEvent :: Event a
noEvent = NoEvent

-- | Suppress any event in the first component of a pair.
noEventFst :: (Event a, b) -> (Event c, b)
noEventFst (_, b) = (NoEvent, b)


-- | Suppress any event in the second component of a pair.
noEventSnd :: (a, Event b) -> (a, Event c)
noEventSnd (a, _) = (a, NoEvent)

-- | An event-based version of the maybe function.
event :: a -> (b -> a) -> Event b -> a
event _ f (Event x) = f x
event x _ NoEvent   = x

-- | Extract the value from an event. Fails if there is no event.
fromEvent :: Event a -> a
fromEvent (Event x) = x
fromEvent _         = error "fromEvent NoEvent"

-- | Tests whether the input represents an actual event.
isEvent :: Event a -> Bool
isEvent (Event _) = True
isEvent _         = False

-- | Negation of 'isEvent'.
isNoEvent :: Event a -> Bool
isNoEvent (Event _) = False
isNoEvent _         = True

-- | Tags an (occurring) event with a value ("replacing" the old value).
--
-- Applicative-based definition:
-- tag = ($>)
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

-- | Unbiased event merge: simultaneous occurrence is an error.
merge :: Event a -> Event a -> Event a
merge = mergeBy $ error "Bearriver: merge: Simultaneous event occurrence."

-- Applicative-based definition:
-- mergeBy f le re = (f <$> le <*> re) <|> le <|> re
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
-- Whenever the second value in the pair actually is an event,
-- the value carried by the event is used to obtain a new signal
-- function to be applied *at future times*.
--
-- Until that happens, the first value in the pair is produced
-- in the output signal.
--
-- Important note: at the time of switching, the second
-- signal function is used immediately, but the current
-- input is fed by it (even though the actual output signal
-- value at time 0 is discarded).
--
-- If that second SF can also switch at time zero, then a
-- double (nested) -- switch might take place. If the second SF refers to the
-- first one, the switch might take place infinitely many times and never be
-- resolved.
--
-- Remember: The continuation is evaluated strictly at the time
-- of switching!
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
-- ^ Spatial parallel composition of a signal function collection.
-- Given a collection of signal functions, it returns a signal
-- function that broadcasts its input signal to every element
-- of the collection, to return a signal carrying a collection
-- of outputs. See 'par'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
parB = widthFirst . sequenceS

-- | Decoupled parallel switch with broadcasting (dynamic collection of
-- signal functions spatially composed in parallel). See 'dpSwitch'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
dpSwitchB :: (Functor m, Monad m , Traversable col)
          => col (SF m a b) -> SF m (a, col b) (Event c) -> (col (SF m a b) -> c -> SF m a (col b))
          -> SF m a (col b)
dpSwitchB sfs sfF sfCs = MSF $ \a -> do
  res <- T.mapM (`unMSF` a) sfs
  let bs   = fmap fst res
      sfs' = fmap snd res
  (e,sfF') <- unMSF sfF (a, bs)
  ct <- case e of
          Event c -> snd <$> unMSF (sfCs sfs c) a
          NoEvent -> return (dpSwitchB sfs' sfF' sfCs)
  return (bs, ct)

-- ** Parallel composition over collections

-- | Apply an SF to every element of a list.
--
--   Example:
--
--   >>> embed (parC integral) (deltaEncode 0.1 [[1, 2], [2, 4], [3, 6], [4.0, 8.0 :: Float]])
--   [[0.0,0.0],[0.1,0.2],[0.3,0.6],[0.6,1.2]]
--
--   The number of SFs or expected inputs is determined by the first input
--   list, and not expected to vary over time.
--
--   If more inputs come in a subsequent list, they are ignored.
--
--   >>> embed (parC (arr (+1))) (deltaEncode 0.1 [[0], [1, 1], [3, 4], [6, 7, 8], [1, 1], [0, 0], [1, 9, 8]])
--   [[1],[2],[4],[7],[2],[1],[2]]
--
--   If less inputs come in a subsequent list, an exception is thrown.
--
--   >>> embed (parC (arr (+1))) (deltaEncode 0.1 [[0, 0], [1, 1], [3, 4], [6, 7, 8], [1, 1], [0, 0], [1, 9, 8]])
--   [[1,1],[2,2],[4,5],[7,8],[2,2],[1,1],[2,10]]
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

-- | Zero-order hold.
--
-- Converts a discrete-time signal into a continuous-time signal, by holding
-- the last value until it changes in the input signal. The given parameter
-- may be used for time zero, and until the first event occurs in the input
-- signal, so hold is always well-initialized.
--
-- >>> embed (hold 1) (deltaEncode 0.1 [NoEvent, NoEvent, Event 2, NoEvent, Event 3, NoEvent])
-- [1,1,2,2,3,3]
hold :: Monad m => a -> SF m (Event a) a
hold a = feedback a $ arr $ \(e,a') ->
    dup (event a' id e)

-- ** Accumulators

-- | Accumulator parameterized by the accumulation function.
accumBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) (Event b)
accumBy f b = mapEventS $ accumulateWith (flip f) b

-- | Zero-order hold accumulator parameterized by the accumulation function.
accumHoldBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) b
accumHoldBy f b = feedback b $ arr $ \(a, b') ->
  let b'' = event b' (f b') a
  in (b'', b'')

-- * State keeping combinators

-- ** Loops with guaranteed well-defined feedback

-- | Loop with an initial value for the signal being fed back.
loopPre :: Monad m => c -> SF m (a, c) (b, c) -> SF m a b
loopPre = feedback

-- * Integration and differentiation

-- | Integration using the rectangle rule.
integral :: (Monad m, Fractional s, VectorSpace a s) => SF m a a
integral = integralFrom zeroVector


-- | Integrate using an auxiliary function that takes the current and the last
-- input, the time between those samples, and the last output, and returns a
-- new output.
integralFrom :: (Monad m, Fractional s, VectorSpace a s) => a -> SF m a a
integralFrom a0 = proc a -> do
  dt <- constM ask         -< ()
  accumulateWith (^+^) a0 -< realToFrac dt *^ a

-- | A very crude version of a derivative. It simply divides the
-- value difference by the time difference. Use at your own risk.
derivative :: (Monad m, Fractional s, VectorSpace a s) => SF m a a
derivative = derivativeFrom zeroVector

derivativeFrom :: (Monad m, Fractional s, VectorSpace a s) => a -> SF m a a
derivativeFrom a0 = proc a -> do
  dt   <- constM ask -< ()
  aOld <- iPre a0    -< a
  returnA            -< (a ^-^ aOld) ^/ realToFrac dt

-- NOTE: BUG in this function, it needs two a's but we
-- can only provide one
iterFrom :: Monad m => (a -> a -> DTime -> b -> b) -> b -> SF m a b
iterFrom f b = MSF $ \a -> do
  dt <- ask
  let b' = f a a dt b
  return (b, iterFrom f b')

-- * Noise (random signal) sources and stochastic event sources

-- | Stochastic event source with events occurring on average once every t_avg
-- seconds. However, no more than one event results from any one sampling
-- interval in the case of relatively sparse sampling, thus avoiding an
-- "event backlog" should sampling become more frequent at some later
-- point in time.
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

-- | Convenience function to run a signal function indefinitely, using a IO
-- actions to obtain new input and process the output.
--
-- This function first runs the initialization action, which provides the
-- initial input for the signal transformer at time 0.
--
-- Afterwards, an input sensing action is used to obtain new input (if any) and
-- the time since the last iteration. The argument to the input sensing
-- function indicates if it can block. If no new input is received, it is
-- assumed to be the same as in the last iteration.
--
-- After applying the signal function to the input, the actuation IO action is
-- executed. The first argument indicates if the output has changed, the second
-- gives the actual output). Actuation functions may choose to ignore the first
-- argument altogether. This action should return True if the reactimation must
-- stop, and False if it should continue.
--
-- Note that this becomes the program's /main loop/, which makes using this
-- function incompatible with GLUT, Gtk and other graphics libraries. It may
-- also impose a sizeable constraint in larger projects in which different
-- subparts run at different time steps. If you need to control the main loop
-- yourself for these or other reasons, use 'reactInit' and 'react'.
reactimate :: Monad m => m a -> (Bool -> m (DTime, Maybe a)) -> (Bool -> b -> m Bool) -> SF Identity a b -> m ()
reactimate senseI sense actuate sf = do
  -- runMaybeT $ MSF.reactimate $ liftMSFTrans (senseSF >>> sfIO) >>> actuateSF
  MSF.reactimateB $ senseSF >>> sfIO >>> actuateSF
  return ()
 where sfIO        = morphS (return.runIdentity) (runReaderS sf)

       -- Sense
       senseSF     = MSF.dSwitch senseFirst senseRest

       -- Sense: First sample
       senseFirst = constM senseI >>> arr (\x -> ((0, x), Just x))

       -- Sense: Remaining samples
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
dup :: a -> (a, a)
dup  x     = (x,x)
