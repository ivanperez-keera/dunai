-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Event Signal Functions and SF combinators.
--
-- Events represent values that only exist instantaneously, at discrete points
-- in time. Examples include mouse clicks, zero-crosses of monotonic continuous
-- signals, and square waves.
--
-- For signals that carry events, there should be a limit in the number of
-- events we can observe in a time period, no matter how much we increase the
-- sampling frequency.
module FRP.BearRiver.EventS
    (
      -- * Basic event sources
      never
    , now
    , after
    , repeatedly
    , afterEach
    , afterEachCat
    , edge
    , iEdge
    , edgeTag
    , edgeJust
    , edgeBy

      -- * Stateful event suppression
    , notYet
    , once
    , takeEvents
    , dropEvents

      -- * Hybrid SF combinators
    , snap
    )
  where

-- External imports
import Control.Arrow (arr, (&&&), (>>>), (>>^))

-- Internal imports (dunai)
import Control.Monad.Trans.MSF                 (ask)
import Data.MonadicStreamFunction              (feedback)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- Internal imports
import FRP.BearRiver.Arrow        (dup)
import FRP.BearRiver.Basic        (constant, identity, (-->), (>--))
import FRP.BearRiver.Event        (Event (..), maybeToEvent, tag)
import FRP.BearRiver.InternalCore (SF, Time)
import FRP.BearRiver.Switches     (dSwitch, switch)

-- | Event source that never occurs.
never :: Monad m => SF m a (Event b)
never = constant NoEvent

-- | Event source with a single occurrence at time 0. The value of the event is
-- given by the function argument.
now :: Monad m => b -> SF m a (Event b)
now b0 = Event b0 --> never

-- | Event source with a single occurrence at or as soon after (local) time /q/
-- as possible.
after :: Monad m
      => Time -- ^ The time /q/ after which the event should be produced
      -> b    -- ^ Value to produce at that time
      -> SF m a (Event b)
after q x = feedback q go
  where
    go = MSF $ \(_, t) -> do
           dt <- ask
           let t' = t - dt
               e  = if t > 0 && t' < 0 then Event x else NoEvent
               ct = if t' < 0 then constant (NoEvent, t') else go
           return ((e, t'), ct)

-- | Event source with repeated occurrences with interval q.
--
-- Note: If the interval is too short w.r.t. the sampling intervals, the result
-- will be that events occur at every sample. However, no more than one event
-- results from any sampling interval, thus avoiding an "event backlog" should
-- sampling become more frequent at some later point in time.
repeatedly :: Monad m => Time -> b -> SF m a (Event b)
repeatedly q x
    | q > 0     = afterEach qxs
    | otherwise = error "bearriver: repeatedly: Non-positive period."
  where
    qxs = (q, x):qxs

-- | Event source with consecutive occurrences at the given intervals.
--
-- Should more than one event be scheduled to occur in any sampling interval,
-- only the first will in fact occur to avoid an event backlog.

-- After all, after, repeatedly etc. are defined in terms of afterEach.
afterEach :: Monad m => [(Time, b)] -> SF m a (Event b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)

-- | Event source with consecutive occurrences at the given intervals.
--
-- Should more than one event be scheduled to occur in any sampling interval,
-- the output list will contain all events produced during that interval.
afterEachCat :: Monad m => [(Time, b)] -> SF m a (Event [b])
afterEachCat = afterEachCat' 0
  where
    afterEachCat' :: Monad m => Time -> [(Time, b)] -> SF m a (Event [b])
    afterEachCat' _ []  = never
    afterEachCat' t qxs = MSF $ \_ -> do
      dt <- ask
      let (ev, t', qxs') = fireEvents [] (t + dt) qxs
          ev' = if null ev
                  then NoEvent
                  else Event (reverse ev)

      return (ev', afterEachCat' t' qxs')

    fireEvents :: [b] -> Time -> [(Time, b)] -> ([b], Time, [(Time, b)])
    fireEvents ev t []       = (ev, t, [])
    fireEvents ev t (qx:qxs)
        | fst qx < 0   = error "bearriver: afterEachCat: Non-positive period."
        | overdue >= 0 = fireEvents (snd qx:ev) overdue qxs
        | otherwise    = (ev, t, qx:qxs)
      where
        overdue = t - fst qx

-- | A rising edge detector. Useful for things like detecting key presses. It is
-- initialised as /up/, meaning that events occurring at time 0 will not be
-- detected.
edge :: Monad m => SF m Bool (Event ())
edge = edgeFrom True

-- | A rising edge detector that can be initialized as up ('True', meaning that
-- events occurring at time 0 will not be detected) or down ('False', meaning
-- that events occurring at time 0 will be detected).
iEdge :: Monad m => Bool -> SF m Bool (Event ())
iEdge = edgeFrom

-- | A rising edge detector that can be initialized as up ('True', meaning that
-- events occurring at time 0 will not be detected) or down ('False', meaning
-- that events occurring at time 0 will be detected).
edgeFrom :: Monad m => Bool -> SF m Bool (Event())
edgeFrom prev = MSF $ \a -> do
  let res | prev      = NoEvent
          | a         = Event ()
          | otherwise = NoEvent
      ct  = edgeFrom a
  return (res, ct)

-- | Like 'edge', but parameterized on the tag value.
edgeTag :: Monad m => a -> SF m Bool (Event a)
edgeTag a = edge >>> arr (`tag` a)

-- | Edge detector particularized for detecting transitions on a 'Maybe' signal
-- from 'Nothing' to 'Just'.
edgeJust :: Monad m => SF m (Maybe a) (Event a)
edgeJust = edgeBy isJustEdge (Just undefined)
  where
    isJustEdge Nothing  Nothing     = Nothing
    isJustEdge Nothing  ma@(Just _) = ma
    isJustEdge (Just _) (Just _)    = Nothing
    isJustEdge (Just _) Nothing     = Nothing

-- | Edge detector parameterized on the edge detection function and initial
-- state, i.e., the previous input sample. The first argument to the edge
-- detection function is the previous sample, the second the current one.
edgeBy :: Monad m => (a -> a -> Maybe b) -> a -> SF m a (Event b)
edgeBy isEdge aPrev = MSF $ \a ->
  return (maybeToEvent (isEdge aPrev a), edgeBy isEdge a)

-- * Stateful event suppression

-- | Suppression of initial (at local time 0) event.
notYet :: Monad m => SF m (Event a) (Event a)
notYet = feedback False $ arr (\(e, c) ->
  if c then (e, True) else (NoEvent, True))

-- | Suppress all but the first event.
once :: Monad m => SF m (Event a) (Event a)
once = takeEvents 1

-- | Suppress all but the first n events.
takeEvents :: Monad m => Int -> SF m (Event a) (Event a)
takeEvents n | n <= 0 = never
takeEvents n = dSwitch (arr dup) (const (NoEvent >-- takeEvents (n - 1)))

-- | Suppress first n events.
dropEvents :: Monad m => Int -> SF m (Event a) (Event a)
dropEvents n | n <= 0 = identity
dropEvents n =
  -- Here dSwitch or switch does not really matter.
  dSwitch (never &&& identity)
          (const (NoEvent >-- dropEvents (n - 1)))

-- ** Hybrid continuous-to-discrete SF combinators.

-- | Event source with a single occurrence at time 0. The value of the event is
-- obtained by sampling the input at that time.
snap :: Monad m => SF m a (Event a)
snap =
  -- switch ensures that the entire signal function will become just
  -- "constant" once the sample has been taken.
  switch (never &&& (identity &&& now () >>^ \(a, e) -> e `tag` a)) now
