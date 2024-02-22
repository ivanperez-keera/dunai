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
    , delayEvent
    , delayEventCat
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
    , snapAfter
    , sample
    , sampleWindow

      -- * Repetition and switching
    , recur
    , andThen
    )
  where

-- External imports
import Control.Arrow (arr, (&&&), (>>>), (>>^))

-- Internal imports (dunai)
import Control.Monad.Trans.MSF                 (ask)
import Data.MonadicStreamFunction              (feedback)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF))

-- Internal imports
import FRP.BearRiver.Arrow        (dup)
import FRP.BearRiver.Basic        (constant, identity, (-->), (>--))
import FRP.BearRiver.Event        (Event (..), maybeToEvent, tag)
import FRP.BearRiver.Hybrid       (accumBy)
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

-- | Event source with consecutive occurrences at the given intervals. Should
-- more than one event be scheduled to occur in any sampling interval, only the
-- first will in fact occur to avoid an event backlog.
afterEach :: Monad m => [(Time, b)] -> SF m a (Event b)
afterEach qxs = afterEachCat qxs >>> arr (fmap head)

-- | Event source with consecutive occurrences at the given intervals. Should
-- more than one event be scheduled to occur in any sampling interval, the
-- output list will contain all events produced during that interval.
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


-- | Delay for events. (Consider it a triggered after, hence /basic/.)
delayEvent :: Monad m => Time -> SF m (Event a) (Event a)
delayEvent q | q < 0     = error "bearriver: delayEvent: Negative delay."
             | q == 0    = identity
             | otherwise = delayEventCat q >>> arr (fmap head)

-- | Delay an event by a given delta and catenate events that occur so closely
-- so as to be /inseparable/.
delayEventCat :: Monad m => Time -> SF m (Event a) (Event [a])
delayEventCat q | q < 0     = error "bearriver: delayEventCat: Negative delay."
                | q == 0    = arr (fmap (:[]))
                | otherwise = MSF noPendingEvent
  where
    noPendingEvent e
          = return
               ( NoEvent
               , case e of
                   NoEvent -> MSF $ noPendingEvent
                   Event x -> MSF (pendingEvents (-q) [] [] (-q) x)
               )

    -- tNext is the present time w.r.t. the next scheduled event.
    -- tLast is the present time w.r.t. the last scheduled event.
    -- In the event queues, events are associated with their time
    -- w.r.t. to preceding event (positive).
    pendingEvents tLast rqxs qxs tNext x = tf -- True
      where
        tf e = do dt <- ask
                  return (tf' dt e)

        tf' dt e
            | tNext' >= 0
            = emitEventsScheduleNext e tLast' rqxs qxs tNext' [x]
            | otherwise
            = (NoEvent, MSF (pendingEvents tLast'' rqxs' qxs tNext' x))
          where
            tNext' = tNext + dt
            tLast' = tLast + dt
            (tLast'', rqxs') =
              case e of
                NoEvent  -> (tLast', rqxs)
                Event x' -> (-q,     (tLast' + q, x') : rqxs)

    -- tNext is the present time w.r.t. the *scheduled* time of the event that
    -- is about to be emitted (i.e. >= 0).
    -- The time associated with any event at the head of the event queue is also
    -- given w.r.t. the event that is about to be emitted.  Thus, tNext - q' is
    -- the present time w.r.t. the event at the head of the event queue.
    emitEventsScheduleNext e _ [] [] _ rxs =
      ( Event (reverse rxs)
      , case e of
          NoEvent -> MSF $ noPendingEvent
          Event x -> MSF $ pendingEvents (-q) [] [] (-q) x
      )
    emitEventsScheduleNext e tLast rqxs [] tNext rxs =
      emitEventsScheduleNext e tLast [] (reverse rqxs) tNext rxs
    emitEventsScheduleNext e tLast rqxs ((q', x') : qxs') tNext rxs
      | q' > tNext = ( Event (reverse rxs)
                     , case e of
                         NoEvent -> MSF $
                           pendingEvents tLast
                                         rqxs
                                         qxs'
                                         (tNext - q')
                                         x'
                         Event x'' -> MSF $
                           pendingEvents (-q)
                                         ((tLast + q, x'') : rqxs)
                                         qxs'
                                         (tNext - q')
                                         x'
                      )
      | otherwise  = emitEventsScheduleNext e
                                            tLast
                                            rqxs
                                            qxs'
                                            (tNext - q')
                                            (x' : rxs)
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

-- | Event source with a single occurrence at or as soon after (local) time
-- @tEv@ as possible. The value of the event is obtained by sampling the input a
-- that time.
snapAfter :: Monad m => Time -> SF m a (Event a)
snapAfter tEv =
  switch (never &&& (identity &&& after tEv () >>^ \(a, e) -> e `tag` a)) now

-- | Sample a signal at regular intervals.
sample :: Monad m => Time -> SF m a (Event a)
sample pEv = identity &&& repeatedly pEv () >>^ \(a, e) -> e `tag` a

-- | Window sampling.
--
-- First argument is the window length wl, second is the sampling interval t.
-- The output list should contain (min (truncate (T/t) wl)) samples, where T is
-- the time the signal function has been running. This requires some care in
-- case of sparse sampling. In case of sparse sampling, the current input value
-- is assumed to have been present at all points where sampling was missed.
sampleWindow :: Monad m => Int -> Time -> SF m a (Event [a])
sampleWindow wl q =
    identity &&& afterEachCat (repeat (q, ()))
    >>> arr (\(a, e) -> fmap (map (const a)) e)
    >>> accumBy updateWindow []
  where
    updateWindow w as = drop (max (length w' - wl) 0) w'
      where
        w' = w ++ as

-- * Repetition and switching

-- | Makes an event source recurring by restarting it as soon as it has an
-- occurrence.
recur :: Monad m => SF m a (Event b) -> SF m a (Event b)
recur sfe = switch (never &&& sfe) $ \b -> Event b --> recur (NoEvent --> sfe)

-- | Apply the first SF until it produces an event, and, afterwards, switch to
-- the second SF. This is just a convenience function, used to write what
-- sometimes is more understandable switch-based code.
andThen :: Monad m => SF m a (Event b) -> SF m a (Event b) -> SF m a (Event b)
sfe1 `andThen` sfe2 = dSwitch (sfe1 >>^ dup) (const sfe2)
