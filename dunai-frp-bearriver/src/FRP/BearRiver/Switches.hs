{-# LANGUAGE CPP        #-}
{-# LANGUAGE Rank2Types #-}
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
--
-- For example, the most basic switch has the following signature:
--
-- @switch :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b@
--
-- which indicates that it has two parameters: a signal function that produces
-- an output and indicates, with an event, when it is time to switch, and a
-- signal function that starts with the residual data left by the first SF in
-- the event and continues onwards.
--
-- Switching occurs, at most, once. If you want something to switch repeatedly,
-- in general, you need to loop, or to switch onto the same signal function
-- again. However, some switches, explained below, are immediate (meaning that
-- the second SF is started at the time of switching). If you use the same SF
-- that originally provoked the switch, you are very likely to fall into an
-- infinite loop. In those cases, the use of 'dSwitch' or '-->' may help.
--
-- Switches vary depending on a number of criteria:
--
-- - /Decoupled/ vs normal switching /(d)/: when an SF is being applied and a
-- different SF needs to be applied next, one question is which one is used for
-- the time in which the switching takes place. In decoupled switching, the old
-- SF is used for the time of switching, and the one SF is only used after that.
-- In normal or instantaneous or coupled switching, the old SF is discarded
-- immediately and a new SF is used for the output already from that point in
-- time.
--
-- - How the switching event is provided /( \/r\/k)/: normally, an 'Event' is
-- used to indicate that a switching must take place. This event can be part of
-- the argument SF (e.g., 'switch'), it can be part of the input (e.g.,
-- 'rSwitch'), or it can be determined by a second argument SF (e.g, 'kSwitch').
--
-- - How many SFs are being handled /( \/p\/par)/: some combinators deal with
-- only one SF, others handle collections, either in the form of a 'Functor' or
-- a list ('[]').
--
-- - How the input is router /(B\/Z\/ )/: when multiple SFs are being combined,
-- a decision needs to be made about how the input is passed to the internal
-- SFs.  In some cases, broadcasting is used to pass the same input to all
-- internal SFs. In others, the input is itself a collection, and each element
-- is passed to one internal SF (i.e., /zipping/). In others, an auxiliary
-- function is used to decide how to route specific inputs to specific SFs in
-- the collection.
--
-- These gives a number of different combinations, some of which make no sense,
-- and also helps determine the expected behaviour of a combinator by looking at
-- its name. For example, 'drpSwitchB' is the decoupled (/d/), recurrent (/r/),
-- parallel (/p/) switch with broadcasting (/B/).
module FRP.BearRiver.Switches
    (
      -- * Basic switching
      switch,  dSwitch
    , rSwitch, drSwitch
    , kSwitch, dkSwitch

      -- * Parallel composition\/switching (collections)
      -- ** With broadcasting
    , parB
    , pSwitchB, dpSwitchB
    , rpSwitchB, drpSwitchB

      -- ** With helper routing function
    , par
    , pSwitch,  dpSwitch
    , rpSwitch, drpSwitch

      -- * Parallel composition\/switching (lists)
      --
      -- ** With "zip" routing
    , parZ
    , pSwitchZ
    , dpSwitchZ
    , rpSwitchZ
    , drpSwitchZ

      -- ** With replication
    , parC
    )
  where

-- External imports
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative (..), (<$>))
#endif
import Control.Arrow              (arr, first)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Traversable           as T

-- Internal imports (dunai)
import Control.Monad.Trans.MSF                 (local, performOnFirstSample)
import Control.Monad.Trans.MSF.List            (sequenceS, widthFirst)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- Internal imports
import FRP.BearRiver.Basic        ((>=-))
import FRP.BearRiver.Event        (Event (..), noEventSnd)
import FRP.BearRiver.InternalCore (DTime, SF)

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

-- | Recurring switch.
--
-- Uses the given SF until an event comes in the input, in which case the SF in
-- the event is turned on, until the next event comes in the input, and so on.
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more information on how
-- this switch works.
rSwitch :: Monad m => SF m a b -> SF m (a, Event (SF m a b)) b
rSwitch sf = switch (first sf) ((noEventSnd >=-) . rSwitch)

-- | Recurring switch with delayed observation.
--
-- Uses the given SF until an event comes in the input, in which case the SF in
-- the event is turned on, until the next event comes in the input, and so on.
--
-- Uses decoupled switch ('dSwitch').
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more information on how
-- this switch works.
drSwitch :: Monad m => SF m a b -> SF m (a, Event (SF m a b)) b
drSwitch sf = dSwitch (first sf) ((noEventSnd >=-) . drSwitch)

-- | Call-with-current-continuation switch.
--
-- Applies the first SF until the input signal and the output signal, when
-- passed to the second SF, produce an event, in which case the original SF and
-- the event are used to build an new SF to switch into.
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more information on how
-- this switch works.
kSwitch :: Monad m
        => SF m a b
        -> SF m (a, b) (Event c)
        -> (SF m a b -> c -> SF m a b)
        -> SF m a b
kSwitch sf10 tfe0 k = MSF tf0
  where
    tf0 a0 = do
      (b0, sf1) <- unMSF sf10 a0
      (me, sfe) <- unMSF tfe0 (a0, b0)
      case me of
        NoEvent  -> return (b0, kSwitch sf1 sfe k)
        Event c0 -> unMSF (k sf10 c0) a0

-- | 'kSwitch' with delayed observation.
--
-- Applies the first SF until the input signal and the output signal, when
-- passed to the second SF, produce an event, in which case the original SF and
-- the event are used to build an new SF to switch into.
--
-- The switch is decoupled ('dSwitch').
--
-- See <https://wiki.haskell.org/Yampa#Switches> for more information on how
-- this switch works.
#if MIN_VERSION_base(4,8,0)
dkSwitch :: Monad m
         => SF m a b
         -> SF m (a, b) (Event c)
         -> (SF m a b -> c -> SF m a b)
         -> SF m a b
#else
dkSwitch :: (Functor m, Monad m)
         => SF m a b
         -> SF m (a, b) (Event c)
         -> (SF m a b -> c -> SF m a b)
         -> SF m a b
#endif
dkSwitch sf1 sfe k = MSF tf -- False
      where
        tf a = do
          (b, sf1')  <- unMSF sf1 a
          (me, sfe') <- unMSF sfe (a, b)
          let sfe'' = case me of
                        NoEvent -> dkSwitch sf1' sfe' k
                        Event c -> performOnFirstSample (snd <$> unMSF (k sf1 c) a)
          return (b, sfe'')

-- * Parallel composition and switching

-- ** Parallel composition and switching over collections with broadcasting

-- | Tuple a value up with every element of a collection of signal functions.
broadcast :: Functor col => a -> col sf -> col (a, sf)
broadcast a = fmap (\sf -> (a, sf))

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

-- | Parallel switch (dynamic collection of signal functions spatially composed
-- in parallel) with broadcasting. See 'pSwitch'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
pSwitchB :: (Functor m, Monad m, Traversable col, Functor col)
         => col (SF m a b)
         -> SF m (a, col b) (Event c)
         -> (col (SF m a b) -> c -> SF m a (col b))
         -> SF m a (col b)
pSwitchB = pSwitch broadcast

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

-- | Recurring parallel switch with broadcasting.
--
-- Uses the given collection of SFs, until an event comes in the input, in which
-- case the function in the 'Event' is used to transform the collections of SF
-- to be used with 'rpSwitch' again, until the next event comes in the input,
-- and so on.
--
-- Broadcasting is used to decide which subpart of the input goes to each SF in
-- the collection.
--
-- See 'rpSwitch'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
rpSwitchB :: (Functor m, Monad m, Functor col, Traversable col)
          => col (SF m a b)
          -> SF m (a, Event (col (SF m a b) -> col (SF m a b))) (col b)
rpSwitchB = rpSwitch broadcast

-- | Decoupled recurring parallel switch with broadcasting.
--
-- Uses the given collection of SFs, until an event comes in the input, in which
-- case the function in the 'Event' is used to transform the collections of SF
-- to be used with 'rpSwitch' again, until the next event comes in the input,
-- and so on.
--
-- Broadcasting is used to decide which subpart of the input goes to each SF in
-- the collection.
--
-- This is the decoupled version of 'rpSwitchB'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
drpSwitchB :: (Functor m, Monad m, Functor col, Traversable col)
           => col (SF m a b)
           -> SF m (a, Event (col (SF m a b) -> col (SF m a b))) (col b)
drpSwitchB = drpSwitch broadcast

-- * Parallel composition and switching over collections with general routing

-- | Spatial parallel composition of a signal function collection parameterized
-- on the routing function.
par :: (Functor m, Monad m, Functor col, Traversable col)
    => (forall sf . (a -> col sf -> col (b, sf)))
       -- ^ Determines the input to each signal function in the collection.
       -- IMPORTANT! The routing function MUST preserve the structure of the
       -- signal function collection.
    -> col (SF m b c)
       -- ^ Signal function collection.
    -> SF m a (col c)
par rf sfs0 = MSF tf0
  where
    tf0 a0 = do
      let bsfs0 = rf a0 sfs0
      sfcs0 <- T.mapM (\(b0, sf0) -> (unMSF sf0) b0) bsfs0
      let sfs = fmap snd sfcs0
          cs0 = fmap fst sfcs0
      return (cs0, par rf sfs)

-- | Parallel switch parameterized on the routing function. This is the most
-- general switch from which all other (non-delayed) switches in principle can
-- be derived. The signal function collection is spatially composed in parallel
-- and run until the event signal function has an occurrence. Once the switching
-- event occurs, all signal function are "frozen" and their continuations are
-- passed to the continuation function, along with the event value.
pSwitch :: (Functor m, Monad m, Traversable col, Functor col)
        => (forall sf . (a -> col sf -> col (b, sf)))
           -- ^ Routing function: determines the input to each signal function
           -- in the collection. IMPORTANT! The routing function has an
           -- obligation to preserve the structure of the signal function
           -- collection.
        -> col (SF m b c)
           -- ^ Signal function collection.
        -> SF m (a, col c) (Event d)
           -- ^ Signal function generating the switching event.
        -> (col (SF m b c) -> d -> SF m a (col c))
           -- ^ Continuation to be invoked once event occurs.
        -> SF m a (col c)
pSwitch rf sfs0 sfe0 k = MSF tf0
  where
    tf0 a0 = do
      let bsfs0 = rf a0 sfs0
      sfcs0 <- T.mapM (\(b0, sf0) -> (unMSF sf0) b0) bsfs0
      let sfs   = fmap snd sfcs0
          cs0   = fmap fst sfcs0
      (e, sfe) <- unMSF sfe0 (a0, cs0)
      case e of
        NoEvent  -> return (cs0, pSwitchAux sfs sfe)
        Event d0 -> unMSF (k sfs0 d0) a0

    pSwitchAux sfs sfe = MSF tf
      where
        tf a = do
          let bsfs = rf a sfs
          sfcs' <- T.mapM (\(b, sf) -> (unMSF sf b)) bsfs
          let sfs' = fmap snd sfcs'
              cs   = fmap fst sfcs'
          (e, sfe') <- unMSF sfe (a, cs)
          case e of
            NoEvent -> return (cs, pSwitchAux sfs' sfe')
            Event d -> do dt <- ask
                          unMSF (k (freezeCol sfs dt) d) a

-- | Parallel switch with delayed observation parameterized on the routing
-- function.
--
-- The collection argument to the function invoked on the switching event is of
-- particular interest: it captures the continuations of the signal functions
-- running in the collection maintained by 'dpSwitch' at the time of the
-- switching event, thus making it possible to preserve their state across a
-- switch.  Since the continuations are plain, ordinary signal functions, they
-- can be resumed, discarded, stored, or combined with other signal functions.
dpSwitch :: (Monad m, Traversable col)
         => (forall sf. (a -> col sf -> col (b, sf)))
            -- ^ Routing function. Its purpose is to pair up each running signal
            -- function in the collection maintained by 'dpSwitch' with the
            -- input it is going to see at each point in time. All the routing
            -- function can do is specify how the input is distributed.
         -> col (SF m b c)
            -- ^ Initial collection of signal functions.
         -> SF m (a, col c) (Event d)
            -- ^ Signal function that observes the external input signal and the
            -- output signals from the collection in order to produce a
            -- switching event.
         -> (col (SF m b c) -> d -> SF m a (col c))
            -- ^ The fourth argument is a function that is invoked when the
            -- switching event occurs, yielding a new signal function to switch
            -- into based on the collection of signal functions previously
            -- running and the value carried by the switching event. This allows
            -- the collection to be updated and then switched back in, typically
            -- by employing 'dpSwitch' again.
         -> SF m a (col c)
dpSwitch rf sfs sfF sfCs = MSF $ \a -> do
  let bsfs = rf a sfs
  res <- T.mapM (\(b, sf) -> unMSF sf b) bsfs
  let cs   = fmap fst res
      sfs' = fmap snd res
  (e, sfF') <- unMSF sfF (a, cs)
  let ct = case e of
          Event d -> sfCs sfs' d
          NoEvent -> dpSwitch rf sfs' sfF' sfCs
  return (cs, ct)

-- | Recurring parallel switch parameterized on the routing function.
--
-- Uses the given collection of SFs, until an event comes in the input, in which
-- case the function in the 'Event' is used to transform the collections of SF
-- to be used with 'rpSwitch' again, until the next event comes in the input,
-- and so on.
--
-- The routing function is used to decide which subpart of the input goes to
-- each SF in the collection.
--
-- This is the parallel version of 'rSwitch'.
rpSwitch :: (Functor m, Monad m, Functor col, Traversable col)
         => (forall sf . (a -> col sf -> col (b, sf)))
            -- ^ Routing function: determines the input to each signal function
            -- in the collection. IMPORTANT! The routing function has an
            -- obligation to preserve the structure of the signal function
            -- collection.
         -> col (SF m b c)
            -- ^ Initial signal function collection.
         -> SF m (a, Event (col (SF m b c) -> col (SF m b c))) (col c)
rpSwitch rf sfs =
  pSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
  noEventSnd >=- rpSwitch rf (f sfs')

-- | Recurring parallel switch with delayed observation parameterized on the
-- routing function.
--
-- Uses the given collection of SFs, until an event comes in the input, in which
-- case the function in the 'Event' is used to transform the collections of SF
-- to be used with 'rpSwitch' again, until the next event comes in the input,
-- and so on.
--
-- The routing function is used to decide which subpart of the input goes to
-- each SF in the collection.
--
-- This is the parallel version of 'drSwitch'.
drpSwitch :: (Functor m, Monad m, Functor col, Traversable col)
          => (forall sf . (a -> col sf -> col (b, sf)))
             -- ^ Routing function: determines the input to each signal function
             -- in the collection. IMPORTANT! The routing function has an
             -- obligation to preserve the structure of the signal function
             -- collection.
          -> col (SF m b c)
             -- ^ Initial signal function collection.
          -> SF m (a, Event (col (SF m b c) -> col (SF m b c))) (col c)
drpSwitch rf sfs =
  dpSwitch (rf . fst) sfs (arr (snd . fst)) $ \sfs' f ->
    noEventSnd >=- drpSwitch rf (f sfs')

-- * Parallel composition/switchers with "zip" routing

-- | Parallel composition of a list of SFs.
--
-- Given a list of SFs, returns an SF that takes a list of inputs, applies each
-- SF to each input in order, and returns the SFs' outputs.
--
-- >>> embed (parZ [arr (+1), arr (+2)]) (deltaEncode 0.1 [[0, 0], [1, 1]])
-- [[1,2],[2,3]]
--
-- If there are more SFs than inputs, an exception is thrown.
--
-- >>> embed (parZ [arr (+1), arr (+1), arr (+2)]) (deltaEncode 0.1 [[0, 0], [1, 1]])
-- [[1,1,*** Exception: FRP.Yampa.Switches.parZ: Input list too short.
--
-- If there are more inputs than SFs, the unused inputs are ignored.
--
-- >>> embed (parZ [arr (+1)]) (deltaEncode 0.1 [[0, 0], [1, 1]])
-- [[1],[2]]
parZ :: (Functor m, Monad m) => [SF m a b] -> SF m [a] [b]
parZ = par (safeZip "parZ")

-- | Parallel switch (dynamic collection of signal functions spatially composed
-- in parallel). See 'pSwitch'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
pSwitchZ :: (Functor m, Monad m)
         => [SF m a b]
         -> SF m ([a], [b]) (Event c)
         -> ([SF m a b] -> c -> SF m [a] [b])
         -> SF m [a] [b]
pSwitchZ = pSwitch (safeZip "pSwitchZ")

-- | Decoupled parallel switch with broadcasting (dynamic collection of signal
-- functions spatially composed in parallel). See 'dpSwitch'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
dpSwitchZ :: (Functor m, Monad m)
          => [SF m a b]
          -> SF m ([a], [b]) (Event c)
          -> ([SF m a b] -> c -> SF m [a] [b])
          -> SF m [a] [b]
dpSwitchZ = dpSwitch (safeZip "dpSwitchZ")

-- | Recurring parallel switch with "zip" routing.
--
-- Uses the given list of SFs, until an event comes in the input, in which case
-- the function in the 'Event' is used to transform the list of SF to be used
-- with 'rpSwitchZ' again, until the next event comes in the input, and so on.
--
-- Zip routing is used to decide which subpart of the input goes to each SF in
-- the list.
--
-- See 'rpSwitch'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
rpSwitchZ :: (Functor m, Monad m)
          => [SF m a b] -> SF m ([a], Event ([SF m a b] -> [SF m a b])) [b]
rpSwitchZ = rpSwitch (safeZip "rpSwitchZ")

-- | Decoupled recurring parallel switch with "zip" routing.
--
-- Uses the given list of SFs, until an event comes in the input, in which case
-- the function in the 'Event' is used to transform the list of SF to be used
-- with 'rpSwitchZ' again, until the next event comes in the input, and so on.
--
-- Zip routing is used to decide which subpart of the input goes to each SF in
-- the list.
--
-- See 'rpSwitchZ' and 'drpSwitch'.
--
-- For more information on how parallel composition works, check
-- <https://www.antonycourtney.com/pubs/hw03.pdf>
drpSwitchZ :: (Functor m, Monad m)
           => [SF m a b] -> SF m ([a], Event ([SF m a b] -> [SF m a b])) [b]
drpSwitchZ = drpSwitch (safeZip "drpSwitchZ")

-- | Zip two lists.
--
-- PRE: The first list is not shorter than the second.
safeZip :: String -> [a] -> [b] -> [(a, b)]
safeZip fn = safeZip'
  where
    safeZip' :: [a] -> [b] -> [(a, b)]
    safeZip' _      []     = []
    safeZip' (a:as) (b:bs) = (a, b) : safeZip' as bs
    safeZip' _      _      =
      error $ "FRP.BearRiver.Switches: " ++ fn ++ ": Input list too short."

-- Freezes a "running" signal function, i.e., turns it into a continuation in
-- the form of a plain signal function.
freeze :: Monad m => SF m a b -> DTime -> SF m a b
freeze sf dt = MSF $ \a ->
  lift $ runReaderT (unMSF sf a) dt

freezeCol :: (Monad m, Functor col)
          => col (SF m a b) -> DTime -> col (SF m a b)
freezeCol sfs dt = fmap (`freeze` dt) sfs

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
