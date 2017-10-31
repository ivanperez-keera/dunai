{-# LANGUAGE Arrows     #-}
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
import           Control.Arrow                as X
import qualified Control.Category             as Category
import           Control.Monad                (mapM)
--import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.MSF
import           Data.Traversable             as T
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MonadicStreamFunction   as X hiding (reactimate, switch, sum, trace)
import qualified Data.MonadicStreamFunction   as MSF
import           Data.MonadicStreamFunction.Instances.ArrowLoop
import           FRP.Yampa.VectorSpace        as X

type Time  = Double
type DTime = Double

type SF m        = MSF (ClockInfo m)
type ClockInfo m = ReaderT DTime m

identity :: Monad m => SF m a a
identity = Category.id

constant :: Monad m => b -> SF m a b
constant = arr . const


-- * Continuous time

time :: Monad m => SF m () Time
time = integral <<< constant 1

integral :: (Monad m, VectorSpace a s) => SF m a a
integral = integralFrom zeroVector

integralFrom :: (Monad m, VectorSpace a s) => a -> SF m a a
integralFrom a0 = proc a -> do
  dt <- arrM_ ask         -< ()
  accumulateWith (^+^) a0 -< realToFrac dt *^ a

derivative :: (Monad m, VectorSpace a s) => SF m a a
derivative = derivativeFrom zeroVector

derivativeFrom :: (Monad m, VectorSpace a s) => a -> SF m a a
derivativeFrom a0 = proc a -> do
  dt   <- arrM_ ask   -< ()
  aOld <- MSF.iPre a0 -< a
  returnA             -< (a ^-^ aOld) ^/ realToFrac dt

-- * Events

data Event a = Event a | NoEvent
 deriving Show

instance Functor Event where
  fmap f NoEvent   = NoEvent
  fmap f (Event c) = Event (f c)

instance Applicative Event where
  pure = Event

  Event f <*> Event x = Event (f x)
  _       <*> _       = NoEvent

noEvent :: Event a
noEvent = NoEvent

event :: a -> (b -> a) -> Event b -> a
event _ f (Event x) = f x
event x _ NoEvent   = x

fromEvent (Event x) = x
fromEvent _         = error "fromEvent NoEvent"

isEvent (Event _) = True
isEvent _         = False

tag :: Event a -> b -> Event b
tag NoEvent   _ = NoEvent
tag (Event _) b = Event b

mergeBy :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeBy _       NoEvent      NoEvent      = NoEvent
mergeBy _       le@(Event _) NoEvent      = le
mergeBy _       NoEvent      re@(Event _) = re
mergeBy resolve (Event l)    (Event r)    = Event (resolve l r)

lMerge :: Event a -> Event a -> Event a
lMerge = mergeBy (\e1 _ -> e1)

-- ** Relation to other types

eventToMaybe = event Nothing Just
maybeToEvent = maybe NoEvent Event

boolToEvent :: Bool -> Event ()
boolToEvent True  = Event ()
boolToEvent False = NoEvent

-- * Hybrid SF m combinators

edge :: Monad m => SF m Bool (Event ())
edge = edgeFrom True

edgeBy :: Monad m => (a -> a -> Maybe b) -> a -> SF m a (Event b)
edgeBy isEdge a_prev = MSF $ \a ->
  return (maybeToEvent (isEdge a_prev a), edgeBy isEdge a)

edgeFrom :: Monad m => Bool -> SF m Bool (Event())
edgeFrom prev = MSF $ \a -> do
  let res | prev      = NoEvent
          | a         = Event ()
          | otherwise = NoEvent
      ct  = edgeFrom a
  return (res, ct)

-- | Suppression of initial (at local time 0) event.
notYet :: Monad m => SF m (Event a) (Event a)
notYet = feedback False $ arr (\(e,c) ->
  if c then (e, True) else (NoEvent, True))

hold :: Monad m => a -> SF m (Event a) a
hold a = feedback a $ arr $ \(e,a') ->
  dup (event a' id e)
 where dup x = (x,x)

loopPre :: Monad m => c -> SF m (a, c) (b, c) -> SF m a b
loopPre = feedback

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

(-->) :: Monad m => b -> SF m a b -> SF m a b
b0 --> sf = MSF $ \a -> do
  (_, ct) <- unMSF sf a
  return (b0, ct)

accumHoldBy :: Monad m => (b -> a -> b) -> b -> SF m (Event a) b
accumHoldBy f b = feedback b $ arr $ \(a, b') ->
  let b'' = event b' (f b') a
  in (b'', b'')

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

dSwitch ::  Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
dSwitch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (b, Event c) -> do (_,ct') <- unMSF (sfC c) a
                       return (b, ct')
    (b, NoEvent) -> return (b, dSwitch ct sfC)

switch :: Monad m => SF m a (b, Event c) -> (c -> SF m a b) -> SF m a b
switch sf sfC = MSF $ \a -> do
  (o, ct) <- unMSF sf a
  case o of
    (_, Event c) -> unMSF (sfC c) a
    (b, NoEvent) -> return (b, switch ct sfC)

parC :: Monad m => SF m a b -> SF m [a] [b]
parC sf = parC' [sf]

parC' :: Monad m => [SF m a b] -> SF m [a] [b]
parC' sfs = MSF $ \as -> do
  os <- T.mapM (\(a,sf) -> unMSF sf a) $ zip as sfs
  let bs  = fmap fst os
      cts = fmap snd os
  return (bs, parC' cts)

-- NOTE: BUG in this function, it needs two a's but we
-- can only provide one
iterFrom :: Monad m => (a -> a -> DTime -> b -> b) -> b -> SF m a b
iterFrom f b = MSF $ \a -> do
  dt <- ask
  let b' = f a a dt b
  return (b, iterFrom f b')

reactimate :: Monad m => m a -> (Bool -> m (DTime, Maybe a)) -> (Bool -> b -> m Bool) -> SF Identity a b -> m ()
reactimate senseI sense actuate sf = do
  -- runMaybeT $ MSF.reactimate $ liftMSFTrans (senseSF >>> sfIO) >>> actuateSF
  MSF.reactimateB $ senseSF >>> sfIO >>> actuateSF
  return ()
 where sfIO        = liftMSFPurer (return.runIdentity) (runReaderS sf)

       -- Sense
       senseSF     = switch senseFirst senseRest
       senseFirst  = arrM_ senseI >>> (arr $ \x -> ((0, x), Event x))
       senseRest a = arrM_ (sense True) >>> (arr id *** keepLast a)

       keepLast :: Monad m => a -> MSF m (Maybe a) a
       keepLast a = MSF $ \ma -> let a' = fromMaybe a ma in return (a', keepLast a')

       -- Consume/render
       -- actuateSF :: MSF IO b ()
       -- actuateSF    = arr (\x -> (True, x)) >>> liftMSF (lift . uncurry actuate) >>> exitIf
       actuateSF    = arr (\x -> (True, x)) >>> arrM (uncurry actuate)

       switch sf sfC = MSF.switch (sf >>> second (arr eventToMaybe)) sfC

-- * Auxiliary

-- ** Tuples

dup  x     = (x,x)
swap (x,y) = (y,x)
