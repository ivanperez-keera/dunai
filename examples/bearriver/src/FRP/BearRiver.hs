-- {-# LANGUAGE ExistentialQuantification #-}
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
import           Control.Monad                (mapM)
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.MStreamF
import           Data.Traversable             as T
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MonadicStreamFunction   as X hiding (iPre, reactimate, switch, sum, trace)
import qualified Data.MonadicStreamFunction   as MSF
import           Data.MonadicStreamFunction.ArrowLoop
import           FRP.Yampa.VectorSpace        as X

type Time  = Double
type DTime = Double

type SF        = MStreamF ClockInfo
type ClockInfo = Reader DTime

identity :: SF a a
identity = arr id

constant :: b -> SF a b
constant = arr . const

iPre :: a -> SF a a
iPre i = MStreamF $ \i' -> return (i, iPre i')

-- * Continuous time

time :: SF () Time
time = integral <<< constant 1

integral :: VectorSpace a s => SF a a
integral = integralFrom zeroVector

integralFrom :: VectorSpace a s => a -> SF a a
integralFrom n0 = MStreamF $ \n -> do
  dt <- ask
  let acc = n0 ^+^ realToFrac dt *^ n
  acc `seq` return (acc, integralFrom acc)

derivative :: VectorSpace a s => SF a a
derivative = derivativeFrom zeroVector

derivativeFrom :: VectorSpace a s => a -> SF a a
derivativeFrom n0 = MStreamF $ \n -> do
  dt <- ask
  let res = (n ^-^ n0) ^/ realToFrac dt
  res `seq` return (res, derivativeFrom n)

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
lMerge = mergeBy (\e1 _e2 -> e1)

-- ** Relation to other types

eventToMaybe = event Nothing Just
maybeToEvent = maybe NoEvent Event

boolToEvent :: Bool -> Event ()
boolToEvent True  = Event ()
boolToEvent False = NoEvent

-- * Hybrid SF combinators

edge :: SF Bool (Event ())
edge = edgeFrom True

edgeBy :: (a -> a -> Maybe b) -> a -> SF a (Event b)
edgeBy isEdge a_prev = MStreamF $ \a ->
  return (maybeToEvent (isEdge a_prev a), edgeBy isEdge a)

edgeFrom :: Bool -> SF Bool (Event())
edgeFrom prev = MStreamF $ \a -> do
  let res = if prev then NoEvent else if a then Event () else NoEvent
      ct  = edgeFrom a
  return (res, ct)

-- | Suppression of initial (at local time 0) event.
notYet :: SF (Event a) (Event a)
notYet = feedback False $ arr (\(e,c) ->
  if c then (e, True) else (NoEvent, True))

hold :: a -> SF (Event a) a
hold a = feedback a $ arr $ \(e,a') ->
  dup (event a' id e)
 where dup x = (x,x)

loopPre :: c -> SF (a, c) (b, c) -> SF a b
loopPre = feedback

after :: Time -- ^ The time /q/ after which the event should be produced
      -> b    -- ^ Value to produce at that time
      -> SF a (Event b)
after q x = feedback q $ go
 where go = MStreamF $ \(_, t) -> do
              dt <- ask
              let t' = t - dt
                  e  = if t > 0 && t' < 0 then Event x else NoEvent
                  ct = if t' < 0 then constant (NoEvent, t') else go
              return ((e, t'), ct)

(-->) :: b -> SF a b -> SF a b
b0 --> sf = MStreamF $ \a -> do 
  (_, ct) <- unMStreamF sf a
  return (b0, ct)

accumHoldBy :: (b -> a -> b) -> b -> SF (Event a) b
accumHoldBy f b = feedback b $ arr $ \(a, b') ->
  let b'' = event b' (f b') a
  in (b'', b'')

dpSwitchB :: Traversable col
          => col (SF a b) -> SF (a, col b) (Event c) -> (col (SF a b) -> c -> SF a (col b))
          -> SF a (col b)
dpSwitchB sfs sfF sfCs = MStreamF $ \a -> do
  res <- T.mapM (`unMStreamF` a) sfs
  let bs   = fmap fst res
      sfs' = fmap snd res
  (e,sfF') <- unMStreamF sfF (a, bs)
  let ct = case e of
             Event c -> sfCs sfs' c
             NoEvent -> dpSwitchB sfs' sfF' sfCs 
  return (bs, ct)

dSwitch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
dSwitch sf sfC = MStreamF $ \a -> do
  (o, ct) <- unMStreamF sf a
  case o of
    (b, Event c) -> do (_,ct') <- unMStreamF (sfC c) a
                       return (b, ct')
    (b, NoEvent) -> return (b, dSwitch ct sfC)

switch :: SF a (b, Event c) -> (c -> SF a b) -> SF a b
switch sf sfC = MStreamF $ \a -> do
  (o, ct) <- unMStreamF sf a
  case o of
    (_, Event c) -> unMStreamF (sfC c) a
    (b, NoEvent) -> return (b, switch ct sfC)

parC :: SF a b -> SF [a] [b]
parC sf = parC' [sf]

parC' :: [SF a b] -> SF [a] [b]
parC' sfs = MStreamF $ \as -> do
  os <- T.mapM (\(a,sf) -> unMStreamF sf a) $ zip as sfs
  let bs  = fmap fst os
      cts = fmap snd os
  return (bs, parC' cts)

-- NOTE: BUG in this function, it needs two a's but we
-- can only provide one
iterFrom :: (a -> a -> DTime -> b -> b) -> b -> SF a b
iterFrom f b = MStreamF $ \a -> do
  dt <- ask
  let b' = f a a dt b
  return (b, iterFrom f b')

reactimate :: IO a -> (Bool -> IO (DTime, Maybe a)) -> (Bool -> b -> IO Bool) -> SF a b -> IO ()
reactimate senseI sense actuate sf = do
  -- runMaybeT $ MSF.reactimate $ liftMStreamFTrans (senseSF >>> sfIO) >>> actuateSF
  MSF.reactimateB $ senseSF >>> sfIO >>> actuateSF
  return ()
 where sfIO        = liftMStreamFPurer (return.runIdentity) (runReaderS sf)

       -- Sense
       senseSF     = switch senseFirst senseRest
       senseFirst  = liftMStreamF_ senseI >>> (arr $ \x -> ((0, x), Event x))
       senseRest a = liftMStreamF_ (sense True) >>> (arr id *** keepLast a)

       keepLast :: Monad m => a -> MStreamF m (Maybe a) a
       keepLast a = MStreamF $ \ma -> let a' = fromMaybe a ma in return (a', keepLast a')

       -- Consume/render
       -- actuateSF :: MStreamF IO b ()
       -- actuateSF    = arr (\x -> (True, x)) >>> liftMStreamF (lift . uncurry actuate) >>> exitIf
       actuateSF    = arr (\x -> (True, x)) >>> liftMStreamF (uncurry actuate)

       switch sf sfC = MSF.switch (sf >>> second (arr eventToMaybe)) sfC

-- * Auxiliary

-- ** Tuples

dup  x     = (x,x)
swap (x,y) = (y,x)
