{-# LANGUAGE Arrows              #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.Core (MSF (unMSF))
import Control.Monad.Trans.MSF
import Control.Monad.Trans.Class
import Data.Functor.Identity

type DTime = Double

data Event a = Event a | NoEvent

-- * Synchronous time transformations

-- | Transform the time domain in a synchronous, uni-directional way, applying
-- a transformation that may depend on the past and on the input.
timeTransformMSF :: Monad m
                 => MSF (ReaderT t1 m) a (t1 -> t2)
                 -> MSF (ReaderT t2 m) a b
                 -> MSF (ReaderT t1 m) a b
timeTransformMSF timeSF sf = MSF $ \a -> do
  (f, timeSF') <- unMSF timeSF a
  (b, sf')     <- withReaderT f (unMSF sf a)
  return (b, timeTransformMSF timeSF' sf')

-- | Transform the time domain in a synchronous, uni-directional way, applying
-- a point-wise (time-wise) pure transformation.
timeTransformF :: Monad m
               => (t1 -> t2)
               -> MSF (ReaderT t2 m) a b
               -> MSF (ReaderT t1 m) a b
timeTransformF f = timeTransformMSF (constant f)

-- | Run an MSF with a fixed time delta.
runAtFixedR :: Monad m
            => DTime -> MSF (ReaderT DTime m) a b -> MSF (ReaderT DTime m) a b
runAtFixedR dt = timeTransformF (const dt)

-- | Run an MSF with a maximum time delta.
runAtMaxR :: Monad m
          => DTime -> MSF (ReaderT DTime m) a b ->  MSF (ReaderT DTime m) a b
runAtMaxR dt = timeTransformF (min dt)

-- | Run an MSF with a minimum time delta.
runAtMinR :: Monad m
          => DTime -> MSF (ReaderT DTime m) a b -> MSF (ReaderT DTime m) a b
runAtMinR dt = timeTransformF (max dt)

-- * Asynchronous time transformations

-- ** Types

-- | An asynchronous MSF that may product output even when there is no input.
type AsyncMSF m a b = MSF m (Maybe a) b

-- type SelfClockingMSF m a b =
--   MSF (WriterT (DTime, Bool) (ReaderT DTime m)) a b

-- | An MSF with internal clocking information.
type SelfClockingMSF m a b =
  MSF (StateT (DTime, Bool) (ReaderT DTime m)) a b

-- ** Execution

-- runAtFixedS :: b
--             -> DTime
--             -> MSF (ReaderT DTime m) a b
--             -> SelfClockingMSF m a b
-- runAtFixedS b dt msf = timeTransformF (const dt) msf
--
-- runAtMaxS :: DTime -> MSF (ReaderT DTime m) a b -> SelfClockingMSF m a b
-- runAtMaxS dt msf = timeTransformF (min dt) msf
--
-- runAtMinS :: DTime -> MSF (ReaderT DTime m) a b -> SelfClockingMSF m a b
-- runAtMinS dt msf = timeTransformF (max dt) msf

-- | Make an MSF asynchronous. The default initial value is used for the output
-- until a first @Just@ value is received, and then the last output is cached.
mkAsyncMSF :: Monad m => b -> MSF  m a b -> MSF m (Maybe a) b
mkAsyncMSF b0 msf = MSF $ \ma ->
  case ma of
    Nothing -> return (b0, mkAsyncMSF b0 msf)
    Just a  -> do
      (b, msf') <- unMSF msf a
      return (b, mkAsyncMSF b msf')

-- | Run an MSF with clocking information multiple times, possibly outputing
-- several results.
runSelfClocking :: Monad m
                => SelfClockingMSF m a b -> MSF (ReaderT DTime m) a [(DTime, b)]
runSelfClocking msf = MSF $ \a -> do
  runSelfClocking' msf a []

runSelfClocking' :: Monad m
                 => SelfClockingMSF m a b
                 -> a
                 -> [(DTime, b)]
                 -> ReaderT DTime
                            m
                            ([(DTime, b)], MSF (ReaderT DTime m) a [(DTime, b)])
runSelfClocking' msf a acc = do
  dt <- ask
  ((b, msf'), (dt', last)) <- runStateT (unMSF msf a) (0, False)
  let acc' = acc ++ [(dt', b)]
  if last
    then return (acc', runSelfClocking msf')
    else withReaderT (\x -> x - dt') (runSelfClocking' msf' a acc')

-- | Run an MSF with clocking information multiple times, possibly outputing
-- several results, by merging the results.
resample :: Monad m
         => (DTime -> [(DTime, b)] -> b)
         -> SelfClockingMSF m a b
         -> MSF (ReaderT DTime m) a b
resample f msf = (curDT &&& runSelfClocking msf) >>> arr (uncurry f)

curDT :: Monad m => MSF (ReaderT DTime m) a DTime
curDT = arrM (\_ -> ask)

-- | Make an msf that runs on a binary clock that may or may not tick.
--
-- This is extremely similar to @mkAsyncMSF@, but that one has the option
-- embedded in the value and this one has it embedded in the clock.
possiblyRun :: Monad m => b -> MSF m a b -> MSF (ReaderT Bool m) a b
possiblyRun b0 msf = MSF $ \a -> do
  run <- ask
  if run
    then do (b, msf') <- lift (unMSF msf a)
            return (b, possiblyRun b msf')
    else return (b0, possiblyRun b0 msf )

-- | Monad for pure MSFs on a discrete (binary) clock.
type DiscreteMonad = Reader Bool

-- | Monad for MSFs on a discrete (binary) clock.
type ContinuousMonad = WriterT Bool (Reader DTime)

-- discreteIntoContinuous :: b -> MSF m a b -> MSF (ReaderT DTime m) a b
-- discreteIntoContinuous b0 msf = undefined
--
-- discreteCatchingUp :: b -> MSF m a b -> MSF (ReaderT Bool m) a b
-- discreteCatchingUp b0 msf = undefined

-- | Different forms of discrete-continuous board synchronization
syncBoards :: forall m a b c
           .  Monad m
           => MSF m a b
           -> MSF (StateT Bool (ReaderT DTime m)) b c
           -> MSF (ReaderT DTime m) a c
syncBoards msfDiscrete msfCont = MSF $ \a -> do
  (b, msfD') <- lift $ unMSF msfDiscrete a
  let r :: StateT
             Bool
             (ReaderT DTime m)
             (c, MSF (StateT Bool (ReaderT DTime m)) b c)
      r = unMSF msfCont b

      r2 :: ReaderT DTime m ((c, MSF (StateT Bool (ReaderT DTime m)) b c), Bool)
      r2 = runStateT r True

  ((c, msfC'), finished) <- r2
  return (c, syncBoards' (finished, b) msfD' msfC')

syncBoards' :: Monad m
            => (Bool, b)
            -> MSF m a b
            -> MSF (StateT Bool (ReaderT DTime m)) b c
            -> MSF (ReaderT DTime m) a c
syncBoards' (finished,b0) msfDiscrete msfCont = MSF $ \a -> do
  (b, msfD') <- if finished
                  then lift $ unMSF msfDiscrete a
                  else return (b0, msfDiscrete)
  ((c, msfC'), finished') <- runStateT (unMSF msfCont b) True
  return (c, syncBoards' (finished', b) msfD' msfC')

-- * Auxiliary operations
constant :: Arrow a => c -> a b c
constant = arr . const

-- * Examples

-- | Run an MSF with a frequency cap of 60FPS. If the time delta is more than
-- 60, it is executed at 60FPS once and indicated in the monad that more steps
-- can be executed.
game60FPS' :: MSF (Reader DTime) a b -> SelfClockingMSF Identity a b
game60FPS' game = MSF $ \a -> do
  dt <- lift $ ask
  let realDT = min dt (1 / 60)
      lastStep = dt < (1 / 60)
  put (realDT, lastStep)
  (output, game' ) <- lift $ withReader (const realDT) (unMSF game a)
  return (output, game60FPS' game')

reverseDTime :: MSF (Reader DTime) a b -> MSF (Reader DTime) a b
reverseDTime = timeTransformF ((-1)*)

type History a = [(DTime, a)]

lastSample :: History a -> (DTime, a)
lastSample as = last as

cache :: ((DTime, a) -> Maybe (DTime, a) -> DTime -> a) -> MSF (Reader DTime) a a
cache interpolate = cache' interpolate [] 0

-- | Possible bug if initial dt is zero
cache' :: ((DTime, a) -> Maybe (DTime, a) -> DTime -> a)
       -> History a
       -> DTime
       -> MSF (Reader DTime) a a
cache' interpolate history globalDTime = MSF $ \a -> do
  dt <- ask
  if (dt == 0)
    then return ( snd $ lastSample history
                , cache' interpolate history globalDTime
                )
    else -- dt <  0
         let globalDTime' = globalDTime + dt
             sample = sampleAt interpolate history globalDTime'
             history' = discardAfter history globalDTime'
             history'' = addSample  history' globalDTime' sample
         in return (sample, cache' interpolate history'' globalDTime')
  -- where
  --   tf a | dt == 0 = ( cache' interpolate history globalDTime
  --                    , snd $ lastSample history
  --                    )
  --        | dt <  0 = let globalDTime' = globalDTime + dt
  --                        sample = sampleAt interpolate history globalDTime'
  --                        history' = discardAfter history globalDTime'
  --                        history'' = addSample  history' globalDTime' sample
  --                    in (cache' interpolate history'' globalDTime', sample)

sampleAt :: ((DTime, a) -> Maybe (DTime, a) -> DTime -> a)
         -> History a
         -> DTime
         -> a
sampleAt interpolate history time =
  sampleAt' interpolate time (head history) (tail history)

sampleAt' interpolate time (t0, a0) [] =
  interpolate (t0, a0) Nothing time

sampleAt' interpolate time (t0, a0) ((t1,a1):hs)
  | time >= t0 && time <= t1 = interpolate (t0, a0) (Just (t1, a1)) time
  | otherwise                = sampleAt' interpolate time (t1,a1) hs

discardAfter :: History a -> DTime -> History a
discardAfter history time = filter (\(t, a) -> t <= time) history

addSample :: History a -> DTime -> a -> History a
addSample history time sample = addSample' time sample history

addSample' :: DTime -> a -> History a -> History a
addSample' time sample [] = [(time, sample)]
addSample' time sample h@((t1,a1):hs)
  | t1 == time = (time, sample):hs
  | t1 >  time = (time, sample):h
  | otherwise  = (t1,a1) : addSample' time sample hs

revSwitch :: MSF (Reader DTime) a (b, Event c)
          -> (c -> MSF (Reader DTime) a b)
          -> MSF (Reader DTime) a b
revSwitch msf k = MSF $ \a -> do
  ((b0, e), sf1) <- unMSF msf a
  case e of
     NoEvent  -> return (b0, switchAux sf1 k)
     Event c0 -> do (b1, sf2) <- unMSF (k c0) a
                    return (b1, switchingPoint sf1 k b1 sf2)
    -- where
    --   tf0 a0 =
    --     case tf10 a0 of
    --       (sf1, (b0, NoEvent))  -> (switchAux sf1 k, b0)
    --       (sf1, (_,  Event c0)) -> switchingPoint sf1 k (sfTF (k c0) a0)

 where
        switchingPoint :: MSF (Reader DTime) a (b, Event c)
                       -> (c -> MSF (Reader DTime) a b)
                       -> b
                       -> MSF (Reader DTime) a b
                       -> MSF (Reader DTime) a b
        switchingPoint sf1 k b sfN' = MSF $ \a -> do
           dt <- ask
           if | dt < 0  -> unMSF (switchAux sf1 k) a
              | dt > 0  -> do (b1, sfN'') <- unMSF sfN' a
                              return (b1, switchingPoint' sf1 k dt b1 sfN'')
              | dt == 0 -> return (b, switchingPoint sf1 k b sfN')
          -- where
          --   sf' = MSF tf'
          --   tf' dt a =
          --     if | dt < 0  -> unMSF (switchAux sf1 k) dt a
          --                     -- let (sf1', b') = unMSF sf1 dt a
          --                     -- in (switchAux sf1' k, b')
          --        | dt > 0  -> switchingPoint' sf1 k dt (unMSF sfN' dt a)
          --        | dt == 0 -> switchingPoint sf1 k (sfN', b)

        switchingPoint' :: MSF (Reader DTime) a (b, Event c)
                        -> (c -> MSF (Reader DTime) a b)
                        -> DTime
                        -> b
                        -> MSF (Reader DTime) a b
                        -> MSF (Reader DTime) a b
        switchingPoint' sf1 k accumDT b sfN' = MSF $ \a -> do
          dt <- ask
          let dt' = dt + accumDT
          if | dt < 0 -> if | dt' < 0
                            -> withReaderT
                                 (const dt')
                                 (unMSF (switchAux sf1 k) a)

                            | dt' > 0
                            -> dt' `seq` do
                                 (b1, sfN'') <- unMSF sfN' a
                                 return ( b1
                                        , switchingPoint' sf1 k dt' b1 sfN''
                                        )

                            | dt' == 0
                            -> return ( b
                                      , switchingPoint' sf1 k accumDT b sfN'
                                      )

              | dt > 0 -> dt' `seq` do
                  (b1, sfN'') <- unMSF sfN' a
                  return (b1, switchingPoint' sf1 k dt' b1 sfN'')

              | dt == 0 -> return (b, switchingPoint' sf1 k accumDT b sfN')

        switchAux :: MSF (Reader DTime) a (b, Event c)
                  -> (c -> MSF (Reader DTime) a b)
                  -> MSF (Reader DTime) a b
        switchAux sf1 k = MSF $ \a -> do
          ((b, e), sf1') <- unMSF sf1 a
          case e of
           NoEvent -> return (b, switchAux sf1' k)
           Event c -> do (b1, sf1'') <- unMSF (k c) a
                         return (b1, switchingPoint sf1 k b1 sf1'')

checkpoint :: MSF (Reader DTime) a (b, Event (), Event ())
           -> MSF (Reader DTime) a b
checkpoint sf = MSF $ \a -> do
   ((b, advance, reset), sf') <- unMSF sf a
   case reset of
        Event () -> error "loop"
        NoEvent -> let pt = case advance of
                              Event () -> Left sf'
                              NoEvent  -> Right sf
                   in return (b, checkpoint' pt sf')

checkpoint' :: Either (MSF (Reader DTime) a (b, Event (), Event ()))
                      (MSF (Reader DTime) a (b, Event (), Event ()))
            -> (MSF (Reader DTime) a (b, Event (), Event ()))
            -> MSF (Reader DTime) a b
checkpoint' rstPt sf' = MSF $ \a -> do
   ((b, advance, reset), sf'') <- unMSF sf' a
   case reset of
     Event () -> case rstPt of
                   Left sf''' -> unMSF (checkpoint' rstPt sf''') a
                   Right sf   -> unMSF (checkpoint sf) a
     NoEvent -> let pt = case advance of
                           Event () -> Left sf''
                           NoEvent -> rstPt
                in return (b, checkpoint' pt sf'')

forgetPast :: Monad m => MSF (ReaderT DTime m) a b -> MSF (ReaderT DTime m) a b
forgetPast sf = MSF $ \a -> do
   (b, sf') <- unMSF sf a
   return (b, forgetPast' 0 sf')

forgetPast' :: Monad m
            => DTime -> MSF (ReaderT DTime m) a b -> MSF (ReaderT DTime m) a b
forgetPast' time sf' = MSF $ \a -> do
  dt <- ask
  let time' = time + dt
  -- trace (show time') $
  if time' < 0
    then do (b, sf'') <- withReaderT (const (-time)) (unMSF sf' a)
            return (b, forgetPast' 0 sf'')
    else do (b, sf'') <- unMSF sf' a
            return (b, forgetPast' time' sf'')

alwaysForward :: MSF (Reader DTime) a b -> MSF (Reader DTime) a b
alwaysForward = timeTransformF (\dt -> max dt (-dt))

limitHistory :: DTime -> MSF (Reader DTime) a b -> MSF (Reader DTime) a b
limitHistory time sf = MSF $ \a -> do
  (b, sf') <- unMSF sf a
  return (b, limitHistory' 0 time sf')

limitHistory' :: DTime
              -> DTime
              -> MSF (Reader DTime) a b
              -> MSF (Reader DTime) a b
limitHistory' curT maxT sf' = MSF $ \a -> do
  dt <- ask
  let curT' = curT + dt
      time' = if curT' > maxT then maxT else curT'
  -- trace (show (dt, curT, maxT, maxMaxT)) $
  if time' < 0
    then do (b, sf'') <- withReaderT (\_ -> -curT) (unMSF sf' a)
            return (b, limitHistory' 0 maxT sf'')
    else do (b, sf'') <- unMSF sf' a
            return (b, limitHistory' time' maxT sf'')

clocked :: MSF (Reader DTime) a DTime
        -> MSF (Reader DTime) a b
        -> MSF (Reader DTime) a b
clocked clockSF sf = MSF $ \a -> do
  (b, sf')  <- unMSF sf a
  (_, cSF') <- unMSF clockSF a
  return (b, clocked' cSF' sf')

clocked' :: MSF (Reader DTime) a DTime
         -> MSF (Reader DTime) a b
         -> MSF (Reader DTime) a b
clocked' clockSF sf = MSF $ \a -> do
  (dt', cSF') <- unMSF clockSF a
  (b, sf')    <- withReaderT (\_ -> dt')  (unMSF sf a)
  return (b, clocked' cSF' sf')
