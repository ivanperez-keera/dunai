{-# LANGUAGE Arrows       #-}
import Control.Arrow
import qualified Control.Category
import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Monoid
import Data.Time
import Sound.Pulse.Simple
import Graphics.UI.SDL as SDL

import Control.Monad.Trans.MStreamF
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.Parallel
import Data.MonadicStreamFunction.ReactHandle

mySumFrom :: (Num a, Monad m) => a -> MStreamF m a a
mySumFrom a = arr Sum >>> sumFrom (Sum a) >>> arr getSum


sine :: Float -> MStreamF (ReaderT Float IO) () Float
sine f = stopwatch >>> arr ((* (2*pi*f)) >>> sin >>> (* 0.5))


osc :: Float -> MStreamF (ReaderT Float IO) () Float
osc freq  = osc' 0.5 0 where
    osc' :: Monad m => Float -> Float -> MStreamF (ReaderT Float m) () Float
    osc' x v = MStreamF $ \_ -> do
        delta <- ask
        let x' = x + delta * v
        let v' = v - delta * 4 * (pi*freq)^2 * x'
        return (x, osc' x' v')

-- Maybe could win performance by hardcoding the time delta

osc1 :: Float -> MStreamF (ReaderT Float IO) () Float
osc1 freq = feedback 0.5 osc1' where
    w = 4*(pi*freq)^2
    osc1' :: MStreamF (ReaderT Float IO) ((), Float) (Float, Float)
    osc1' = proc (_, x) -> do
        delta <- liftMStreamF_ ask -< ()
        lastX <- iPre 0.5     -< x
        let newX = (2 - w * delta^2) * x - lastX
        returnA -< (newX, newX)

osc2 :: Float -> MStreamF (ReaderT Float IO) () Float
osc2 freq = osc2' 0.5 0.5 where
    w = 4*(pi*freq)^2
    osc2' :: Float -> Float -> MStreamF (ReaderT Float IO) () Float
    osc2' x lastX = MStreamF $ \_ -> do
        delta <- ask
        let newX = (2 - w * delta^2) * x - lastX
        return (newX, osc2' newX x)

osc3 :: Float -> MStreamF (ReaderT Float IO) () Float
osc3 freq = osc3' 0.5 0.5 where
    w = 4*(pi*freq)^2
    delta = 1 / 44100
    bla = (2 - w * delta^2)
    osc3' :: Float -> Float -> MStreamF (ReaderT Float IO) () Float
    osc3' x lastX = MStreamF $ \_ -> do
        let newX = bla * x - lastX
        return (newX, osc3' newX x)


osc4 :: Float -> MStreamF (ReaderT Float IO) () Float
osc4 freq = osc4' 0.5 0.5 where
    w = 4*(pi*freq)^2
    delta = 1 / 44100
    bla = (2 - w * delta^2)
    osc4' :: Float -> Float -> MStreamF (ReaderT Float IO) () Float
    osc4' x lastX = MStreamF $ \_ -> do
        let newX = bla * x - lastX
        return (newX, osc4' newX x)

stopwatch :: (Monad m, Num a) => MStreamF (ReaderT a m) b a
stopwatch = liftMStreamF_ ask >>> mySumFrom 0

-- This is the wrong one if you want to create samples for a soundcard
timeDeltas :: Fractional a => UTCTime -> MStreamF IO () a
timeDeltas startTime = proc _ -> do
  currentTime <- liftMStreamF_ getCurrentTime -< ()
  lastTime    <- iPre startTime               -< currentTime
  returnA -< realToFrac $ currentTime `diffUTCTime` lastTime



edgeDown :: Monad m => MStreamF (ExceptT () m) Bool ()
edgeDown = proc b -> do
  if not b
    then throwS -< ()
    else pass   -< ()

edgeUp :: Monad m => MStreamF (ExceptT () m) Bool ()
edgeUp = proc b -> do
  if b
    then throwS -< ()
    else pass   -< ()


data AttackStopped = MaxReached | EdgeDown Float
  deriving Show
data EarlyAttack = Released | AttackAt Float
  deriving Show

adsrE' :: Float -> Float -> Float -> Float
     -> MStreamF (ReaderT Float IO) Bool Float
adsrE' a d s r = safely $ adsrE'' 0
  where
    adsrE'' fromLevel = do
      waitForIt
      once_ $ lift $ putStrLn $ "Attacking from " ++ show fromLevel
      attacked <- try $ attack fromLevel
      earlyAttack <- case attacked of
        MaxReached            -> do
          once_ $ lift $ putStrLn "Max attack reached"
          try decay
          once_ $ lift $ putStrLn "Decayed"
          try sustain
          once_ $ lift $ putStrLn "Sustained"
          try $ release s
        EdgeDown releaseLevel -> do
          once_ $ lift $ putStrLn "Stopped attack early"
          try $ release releaseLevel
      once_ $ lift $ putStrLn "Released"
      case earlyAttack of
        Released -> do
          waitForIt
          adsrE'' 0
        AttackAt level -> adsrE'' level
    waitForIt = try $ edgeUp >>> arr (const 0)

    attack fromLevel = proc b -> do
      time <- liftMStreamFTrans stopwatch -< ()
      let level = (time + fromLevel) / a
      tagged edgeDown -< (b, EdgeDown level)
      if level > 1
        then throwS -< MaxReached
        else returnA -< level
    decay = proc _ -> do
      time <- liftMStreamFTrans stopwatch -< ()
      let level = 1 - time / (d * (1-s)) -- Recheck
      if level <= s
        then throwS -< ()
        else returnA -< level
    sustain = edgeDown >>> arr (const s)
    release fromLevel = proc b -> do
      time <- liftMStreamFTrans stopwatch -< ()
      let level = fromLevel - s*time/r
      tagged edgeUp -< (b, AttackAt level)
      if level <= 0
        then throwS -< Released
        else returnA -< level


hull :: MStreamF (ReaderT Float IO) Bool Float
hull = adsrE' 0.01 0.05 0.8 0.2

data SDLEventClock = SDLEventClock { selectEvent :: SDL.Event -> Bool }
mouseUpDownClock :: SDLEventClock
mouseUpDownClock =  SDLEventClock selectEvent where
    selectEvent (SDL.MouseButtonDown _ _ _) = True
    selectEvent (SDL.MouseButtonUp   _ _ _) = True
    selectEvent _                       = False


blockUntilEvent (SDLEventClock selectEvent) = liftMStreamF_ $ do
    event <- waitForRightEvent selectEvent
    return event
waitForRightEvent selectEvent = do
    event <- waitEvent
    if selectEvent event then return event else waitForRightEvent selectEvent

stupidHull :: MStreamF (ReaderT Float IO) Bool Float
stupidHull = arr (\b -> if b then 1 else 0)

sig1 = (hull &|& ((arr (const ())) >>> (((osc4 440 &|& osc4 550) >>> arr (uncurry (+))) &|& osc4 660) >>> arr (uncurry (+)))) >>> arr (uncurry (*))


sig2 = proc b -> if b
  then osc4 440 -< ()
  else osc4 330 -< ()

sighull = proc b -> do
  signal <- osc4 440 -< ()
  amp    <- hull     -< b
  returnA -< signal * amp


keep :: Monad m => a -> MStreamF m (Maybe a) a
keep a = MStreamF $ \ma -> do
  case ma of
    Nothing -> return (a, keep a)
    Just a' -> return (a', keep a')
readSometimes :: Monad m => b -> Integer -> MStreamF m a b -> MStreamF m a b
readSometimes _ n _ | n < 1 = error "Must be a positive integer"
readSometimes b n msf = proc a -> do
  k <- count -< ()
  mb <- if k `mod` n == 0
    then arr Just <<< msf -< a
    else Control.Category.id -< Nothing
  keep b -< mb

main :: IO ()
main = out
out = do
    SDL.init [SDL.InitEverything]
    SDL.setVideoMode 600 600 32 [SDL.HWSurface]
    let samplesPerSecond = 44100
    s <- simpleNew Nothing "example" Play Nothing "bla" (SampleSpec (F32 LittleEndian) samplesPerSecond 1) Nothing Nothing
    (buttonWrite, buttonRead) <- createWormhole False
    currentTime <- getCurrentTime
    forkIO $ reactimate $ blockUntilEvent mouseUpDownClock >>> count >>> arr odd >>> buttonWrite
    reactimate $ proc _ -> do
      let dt = 1 / fromIntegral samplesPerSecond
      button <- readSometimes False 1024 (buttonRead) -< ()
      signal <- runReaderS sig1 -< (dt, button)
      liftMStreamF (simpleWrite s) -< [signal]
    simpleDrain s
    simpleFree s
