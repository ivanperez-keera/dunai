{-# LANGUAGE Arrows #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Reader
import           Data.MonadicStreamFunction hiding (reactimate, switch, trace)
import           Data.MonadicStreamFunction.InternalCore (MSF(..))
import qualified Data.MonadicStreamFunction as MSF
import           Debug.Trace
import           FRP.Yampa                  as Yampa
import           Graphics.UI.SDL            as SDL
import           Graphics.UI.SDL.Primitives as SDL
import           ListT                      as L

main = do
   SDL.init [InitEverything]
   SDL.setVideoMode 800 600 32 [SWSurface]
   reactimate (getMouse)
              (\_ -> getMouse >>= (\p -> return (0.02, Just p)))
              (\_ e -> render e >> return False)
              bouncingBalls

bouncingBalls = proc (mp@(mx, my, ml, mr)) -> do
  b   <- bouncingBall (100.0 :: Float) (0.0) -< () -- Just to be sure the game is running

  -- More balls, started on clicks
  ml' <- isEvent ^<< edge -< ml
  bs  <- fireballs        -< (ml', (my, 0))
  returnA -< (b : bs)

fireballs :: SF (Bool, (Float, Float)) [(Float, Float)]
fireballs = switch
  (arr (const []) &&& arr (\(mp, pos) -> if mp then Event pos else Yampa.NoEvent))

  (\(p, v) -> let oldfb = voidI $ runListMSF (liftTransS (bouncingBall p v))
                  newfb = fireballs
              in (oldfb &&& newfb) >>> arr2 (++)
  )

bouncingBall :: Float -> Float -> SF () (Float, Float)
bouncingBall p0 v0 =
  switch (proc (_) -> do
            (p,v)  <- fallingBall p0 v0  -< ()
            bounce <- edge               -< (p <= 0 && v < 0)
            returnA -< ((p,v), bounce `tag` (p, v))
         )
         (\(p,v) -> bouncingBall 0 (flippedVel p0 v0 (-99.8)))

-- Calculates the flipped velocity in terms of total energy instead of flipping
-- the current velocity. This produces collisions without energy loss. I do not
-- know how to generalise this for all collisions.
flippedVel p0 v0 acc = sqrt (2 * (ike + ipe))
  where ike = abs (acc * p0)
        ipe = (v0**2)/2

fallingBall :: Float -> Float -> SF () (Float, Float)
fallingBall p0 v0 = proc () -> do
  v <- (v0 +) ^<< integral -< (-99.8)
  p <- (p0 +) ^<< integral -< v
  returnA -< (p, v)

-- Input
getMouse :: IO (Float, Float, Bool, Bool)
getMouse = do
  pumpEvents
  (x,y,btns) <- SDL.getMouseState
  let left  = ButtonLeft  `elem` btns
      right = ButtonRight `elem` btns
  return (fromIntegral x, fromIntegral y, left, right)

-- Output
render ps = do
  screen <- SDL.getVideoSurface

  white <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xFF 0xFF 0xFF
  SDL.fillRect screen Nothing white

  mapM_ (\((p,_),xi) ->
     SDL.filledCircle screen (100 + xi * 100) (600 - 100 - round p) 30 (Pixel 0xFF0000FF))
     (zip ps [1..])

  SDL.flip screen

  threadDelay 1000

-- Auxiliary MSF functions
applyMSF :: Monad m => (a -> MSF m b c) -> MSF m (a, b) c
applyMSF f = MSF $ \(a,b) -> do
  (c, msf') <- unMSF (f a) b
  return (c, arr snd >>> msf')

runListMSF :: (Functor m, Monad m) => MSF (ListT m) a b -> MSF m a [b]
runListMSF msf = runListMSF' [msf]
  where
    runListMSF' msfs = MSF $ \a -> do
        (bs, msfs') <- unzip . concat <$> mapM (toList . (`unMSF` a)) msfs
        return (bs, runListMSF' msfs')

-- Auxiliary Arrow functions
voidI :: Arrow a => a () c -> a b c
voidI =  (>>>) (arr (const ()))

arr2 :: Arrow a => (b -> c -> d) -> a (b,c) d
arr2 f = arr (uncurry f)
