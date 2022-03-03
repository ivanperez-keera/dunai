{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP    #-}

#ifdef BEARRIVER
import FRP.BearRiver as Yampa
#else
import FRP.Yampa     as Yampa
#endif

import Control.Concurrent
import Data.IORef
import System.Clock
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

main = do
   SDL.init [InitEverything]
   SDL.setVideoMode 800 600 32 [SWSurface]
   t0 <- getTime Monotonic
   r <- newIORef t0
   reactimate (return ())
              (\_ -> sense r)
              (\_ e -> render e >> return False)
              sf

sense :: IORef TimeSpec -> IO (Double, Maybe ())
sense r = do
  tPrev <- readIORef r
  t <- getTime Monotonic
  writeIORef r t
  let delta = fromIntegral (toNanoSecs (diffTimeSpec t tPrev)) / (1000000000.0 :: Double)
  return (delta, Just ())

sf = bouncingBall (2.0 :: Double) 0.0

bouncingBall p0 v0 =
  switch (proc (_) -> do
            (p,v)  <- fallingBall p0 v0  -< ()
            bounce <- edge               -< (p <= 0 && v < 0)
            returnA -< ((p,v), bounce `tag` (p,v))
         )
         (\(p,v) -> bouncingBall p (-v))

fallingBall p0 v0 = proc () -> do
  v <- (v0 +) ^<< integral -< (-9.8)
  p <- (p0 +) ^<< integral <<< averageLast v0 -< v
  returnA -< (p, v)

averageLast init = arr (^/2) >>> arr id &&& iPre (zeroVector ^+^ init ^/ 2) >>> arr (uncurry (^+^))

render (p,_) = do
  screen <- SDL.getVideoSurface

  white <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xFF 0xFF 0xFF
  SDL.fillRect screen Nothing white

  SDL.fillRect screen (Just (Rect 0 550 800 600)) (Pixel 0x00000000)
  let sphereRadius = 30
  SDL.filledCircle screen 100 (550 - sphereRadius - round (p*100)) sphereRadius (Pixel 0xFF0000FF)

  SDL.flip screen

  threadDelay 1000
