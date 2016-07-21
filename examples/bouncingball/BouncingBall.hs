{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP    #-}

#ifdef BEARRIVER
import FRP.BearRiver as Yampa
#else
import FRP.Yampa     as Yampa
#endif

import Control.Concurrent
import Graphics.UI.SDL            as SDL
import Graphics.UI.SDL.Primitives as SDL

main = do
   SDL.init [InitEverything]
   SDL.setVideoMode 800 600 32 [SWSurface]
   reactimate (return ())
              (\_ -> return (0.01, Just ()))
              (\_ e -> render e >> return False)
              sf

sf = bouncingBall (100.0 :: Float) 0.0


bouncingBall p0 v0 =
  switch (proc (_) -> do
            (p,v)  <- fallingBall p0 v0  -< ()
            bounce <- edge               -< (p <= 0 && v < 0)
            returnA -< ((p,v), bounce `tag` (p,v))
         )
         (\(p,v) -> bouncingBall p (-v))

fallingBall p0 v0 = proc () -> do
  v <- (v0 +) ^<< integral -< (-99.8)
  p <- (p0 +) ^<< integral -< v
  returnA -< (p, v)

-- bouncingBall p0 v0 =
--   switch (fallingBall p0 v0 >>> (arr id &&& whenS (\(p,v) -> p <= 0 && v < 0)))
--          (\(p,v) -> bouncingBall p (-v))
--
-- fallingBall p0 v0 = proc () -> do
--   v <- (v0 +) ^<< integral -< (-9.8)
--   p <- (p0 +) ^<< integral -< v
--   returnA -< (p, v)
--
-- whenS :: (a -> Bool) -> SF a (Yampa.Event a)
-- whenS p = (((arr p >>> edge) &&& arr id) >>> (arr (uncurry tag)))

render (p,_) = do
  screen <- SDL.getVideoSurface

  white <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xFF 0xFF 0xFF
  SDL.fillRect screen Nothing white

  SDL.filledCircle screen 100 (600 - 100 - round p) 30 (Pixel 0xFF0000FF)

  SDL.flip screen

  threadDelay 1000
