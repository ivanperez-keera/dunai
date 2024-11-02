module Test where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.MSF
import           Data.Maybe
import           Data.MonadicStreamFunction hiding (reactimate, switch, trace)
import qualified Data.MonadicStreamFunction as MSF
import           Debug.Trace
import           FRP.BearRiver
import           Graphics.UI.SDL            as SDL
import           Graphics.UI.SDL.Primitives as SDL

type Stream m b = SF m () b
type Signal a   = Stream IO a

ballInCirclesAroundMouse :: Signal (Int, Int)
ballInCirclesAroundMouse =
  addPair <$> mousePos <*> ballInCircles

-- Mouse position
mousePos :: Signal (Int, Int)
mousePos = arrM (\() -> lift getMousePos)

-- Ball going around in circles
ballInCircles :: Signal (Int, Int)
ballInCircles =
  (\(x,y) -> (round x, round y)) <$> ballInCirclesD

ballInCirclesD :: Signal (Double, Double)
ballInCirclesD =
  (\x -> (rad * cos x, rad * sin x)) <$> (/2) <$> time
  where rad = 45 -- radius in pixels

-- Auxiliary
addPair :: Num a => (a,a) -> (a,a) -> (a,a)
addPair (x1,x2) (y1,y2) = (x1+y1, x2+y2)

-- Input
getMousePos :: IO (Int, Int)
getMousePos = do
  pumpEvents
  (x,y,btns) <- SDL.getMouseState
  return (fromIntegral x, fromIntegral y)

main = do
   SDL.init [InitEverything]
   SDL.setVideoMode 800 600 32 [SWSurface]
   reactimate' ballInCirclesAroundMouse

-- Output
render (px,py) = do
  screen <- SDL.getVideoSurface

  white <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xFF 0xFF 0xFF
  SDL.fillRect screen Nothing white

  SDL.filledCircle
    screen
    (fromIntegral px)
    (fromIntegral py)
    30
    (Pixel 0xFF0000FF)

  SDL.flip screen

  threadDelay 1000

reactimate' :: Signal (Int, Int) -> IO ()
reactimate' sf =
  MSF.reactimate $ sense >>> sfIO >>> actuate
 where sfIO    = runReaderS sf
       sense   = arr (const (0.2, ()))
       actuate = arrM render
