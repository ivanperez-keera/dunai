{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleInstances #-}

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
type Signal m a = Stream m a

ballInCirclesAroundMouse :: GameMonad m => Signal m (Int, Int)
ballInCirclesAroundMouse =
  addPair <$> mousePos <*> ballInCircles

-- Mouse position
mousePos :: GameMonad m => Signal m (Int, Int)
mousePos = arrM (\() -> lift getMousePos)

-- Ball going around in circles
ballInCircles :: (Functor m, Monad m) => Signal m (Int, Int)
ballInCircles =
  (\(x,y) -> (round x, round y)) <$> ballInCirclesD

ballInCirclesD :: (Functor m, Monad m) => Signal m (Double, Double)
ballInCirclesD =
  (\x -> (rad * cos x, rad * sin x)) <$> (/2) <$> time
  where rad = 45 -- radius in pixels

-- Auxiliary
addPair :: Num a => (a,a) -> (a,a) -> (a,a)
addPair (x1,x2) (y1,y2) = (x1+y1, x2+y2)

main = do
   SDL.init [InitEverything]
   SDL.setVideoMode 800 600 32 [SWSurface]
   reactimate' ballInCirclesAroundMouse

-- Output
render (px,py) = do
  print (px, py)
  screen <- SDL.getVideoSurface

  white <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0xFF 0xFF 0xFF
  SDL.fillRect screen Nothing white

  SDL.filledCircle screen (fromIntegral px) (fromIntegral py) 30 (Pixel 0xFF0000FF)

  SDL.flip screen

  threadDelay 1000

reactimate' :: Signal Identity (Int, Int) -> IO ()
reactimate' sf =
  MSF.reactimate $ sense >>> morphS (return.runIdentity) sfIO >>> actuate
 where sfIO    = runReaderS sf
       sense   = arr (const (0.2, ()))
       actuate = arrM render

runOnce :: (Monad m, Applicative m) => MSF m a b -> a -> m b
runOnce msf a = head <$> embed msf [a]

runGame :: IO ()
runGame = do
  SDL.init [InitEverything]
  SDL.setVideoMode 800 600 32 [SWSurface]
  MSF.reactimate $ sense >>> sfIO >>> actuate
 where sfIO    = runReaderS ballInCirclesAroundMouse
       sense   = arr (const (0.2, ()))
       actuate = arrM render

runDebug :: StateT [PureGameEnv] IO ()
runDebug =
  MSF.reactimate $ sense >>> sfIO >>> actuate
 where sfIO    = runReaderS ballInCirclesAroundMouse
       sense   = arr (const (0.2, ()))
       actuate = arrM (lift . print)

class (Functor m, Applicative m, Monad m) => GameMonad m where
  getMousePos :: m (Int, Int)

instance GameMonad IO where
  getMousePos = do
    pumpEvents
    (x,y,btns) <- SDL.getMouseState
    putStrLn $ "Mouse position: " ++ show (x,y)
    return (fromIntegral x, fromIntegral y)

instance GameMonad Identity where
  getMousePos = return (400, 300)

instance (Functor m, Applicative m, Monad m) => GameMonad (ReaderT PureGameEnv m) where
  getMousePos = pureMousePos <$> ask

data PureGameEnv = PureGameEnv { pureMousePos :: (Int, Int)}

instance (Functor m, Monad m) => GameMonad (StateT [PureGameEnv] m) where
  getMousePos = StateT $ \ls -> case ls of
    []     -> error "End of saved input trace"
    (p:ps) -> return (pureMousePos p, ps)

testPureEnv :: [ PureGameEnv ]
testPureEnv = map PureGameEnv
  [ (418,331)
  , (496,327)
  , (613,253)
  , (616,214)
  , (500,79)
  , (463,59)
  , (454,54)
  , (437,51)
  , (350,63)
  , (281,82)
  ]

sampleTrace =
 [ ((347,128),(266,140))
 , ((310,149),(252,170))
 , ((295,183),(252,219))
 , ((293,237),(252,285))
 , ((291,307),(252,328))
 , ((289,353),(297,369))
 , ((331,398),(584,372))
 , ((615,404),(737,350))
 , ((765,385),(737,350))
 , ((761,388),(737,350))
 ]

-- Mouse position: (418,331)
-- (463,335)
-- Mouse position: (496,327)
-- (540,336)
-- Mouse position: (613,253)
-- (656,266)
-- Mouse position: (616,214)
-- (657,232)
-- Mouse position: (500,79)
-- (539,101)
-- Mouse position: (463,59)
-- (500,84)
-- Mouse position: (454,54)
-- (488,83)
-- Mouse position: (437,51)
-- (468,83)
-- Mouse position: (350,63)
-- (378,98)
-- Mouse position: (281,82)
-- (305,120)
--
-- (463,335)
-- (540,336)
-- (656,266)
-- (657,232)
-- (539,101)
-- (500,84)
-- (488,83)
-- (468,83)
-- (378,98)
-- (305,120)
