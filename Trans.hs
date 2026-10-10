{-# LANGUAGE Arrows #-}
import Control.Monad
import Control.Monad.Trans.MSF.Reader
import Data.Functor.Identity
import Data.IORef
import Data.MonadicStreamFunction hiding (trace)
import Data.MonadicStreamFunction.InternalCore
import FRP.BearRiver
import GHC.Float
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import Debug.Trace (trace)

type LimitedAnimation =
       SF Identity C (Picture, Event ())

forSeconds n sf = proc c -> do
  r  <- sf         -< c
  let e0 = if Main.next c then Event () else NoEvent
  e1 <- after n () -< ()
  let e' = lMerge e0 e1
  returnA -< (r, e')

type SlideDeck = ([LimitedAnimation], LimitedAnimation, [LimitedAnimation])

data Dir = L | R

nextSlide :: SlideDeck -> 

animate (xs, x, ys) =
  switch (x >>^ (\(p, e) -> 

animate :: [LimitedAnimation] -> SF Identity C Picture
animate []     = constant blank
animate (x:xs) = switch (x >>^ (\(p, e) -> (p, e `tag` p))) $ \p -> 
  let u    :: ReaderT DTime Identity [Picture]
      u    =  embed (animate xs)  [neutralC]

      y    :: Identity [Picture]
      y    =  runReaderT u 0

      z    :: [Picture]
      z    =  runIdentity y

      [p2] = z
  in switch (slideRight 3 p p2) $ \_ -> animate xs

slide1 = constant $ translate (-250) (-50) $ Text "Welcome"
slide2 = constant $ translate (-250) (-50) $ Text "Welcome back"
slide3 = proc _ -> do
           t <- localTime -< ()
           returnA -< Circle (50 / double2Float t :: Float)

slides = animate g
  where
    g = [ forSeconds 5  slide1
        , forSeconds 10 slide2
        , forSeconds 5  slide3
        ] ++ g

-- * Transitions

type Transition m = Picture -> Picture -> SF m C (Picture, Event ())

slideLeft :: Double -> Transition Identity
slideLeft maxT p1 p2 = proc _ -> do
  t <- time -< ()
  let e = if t >= maxT then Event () else NoEvent
      p1' = translate (double2Float $ (-1) * t * (width / maxT))  0 p1
      p2' = translate (double2Float $ width - t * (width / maxT)) 0 p2
  returnA -< (Pictures [p1', p2'], e)

slideRight :: Double -> Transition Identity
slideRight maxT p1 p2 = proc _ -> do
  t <- time -< ()
  let e = if t >= maxT then Event () else NoEvent
      p1' = translate (double2Float $ t * (width / maxT))            0 p1
      p2' = translate (double2Float $ (-width) + t * (width / maxT)) 0 p2
  returnA -< (Pictures [p1', p2'], e)

-- * Screen settings

width  :: Num a => a
width  = 1280

height :: Num a => a
height = 720

main = do

  msfRef <- newIORef slides

  playIO 
    (InWindow "Nice Window" (width, height) (width `div` 2, height `div` 2))
    white
    60
    neutralC
    (\c -> do msf' <- readIORef msfRef
              let (b, msf'') = runReader ((unMSF msf') c) (0.016 :: Double)
              writeIORef msfRef msf''
              return b
    )
    (\e c -> case e of
               EventKey (Char 's') Down _ _ -> return (c { Main.next = True  })
               EventKey (Char 's') Up   _ _ -> return (c { Main.next = False })
               _                            -> return c
    )
    (\t c -> return c)

data C = C { next :: Bool }

neutralC = C False
