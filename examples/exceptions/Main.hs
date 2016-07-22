{-# LANGUAGE Arrows #-}
import Control.Monad.Trans.MStreamF
import Data.MonadicStreamFunction
import qualified Data.MonadicStreamFunction as MSF

import Data.Monoid
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Time.Clock
import System.Random

{-
unpack :: Monoid a => Event a -> a
unpack (Event a) = a
unpack NoEvent   = mempty

type Vel a = (a, a)
type Pos a = (a, a)

pointmass :: (Monad m, Num a) => Vel a -> Pos a -> MStreamF m () (Pos a, Vel a)
pointmass pos0 vel0 = proc impulse -> do
  pos <- integralFrom pos0 -< vel0
  returnA -< (pos, vel0)

data Wall = North | East | South | West


collision :: (Monad m, Num a) => MStreamF (ExceptT (Wall, Pos a) m) (Pos a, Vel a) (Pos a)
collision = liftMStreamF_ collide
  where
    collide ((x, y), (vx, vy)) =
      | x >  1 && vx < 0 = throwE (East
      | x < -1 && vx > 0 = throwE West
      | y >  1 && vy < 0 = throwE North
      | y <  1 && vy > 0 = throwE South
      | otherwise        = return (x, y)

system :: (Monad m, Num a) => Pos a -> Vel a -> MStreamF m () (Pos a)
system pos0 vel0@(vx, vy) = safely $ do
  wall <- try $ pointmass vel0 pos0 >>> collision
  case wall of
  East  ->
  -}

main = do
  putStrLn "Beat the clock!"
  startTime <- getCurrentTime
  Left t <- runExceptT $ reactimate $ runMSFExcept $ game startTime
  putStrLn $ "You played for " ++ show t ++ "seconds!"

data Timing = Close | TimeUp Double

game :: UTCTime -> MSFExcept IO () () Double
game startTime = do
  e <- try $ timeIsTicking startTime
  case e of
    Close -> do
      once_ $ putStrLn "Hurry up!"
      game startTime
    TimeUp t -> do
      once_ $ putStrLn "Time is up!"
      return t

gameLoop :: UTCTime -> MStreamF IO () Double
gameLoop startTime = proc _ -> do
  a <- randomNumber -< ()
  b <- randomNumber -< ()
  _ <- liftMStreamF putStrLn -< show a ++ " times " ++ show b ++ " = ?"
  r <- liftMStreamF_ readLn -< ()
  let result = a * b
  let correct = r == result
  bonusTime <- mySumFrom 60 -< if correct then 10 else 0
  _ <- liftMStreamF putStrLn -< if correct then "Well done! You earn 10 bonus seconds!" else "Wrong, it's " ++ show result
  returnA -< bonusTime

mySumFrom :: (Num a, Monad m) => a -> MStreamF m a a
mySumFrom a = arr Sum >>> sumFrom (Sum a) >>> arr getSum

timeIsTicking :: UTCTime -> MStreamF (ExceptT Timing IO) () ()
timeIsTicking startTime = proc _ -> do
  bonusTime <- liftMStreamFTrans (gameLoop startTime) -< ()
  time <- liftMStreamF_ (lift getCurrentTime) -< ()
  let t = realToFrac $ time `diffUTCTime` startTime
  if t > bonusTime
    then throwS -< TimeUp t
    else if t + 10 > bonusTime
      then throwS -< Close
      else liftMStreamF (lift . putStrLn) -< "Time left: " ++ show (bonusTime - t) ++ " seconds"
-- TODO How to use guards in do notation?


randomNumber :: MStreamF IO () Int
randomNumber = liftMStreamF_ $ randomRIO (3, 15)

-- TODO: Put startTime in a ReaderT