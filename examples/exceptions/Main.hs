{-# LANGUAGE Arrows #-}
-- Internal
import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction
import qualified Data.MonadicStreamFunction as MSF
import Data.VectorSpace.Specific

-- External
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Monoid
import Data.Time.Clock
import System.Random
import Text.Read (readMaybe)

main = do
  putStrLn "Beat the clock!"
  startTime <- getCurrentTime
  t <- reactimateExcept $ game startTime
  putStrLn $ "You played for " ++ show (round t) ++ " seconds!"

data Timing = Close | TimeUp Double

game :: UTCTime -> MSFExcept IO () () Double
game startTime = do
  e <- try $ timeIsTicking startTime
  case e of
    -- TODO Bug: Resets time when Close is thrown.
    -- Either Close has to contain the remaining time or completely different solution.
    Close -> do
      once_ $ putStrLn "Hurry up!"
      game startTime
    TimeUp t -> do
      once_ $ putStrLn "Time is up!"
      return t

gameLoop :: MSF IO () Double
gameLoop = proc _ -> do
  a <- randomNumber  -< ()
  b <- randomNumber  -< ()
  arrM putStrLn      -< show a ++ " times " ++ show b ++ " = ?"
  r <- arrM_ getLine -< ()
  let result  = a * b
      correct = readMaybe r == Just result
      msg     = if correct
        then "Well done! You earn 5 bonus seconds!"
        else "Wrong, it's " ++ show result ++ "! A penalty of 5 seconds!"
  bonusTime <- sumFrom 60 -< if correct then 5 else -5
  arrM putStrLn             -< msg
  returnA                   -< bonusTime


timeIsTicking :: UTCTime -> MSF (ExceptT Timing IO) () ()
timeIsTicking startTime = proc _ -> do
  bonusTime <- liftMSFTrans gameLoop       -< ()
  time      <- arrM_ (lift getCurrentTime)  -< ()
  let t = realToFrac $ time `diffUTCTime` startTime
  throwOn'                           -< (t      > bonusTime, TimeUp t)
  throwOn'                           -< (t + 10 > bonusTime, Close)
  let msg = "Time left: " ++ show (round $ bonusTime - t) ++ " seconds"
  liftS putStrLn                     -< msg


randomNumber :: MSF IO () Int
randomNumber = arrM_ $ randomRIO (3, 15)
