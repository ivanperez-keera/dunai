module DunaiExamplesMonads where

import Control.Monad (when)
import Data.MonadicStreamFunction
import Control.Monad.Trans.Class
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.Reader
import Control.Monad.Trans.MSF.Writer
import Control.Monad.Trans.MSF.List
import Control.Monad.Trans.MSF.State
import Control.Monad.Trans.MSF.Maybe

type GameEnv1 = ReaderT GameSettings

data GameSettings = GameSettings
  {  leftPlayerPos   :: Int
  ,  rightPlayerPos  :: Int
  }

type Ball = Int

ballToRight  ::   Monad m => MSF (GameEnv1 m) () Ball
ballToRight  =
  count >>> arrM (\n -> (n +) <$> asks leftPlayerPos)

hitRight    ::  Monad m => MSF (GameEnv1 m) Ball Bool
hitRight    =   arrM (\i -> (i >=) <$> asks rightPlayerPos)

type GameEnv2 m =
  WriterT [String] (ReaderT GameSettings m)

ballToRight2      :: Monad m => MSF (GameEnv2 m) () Ball
ballToRight2      =
  count >>> arrM addLeftPlayerPos >>> arrM checkHitR

  where  checkHitR    :: Monad m => Int -> GameEnv2 m Int
         checkHitR n  = do
           rp <- lift (asks rightPlayerPos)
           when (rp > n) $ tell [ "Ball at " ++ show n ]
           return n

         addLeftPlayerPos = (\n -> (n +) <$> lift (asks leftPlayerPos))


hitRight2    ::  Monad m => MSF (GameEnv2 m) Ball Bool
hitRight2    =   arrM (\i -> (i >=) <$> lift (asks rightPlayerPos))

ballBounceOnce  :: Monad m =>  MSF (GameEnv2 m) () Ball
ballBounceOnce  =   ballUntilRight `catchMaybe` ballLeft2
  where
    ballLeft2 = undefined

ballUntilRight  ::  Monad m => MSF (MaybeT (GameEnv2 m)) () Ball
ballUntilRight  =   liftTransS (ballToRight2
                    >>> (arr id &&& hitRight2))
                    >>> arrM filterHit
  where
    filterHit (b, c) = MaybeT $ return $
      if c then Nothing else Just b


game2 ::  Monad m => MSF (GameEnv2 m) () Ball
game2 =   ballUntilRight `catchMaybe` (ballUntilLeft `catchMaybe` game2)
  where
    ballUntilLeft = undefined

type GameEnv3 m = ReaderT GameSettings (ListT m)

ballLeft   ::  Monad m => MSF (GameEnv3 m) () Ball
ballLeft   =   singleBallLeft <+> singleBallLeft
  where
    singleBallLeft =
      count >>>
        arrM (\n -> (\p -> p - n) <$> asks rightPlayerPos)


ballToRight3  ::   Monad m => MSF (GameEnv3 m) () Ball
ballToRight3  =
  count >>> arrM (\n -> (n +) <$> asks leftPlayerPos)

hitRight3 :: Monad m => MSF (GameEnv3 m) Ball Bool
hitRight3 =  arrM (\i -> (i >=) <$> asks rightPlayerPos)

game3  :: Monad m
       => MSF (GameEnv3 (StateT Integer m)) () Ball
game3  =        ballToRight3  `untilMaybe` hitRight3
  `catchMaybe`  ballToLeft    `untilMaybe` hitLeft
  `catchMaybe`  (incOneRoundS  `andThen` game3)

ballToLeft :: Monad m => MSF (GameEnv3 m) () Ball
ballToLeft = undefined

hitLeft    :: Monad m => MSF (GameEnv3 m) Ball Bool
hitLeft    = undefined

incOneRound  :: Monad m => StateT Integer m ()
incOneRound  = modify (+1)

incOneRoundS :: Monad m => MSF (GameEnv3 (StateT Integer m)) a Int
incOneRoundS = constM (lift $ lift incOneRound) >>> undefined

mainMSF :: MSF IO () ()
mainMSF = (runStateS_ (widthFirst parallelGame) 0) >>> arrM print
 where
   parallelGame :: Monad m => MSF (ListT (StateT Integer m)) () (Ball, Ball)
   parallelGame  =    runReaderS_ game3 (GameSettings 20 17)
                 &&&  runReaderS_ game3 (GameSettings 10  4)

andThen = undefined
