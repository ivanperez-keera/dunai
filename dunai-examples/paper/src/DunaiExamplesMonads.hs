-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- An example of adding a game environment in a monad to implement passing
-- re-configurable settings to a game implemented using MSFs in a monad.
module DunaiExamplesMonads where

import Control.Monad (when, MonadPlus)
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.Instances.ArrowPlus
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

ballToLeft2      :: Monad m => MSF (GameEnv2 m) () Ball
ballToLeft2      =
  count >>> arrM addRightPlayerPos >>> arrM checkHitR

  where  checkHitR    :: Monad m => Int -> GameEnv2 m Int
         checkHitR n  = do
           rp <- lift (asks rightPlayerPos)
           when (rp > n) $ tell [ "Ball at " ++ show n ]
           return n

         addRightPlayerPos = (\n -> (n +) <$> lift (asks leftPlayerPos))


hitLeft2    ::  Monad m => MSF (GameEnv2 m) Ball Bool
hitLeft2    =   arrM (\i -> (i >=) <$> lift (asks rightPlayerPos))


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

ballBounceOnce  :: (Monad m, MonadPlus m) =>  MSF (GameEnv2 m) () Ball
ballBounceOnce  =  ballUntilRight `catchMaybe` ballLeft2

ballLeft2   ::  (Monad m, MonadPlus m) => MSF (GameEnv2 m) () Ball
ballLeft2   =   singleBallLeft <+> singleBallLeft
  where
    singleBallLeft =
      count >>>
        arrM (\n -> (\p -> p - n) <$> lift (asks rightPlayerPos))

ballUntilRight  ::  Monad m => MSF (MaybeT (GameEnv2 m)) () Ball
ballUntilRight  =   liftTransS (ballToRight2
                    >>> (arr id &&& hitRight2))
                    >>> arrM filterHit
  where
    filterHit (b, c) = MaybeT $ return $
      if c then Nothing else Just b

ballUntilLeft  ::  Monad m => MSF (MaybeT (GameEnv2 m)) () Ball
ballUntilLeft  =   liftTransS (ballToLeft2
                    >>> (arr id &&& hitLeft2))
                    >>> arrM filterHit
  where
    filterHit (b, c) = MaybeT $ return $
      if c then Nothing else Just b


game2 ::  Monad m => MSF (GameEnv2 m) () Ball
game2 =   ballUntilRight `catchMaybe` (ballUntilLeft `catchMaybe` game2)

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
  `catchMaybe`  ballToLeft3   `untilMaybe` hitLeft3
  `catchMaybe`  (lift (lift incOneRound)  `andThen` game3)

ballToLeft3  ::   Monad m => MSF (GameEnv3 m) () Ball
ballToLeft3  =
  count >>> arrM (\n -> (n +) <$> asks rightPlayerPos)

hitLeft3 :: Monad m => MSF (GameEnv3 m) Ball Bool
hitLeft3 =  arrM (\i -> (i >=) <$> asks leftPlayerPos)

incOneRound  :: Monad m => StateT Integer m ()
incOneRound  = modify (+1)

mainMSF :: MSF IO () ()
mainMSF = (runStateS_ (widthFirst parallelGame) 0) >>> arrM print
 where
   parallelGame :: Monad m => MSF (ListT (StateT Integer m)) () (Ball, Ball)
   parallelGame  =    runReaderS_ game3 (GameSettings 20 17)
                 &&&  runReaderS_ game3 (GameSettings 10  4)

andThen :: Monad m
        => m ()
        -> MSF m a b
        -> MSF m a b
andThen a b = performOnFirstSample (a >> return b)
