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

  where  checkHitR    :: n -> GameEnv2 m Int
         checkHitR n  = do
           rp <- asks rightPlayerPos
           when (rp > n) $ tell [ "Ball at " ++ show n ]

ballBounceOnce  ::  MSF (GameEnv2 m) () Ball
ballBounceOnce  =   ballUntilRight `catchMaybe` ballLeft

ballUntilRight  ::  MSF (MaybeT (GameEnv2 m)) () Ball
ballUntilRight  =   liftST (ballToRight
                    >>> (arr id &&& hitRight))
                    >>> arrM filterHit
  where
    filterHit (b, c) = MaybeT $ return $
      if c then Nothing else Just b

game2 ::  Monad m => MSF m () Ball
game2 =   ballUntilRight `catchMaybe` ballUntilLeft `catchMaybe` game2

type GameEnv3 m = ReaderT GameSettings (ListT m)

ballLeft   ::  Monad m => MSF (GameEnv3 m) () Ball
ballLeft   =   singleBallLeft <+> singleBallLeft
  where
    singleBallLeft =
      count >>>
        arrM (\n -> (\p -> p - n) <$> asks rightPlayerPos)

incOneRound  ::  Monad m => StateT Integer m ()
incOneRound  =   modify (+1)


game3  :: Monad m
       => MSF (GameEnv3 (StateT Integer m)) () Ball
game3  =     ballToRight  `untilMaybe` hitRight
  `catchMaybe`  ballToLeft   `untilMaybe` hitLeft
  `catchMaybe`  (lift incOneRound `andThen` game3)

mainMSF :: MSF IO () ()
mainMSF = runStateS_ parallelGame 0 >>> arrM print
 where
   parallelGame :: Int
   parallelGame  =    runReaderS_ game3 (GameSettings 20 17)
                 &&&  runReaderS_ game3 (GameSettings 10  4)

andThen = undefined
