{-# LANGUAGE Arrows #-}

import Control.Monad
import Data.MonadicStreamFunction
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Reader

type DTime = Double

type SF m a b = MSF (ReaderT DTime m) a b

localTime :: Monad m => SF m a DTime
localTime = MSF $ \a -> do
  dt <- ask
  return (dt, localTime >>> arr (+dt))


type AssertionId     = String
type DebuggingMonadT = WriterT [ (String, DTime) ]

assert :: Monad m => String -> SF (DebuggingMonadT m) Bool ()
assert assertionId = proc (val) -> do
    t <- localTime                    -< ()
    _ <- withSideEffect optionallyLog -< (t, val)
    returnA -< ()
  where
    optionallyLog (t1, v1) = when v1 (lift $ tell [(assertionId, t1)])

ballAboveFloorM :: Monad m => SF (DebuggingMonadT m) () Double
ballAboveFloorM = proc () -> do
  ballPos <- bouncingBall -< ()
  () <- assert "Ball must always be above the floor" -<(ballPos >= 0)
  returnA -< ballPos

bouncingBall = undefined
