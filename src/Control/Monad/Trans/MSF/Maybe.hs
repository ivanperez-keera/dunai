{-# LANGUAGE Arrows              #-}
{-# LANGUAGE Rank2Types          #-}
{- |
The 'Maybe' monad is very versatile. It can stand for default arguments,
for absent values, and for (nondescript) exceptions.
The latter viewpoint is most natural in the context of 'MSF's.
-}
module Control.Monad.Trans.MSF.Maybe
  ( module Control.Monad.Trans.MSF.Maybe
  , module Control.Monad.Trans.Maybe
  , maybeToExceptS
  ) where

-- External
import Control.Monad.Trans.Maybe
  hiding (liftCallCC, liftCatch, liftListen, liftPass) -- Avoid conflicting exports

-- Internal
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction

-- * Throwing 'Nothing' as an exception ("exiting")

-- | Throw the exception immediately.
exit :: Monad m => MSF (MaybeT m) a b
exit = arrM_ $ MaybeT $ return Nothing

-- | Throw the exception when the condition becomes true on the input.
exitWhen :: Monad m => (a -> Bool) -> MSF (MaybeT m) a a
exitWhen condition = proc a -> do
  _ <- exitIf -< condition a
  returnA     -< a

-- | Exit when the incoming value is 'True'.
exitIf :: Monad m => MSF (MaybeT m) Bool ()
exitIf = proc condition -> if condition
  then exit    -< ()
  else returnA -< ()

-- | @Just a@ is passed along, 'Nothing' causes the whole 'MSF' to exit.
maybeExit :: Monad m => MSF (MaybeT m) (Maybe a) a
maybeExit = inMaybeT

-- | Embed a 'Maybe' value in the 'MaybeT' layer. Identical to 'maybeExit'.
inMaybeT :: Monad m => MSF (MaybeT m) (Maybe a) a
inMaybeT = arrM $ MaybeT . return


-- * Catching Maybe exceptions

-- | Run the first @msf@ until the second one produces 'True' from the output of the first.
untilMaybe :: Monad m => MSF m a b -> MSF m b Bool -> MSF (MaybeT m) a b
untilMaybe msf cond = proc a -> do
  b <- liftMSFTrans msf  -< a
  c <- liftMSFTrans cond -< b
  inMaybeT -< if c then Nothing else Just b

-- | When an exception occurs in the first 'msf', the second 'msf' is executed from there.
catchMaybe
  :: (Functor m, Monad m)
  => MSF (MaybeT m) a b -> MSF m a b -> MSF m a b
catchMaybe msf1 msf2 = safely $ do
  _ <- try $ maybeToExceptS msf1
  safe msf2

-- * Converting to and from 'MaybeT'

-- | Converts a list to an 'MSF' in 'MaybeT',
--   which outputs an element of the list at each step,
--   throwing 'Nothing' when the list ends.
listToMaybeS :: Monad m => [b] -> MSF (MaybeT m) a b
listToMaybeS = foldr iPost exit

-- * Running 'MaybeT'
-- | Remove the 'MaybeT' layer by outputting 'Nothing' when the exception occurs.
--   The continuation in which the exception occurred is then tested on the next input.
runMaybeS :: Monad m => MSF (MaybeT m) a b -> MSF m a (Maybe b)
runMaybeS msf = go
  where
    go = MSF $ \a -> do
           bmsf <- runMaybeT $ unMSF msf a
           case bmsf of
             Just (b, msf') -> return (Just b, runMaybeS msf')
             Nothing        -> return (Nothing, go)

-- | Different implementation, to study performance.
runMaybeS'' :: Monad m => MSF (MaybeT m) a b -> MSF m a (Maybe b)
runMaybeS'' = transG transformInput transformOutput
  where
    transformInput       = return
    transformOutput _ m1 = do r <- runMaybeT m1
                              case r of
                                Nothing     -> return (Nothing, Nothing)
                                Just (b, c) -> return (Just b,  Just c)

-- mapMaybeS msf == runMaybeS (inMaybeT >>> lift mapMaybeS)

{-
runMaybeS'' :: Monad m => MSF (MaybeT m) a b -> MSF m a (Maybe b)
runMaybeS'' msf = transS transformInput transformOutput msf
  where
    transformInput  = return
    transformOutput _ msfaction = do
      thing <- runMaybeT msfaction
      case thing of
        Just (b, msf') -> return (Just b, msf')
        Nothing        -> return (Nothing, msf)
-}

-- | Reactimates an 'MSF' in the 'MaybeT' monad until it throws 'Nothing'.
reactimateMaybe
  :: (Functor m, Monad m)
  => MSF (MaybeT m) () () -> m ()
reactimateMaybe msf = reactimateExcept $ try $ maybeToExceptS msf

-- | Run an 'MSF' fed from a list, discarding results. Useful when one needs to
-- combine effects and streams (i.e., for testing purposes).
embed_ :: (Functor m, Monad m) => MSF m a () -> [a] -> m ()

embed_ msf as = reactimateMaybe $ listToMaybeS as >>> liftMSFTrans msf
