{-# LANGUAGE Arrows #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- The 'Maybe' monad is very versatile. It can stand for default arguments,
-- for absent values, and for (nondescript) exceptions. The latter viewpoint
-- is most natural in the context of 'MSF's.
module Control.Monad.Trans.MSF.Maybe
    ( module Control.Monad.Trans.MSF.Maybe
    , module Control.Monad.Trans.Maybe
    , maybeToExceptS
    )
  where

-- External imports
import Control.Arrow             (returnA, (>>>), arr)
import Control.Monad.Trans.Maybe hiding (liftCallCC, liftCatch, liftListen,
                                  liftPass)

-- Internal imports
import Control.Monad.Trans.MSF.Except (ExceptT, exceptS, listToMSFExcept,
                                       maybeToExceptS, reactimateExcept,
                                       runExceptT, runMSFExcept, safe, safely,
                                       try)
import Data.MonadicStreamFunction     (MSF, arrM, constM, liftTransS, morphS)

-- * Throwing 'Nothing' as an exception ("exiting")

-- | Throw the exception immediately.
exit :: Monad m => MSF (MaybeT m) a b
exit = constM $ MaybeT $ return Nothing

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

-- | Run the first @msf@ until the second one produces 'True' from the output
-- of the first.
untilMaybe :: Monad m => MSF m a b -> MSF m b Bool -> MSF (MaybeT m) a b
untilMaybe msf cond = proc a -> do
  b <- liftTransS msf  -< a
  c <- liftTransS cond -< b
  inMaybeT -< if c then Nothing else Just b

-- | When an exception occurs in the first 'msf', the second 'msf' is executed
-- from there.
catchMaybe :: (Functor m, Monad m)
           => MSF (MaybeT m) a b
           -> MSF m a b
           -> MSF m a b
catchMaybe msf1 msf2 = safely $ do
  _ <- try $ maybeToExceptS msf1
  safe msf2

-- * Converting to and from 'MaybeT'

-- | Convert exceptions into `Nothing`, discarding the exception value.
exceptToMaybeS :: (Functor m, Monad m)
               => MSF (ExceptT e m) a b
               -> MSF (MaybeT m) a b
exceptToMaybeS =
  morphS $ MaybeT . fmap (either (const Nothing) Just) . runExceptT

-- | Converts a list to an 'MSF' in 'MaybeT', which outputs an element of the
-- list at each step, throwing 'Nothing' when the list ends.
listToMaybeS :: (Functor m, Monad m) => [b] -> MSF (MaybeT m) a b
listToMaybeS = exceptToMaybeS . runMSFExcept . listToMSFExcept

-- * Running 'MaybeT'

-- | Remove the 'MaybeT' layer by outputting 'Nothing' when the exception
-- occurs. The continuation in which the exception occurred is then tested on
-- the next input.
runMaybeS :: (Functor m, Monad m) => MSF (MaybeT m) a b -> MSF m a (Maybe b)
runMaybeS msf = exceptS (maybeToExceptS msf) >>> arr eitherToMaybe
  where
    eitherToMaybe (Left ()) = Nothing
    eitherToMaybe (Right b) = Just b

-- | Reactimates an 'MSF' in the 'MaybeT' monad until it throws 'Nothing'.
reactimateMaybe :: (Functor m, Monad m)
                => MSF (MaybeT m) () ()
                -> m ()
reactimateMaybe msf = reactimateExcept $ try $ maybeToExceptS msf

-- | Run an 'MSF' fed from a list, discarding results. Useful when one needs to
-- combine effects and streams (i.e., for testing purposes).
embed_ :: (Functor m, Monad m) => MSF m a () -> [a] -> m ()
embed_ msf as = reactimateMaybe $ listToMaybeS as >>> liftTransS msf
