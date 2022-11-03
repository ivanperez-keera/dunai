-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- This module contains operations on monadic streams that are asynchronous,
-- i.e. that change the speed at which data enters or leaves the 'MSF'.
module Data.MonadicStreamFunction.Async where

-- base
import Control.Monad (ap)
import Data.Either (fromRight)

-- Internal imports
import Control.Monad.Trans.MSF.Maybe
import Control.Monad.Trans.MSF.Except (step, once, MSFExcept (MSFExcept, runMSFExcept), listToMSFExcept, runExceptT)
import Data.MonadicStreamFunction (morphS, liftTransS, MSF)
import Data.MonadicStreamFunction.InternalCore (MSF(unMSF))
import Data.MonadicStreamFunction.Util         (MStream)

-- | Execute an 'MSF' for an unknown number of steps.
newtype MSFAsync i m a = MSFAsync { unMSFAsync :: MSFExcept m i a () }

instance Monad m => Functor (MSFAsync i m) where
  fmap f = MSFAsync . MSFExcept . fmap f . runMSFExcept . unMSFAsync

instance Monad m => Applicative (MSFAsync i m) where
  pure a = MSFAsync $ step $ const $ return (a, ())
  (<*>) = ap

instance Monad m => Monad (MSFAsync i m) where
  MSFAsync msf0 >>= f = MSFAsync $ go $ runMSFExcept msf0
    where
      go msf = do
        output <- once $ runExceptT <$> unMSF msf
        case output of
          Right (a, msf') -> do
            unMSFAsync $ f a
            go msf'
          Left () -> return ()
-- |
-- Concatenates a monadic stream of lists to a monadic stream.
-- The stream of lists will be called exactly when new data is needed.
--
-- Example:
--
-- >>> let intstream = constS $ putStrLn "Enter a list of Ints:" >> readLn :: MStream IO [Int]
-- >>> reactimate $ concatS intstream >>> arrM print
-- Enter a list of Ints:
-- [1, 2, 33]
-- 1
-- 2
-- 33
-- Enter a list of Ints:
-- []
-- Enter a list of Ints:
-- []
-- Enter a list of Ints:
-- [1, 2]
-- 1
-- 2
-- Enter a list of Ints:
-- ...
--
-- Beware that @concatS msf@ becomes unproductive when @msf@ starts outputting
-- empty lists forever. This is ok:
--
-- >>> let boolToList b = if b then ["Yes"] else []
-- >>> let everyOddEmpty = count >>> arr (even >>> boolToList)
-- >>> reactimate $ concatS everyOddEmpty >>> arrM print
-- "Yes"
-- "Yes"
-- "Yes"
-- "Yes"
-- "Yes"
-- ...
--
-- But this will be caught in a loop:
--
-- >>> let after3Empty = count >>> arr ((<= 3) >>> boolToList)
-- >>> reactimate $ concatS after3Empty  >>> arrM print
-- "Yes"
-- "Yes"
-- "Yes"
-- ^CInterrupted.
concatS :: Monad m => MStream m [b] -> MStream m b
concatS msf = morphS (fmap (fromRight (error "concatS: internal error")) . runExceptT) $ runMSFExcept $ unMSFAsync $ liftAsync (liftTransS msf) >>= listToMSFAsync

listToMSFAsync :: Monad m => [a] -> MSFAsync i m a
listToMSFAsync = MSFAsync . listToMSFExcept

liftAsync :: Monad m => MSF (MaybeT m) a b -> MSFAsync a m b
liftAsync = MSFAsync . MSFExcept . maybeToExceptS
