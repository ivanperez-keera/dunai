-- | In this module, 'MSF's in a monad supporting random number generation
--   (i.e. having the 'RandT' layer in its stack) can be run.
--   Running means supplying an initial random number generator,
--   where the update of the generator at every random number generation
--   is already taken care of.
--
--   Under the hood, 'RandT' is basically just 'StateT',
--   with the current random number generator as mutable state.


{-# LANGUAGE Arrows              #-}
module Control.Monad.Trans.MSF.Random
  (
    runRandS
  , evalRandS

  , getRandomS
  , getRandomsS
  , getRandomRS
  , getRandomRS_
  , getRandomsRS
  , getRandomsRS_
  ) where

-- External
import Control.Monad.Random

-- Internal
import Data.MonadicStreamFunction
import Control.Monad.Trans.MSF.State

-- | Run an 'MSF' in the 'RandT' random number monad transformer
--   by supplying an initial random generator.
--   Updates the generator every step.
runRandS :: (RandomGen g, Functor m, Monad m)
         => MSF (RandT g m) a b
         -> g -- ^ The initial random number generator.
         -> MSF m a (g, b)
runRandS = runStateS_ . morphS (StateT . runRandT)

-- | Evaluate an 'MSF' in the 'RandT' transformer,
--   i.e. extract possibly random values
--   by supplying an initial random generator.
--   Updates the generator every step but discharges the generator.
evalRandS :: (RandomGen g, Functor m, Monad m)
          => MSF (RandT g m) a b -> g -> MSF m a b
evalRandS msf g = runRandS msf g >>> arr snd

-- | Create a stream of random values.
getRandomS :: (MonadRandom m, Random b) => MSF m a b
getRandomS = constM getRandom


-- | Create a stream of lists of random values.
getRandomsS :: (MonadRandom m, Random b) => MSF m a [b]
getRandomsS = constM getRandoms

-- | Create a stream of random values in a given fixed range.
getRandomRS :: (MonadRandom m, Random b) => (b, b) -> MSF m a b
getRandomRS range = constM $ getRandomR range

-- | Create a stream of random values in a given range,
--   where the range is specified on every tick.
getRandomRS_ :: (MonadRandom m, Random b) => MSF m (b, b) b
getRandomRS_  = arrM getRandomR

-- | Create a stream of lists of random values in a given fixed range.
getRandomsRS :: (MonadRandom m, Random b) => (b, b) -> MSF m a [b]
getRandomsRS range = constM $ getRandomRs range

-- | Create a stream of lists of random values in a given range,
--   where the range is specified on every tick.
getRandomsRS_ :: (MonadRandom m, Random b) => MSF m (b, b) [b]
getRandomsRS_ = arrM getRandomRs
