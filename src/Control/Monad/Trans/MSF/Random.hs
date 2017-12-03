{-# LANGUAGE Arrows              #-}
module Control.Monad.Trans.MSF.Random 
  (
    runRandS
  , evalRandS

  , getRandomS
  , getRandomsS
  , getRandomRS
  , getRandomsRS
  ) where

-- External
import Control.Monad.Random

-- Internal
import Data.MonadicStreamFunction

-- | Updates the generator every step
runRandS :: (RandomGen g, Monad m) 
        => MSF (RandT g m) a b 
        -> g 
        -> MSF m a (g, b)
runRandS msf g = MSF $ \a -> do
  ((b, msf'), g') <- runRandT (unMSF msf a) g
  return ((g', b), runRandS msf' g')

-- | Updates the generator every step but discharges the generator 
evalRandS  :: (RandomGen g, Monad m) => MSF (RandT g m) a b -> g -> MSF m a b
evalRandS msf g = runRandS msf g >>> arr snd

getRandomS :: (MonadRandom m, Random b) => MSF m a b
getRandomS = proc _ -> do
  r <- arrM_ $ getRandom -< ()
  returnA -< r

getRandomsS :: (MonadRandom m, Random b) => MSF m a [b]
getRandomsS = proc _ -> do
  r <- arrM_ $ getRandoms -< ()
  returnA -< r

getRandomRS :: (MonadRandom m, Random b) => (b, b) -> MSF m a b
getRandomRS range = proc _ -> do
  r <- arrM_ $ getRandomR range -< ()
  returnA -< r 

getRandomsRS :: (MonadRandom m, Random b) => (b, b) -> MSF m a [b]
getRandomsRS range = proc _ -> do
  r <- arrM_ $ getRandomRs range -< ()
  returnA -< r 