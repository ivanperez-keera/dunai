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
getRandomS = arrM_ getRandom

getRandomsS :: (MonadRandom m, Random b) => MSF m a [b]
getRandomsS = arrM_ getRandoms 

getRandomRS :: (MonadRandom m, Random b) => (b, b) -> MSF m a b
getRandomRS range = arrM_ $ getRandomR range

getRandomRS_ :: (MonadRandom m, Random b) => MSF m (b, b) b
getRandomRS_  = arrM getRandomR
    
getRandomsRS :: (MonadRandom m, Random b) => (b, b) -> MSF m a [b]
getRandomsRS range = arrM_ $ getRandomRs range 

getRandomsRS_ :: (MonadRandom m, Random b) => MSF m (b, b) [b]
getRandomsRS_ = arrM getRandomRs