{-# LANGUAGE Rank2Types          #-}
module Control.Monad.Trans.MSF.State
  ( module Control.Monad.Trans.MSF.State
  , module Control.Monad.Trans.State.Strict
  ) where

-- External
import Control.Applicative
import Control.Monad.Trans.State.Strict
  hiding (liftCallCC, liftCatch, liftListen, liftPass) -- Avoid conflicting exports

-- Internal
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction



-- * Running and wrapping
stateS :: Monad m => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
stateS msf = MSF $ \a -> StateT $ \s -> do
    ((s', b), msf') <- unMSF msf (s, a)
    return ((b, stateS msf'), s')

runStateS :: Monad m => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS msf = MSF $ \(s, a) -> do
    ((b, msf'), s') <- runStateT (unMSF msf a) s
    return ((s', b), runStateS msf')

-- * Auxiliary functions

-- IP: Is runStateS_ msf s = feedback s $ runStateS msf >>> arr (\(s,b) -> ((s,b), s)) ?
runStateS_ :: Monad m => MSF (StateT s m) a b -> s -> MSF m a (s, b)
runStateS_ msf s = MSF $ \a -> do
    ((b, msf'), s') <- runStateT (unMSF msf a) s
    return ((s', b), runStateS_ msf' s')

-- IP: Is runStateS__ msf s = feedback s $ runStateS msf >>> arr (\(s,b) -> (b, s)) ?
runStateS__ :: Monad m => MSF (StateT s m) a b -> s -> MSF m a b
runStateS__ msf s = MSF $ \a -> do
    ((b, msf'), s') <- runStateT (unMSF msf a) s
    return (b, runStateS__ msf' s')


runStateS''' :: (Functor m, Monad m) => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS''' = transG transformInput transformOutput
  where
    transformInput  (_, a)           = return a
    transformOutput (s, _) msfaction = sym <$> runStateT msfaction s
    sym ((b, msf), s)                = ((s, b), Just msf)

-- * Alternative running/wrapping MSF combinators using generic lifting
--
-- IPerez: TODO: Is this exactly the same as stateS?
stateS' :: (Functor m, Monad m) => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
stateS' = lifterS (\g i -> StateT ((resort <$>) . g . flip (,) i))
 where resort ((s, b), ct) = ((b, ct), s)

-- stateS' :: Monad m => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
-- stateS' = lifterS $ \f a -> StateT $ \s -> do
--   ((s', b), msf') <- f (s, a)
--   return ((b, msf'), s')

runStateS' :: (Functor m, Monad m) => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS' = lifterS (\g i -> resort <$> uncurry (flip runStateT) (second g i))
 where resort ((b, msf), s) = ((s, b), msf)


runStateS'' :: (Functor m, Monad m) => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS'' = transS transformInput transformOutput
  where
    transformInput  (_, a)           = return a
    transformOutput (s, _) msfaction = sym <$> runStateT msfaction s
    sym ((b, msf), s)                = ((s, b), msf)

{-
stateS'' :: Monad m => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
stateS'' = transS transformInput transformOutput
  where
    transformInput  (_, a) = return a
    transformOutput (s, _) = do
        put s
-}
