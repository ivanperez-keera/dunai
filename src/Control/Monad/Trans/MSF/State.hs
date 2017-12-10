{-# LANGUAGE Rank2Types #-}
-- | 'MSF's with a 'State' monadic layer.
--
-- This module contains functions to work with 'MSF's that include a 'State'
-- monadic layer. This includes functions to create new 'MSF's that include an
-- additional layer, and functions to flatten that layer out of the 'MSF`'s
-- transformer stack.
module Control.Monad.Trans.MSF.State
  ( module Control.Monad.Trans.State.Strict
  -- * 'State' 'MSF' running and wrapping
  , stateS
  , runStateS
  , runStateS_
  , runStateS__
  -- ** Alternative implementation using 'lifterS'
  , stateS'
  , runStateS'
  -- ** Alternative implementation using 'transS'
  , runStateS''
  -- ** Alternative implementation using 'transG'
  , runStateS'''
  ) where

-- External
import Control.Applicative
import Control.Monad.Trans.State.Strict
  hiding (liftCallCC, liftCatch, liftListen, liftPass) -- Avoid conflicting exports

-- Internal
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction

-- * 'State' 'MSF' running and wrapping

-- | Build an 'MSF' in the 'State' monad from one that takes the state as an
-- extra input. This is the opposite of 'runStateS'.
stateS :: Monad m => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
stateS msf = MSF $ \a -> StateT $ \s -> do
    ((s', b), msf') <- unMSF msf (s, a)
    return ((b, stateS msf'), s')

-- | Build an 'MSF' that takes a state as an extra input from one on the
-- 'State' monad. This is the opposite of 'stateS'.
runStateS :: Monad m => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS msf = MSF $ \(s, a) -> do
    ((b, msf'), s') <- runStateT (unMSF msf a) s
    return ((s', b), runStateS msf')

-- | Build an 'MSF' /function/ that takes a fixed state as additional input,
-- from an 'MSF' in the 'State' monad, and outputs the new state with every
-- transformation step.
--
-- This should be always equal to:
--
-- @
-- runStateS_ msf s = feedback s $ runStateS msf >>> arr (\(s,b) -> ((s,b), s))
-- @
--
-- although possibly more efficient.


runStateS_ :: Monad m => MSF (StateT s m) a b -> s -> MSF m a (s, b)
runStateS_ msf s = MSF $ \a -> do
    ((b, msf'), s') <- runStateT (unMSF msf a) s
    return ((s', b), runStateS_ msf' s')

-- | Build an 'MSF' /function/ that takes a fixed state as additional
-- input, from an 'MSF' in the 'State' monad.
--
-- This should be always equal to:
--
-- @
-- runStateS__ msf s = feedback s $ runStateS msf >>> arr (\(s,b) -> (b, s))
-- @
--
-- although possibly more efficient.


runStateS__ :: Monad m => MSF (StateT s m) a b -> s -> MSF m a b
runStateS__ msf s = MSF $ \a -> do
    ((b, msf'), s') <- runStateT (unMSF msf a) s
    return (b, runStateS__ msf' s')

-- * Alternative implementations
--
-- ** Alternative using running/wrapping 'MSF' combinators using generic lifting

-- ** Alternative using 'lifterS'.

-- | Alternative implementation of 'stateS' using 'lifterS'.
stateS' :: (Functor m, Monad m) => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
stateS' = lifterS (\g i -> StateT ((resort <$>) . g . flip (,) i))
 where resort ((s, b), ct) = ((b, ct), s)

-- stateS' :: Monad m => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
-- stateS' = lifterS $ \f a -> StateT $ \s -> do
--   ((s', b), msf') <- f (s, a)
--   return ((b, msf'), s')

-- | Alternative implementation of 'runStateS' using 'lifterS'.
runStateS' :: (Functor m, Monad m) => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS' = lifterS (\g i -> resort <$> uncurry (flip runStateT) (second g i))
 where resort ((b, msf), s) = ((s, b), msf)

-- ** Alternative using 'transS'.

-- | Alternative implementation of 'runStateS' using 'transS'.
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

-- ** Alternative using 'transG'.

-- | Alternative implementation of 'runStateS' using 'transG'.
runStateS''' :: (Functor m, Monad m) => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS''' = transG transformInput transformOutput
  where
    transformInput  (_, a)           = return a
    transformOutput (s, _) msfaction = sym <$> runStateT msfaction s
    sym ((b, msf), s)                = ((s, b), Just msf)
