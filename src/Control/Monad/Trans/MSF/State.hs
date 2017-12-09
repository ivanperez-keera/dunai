{-# LANGUAGE Rank2Types #-}
-- | MSFs with a State monadic layer.
--
-- This module contains functions to work with MSFs that include a 'State'
-- monadic layer. This includes functions to create new MSFs that include an
-- additional layer, and functions to flatten that layer out of the MSF's
-- transformer stack.
module Control.Monad.Trans.MSF.State
  ( module Control.Monad.Trans.State.Strict
  -- * State MSF running/wrapping/unwrapping
  , stateS
  , runStateS
  , runStateS_
  , runStateS__
  ) where

-- External
import Control.Applicative
import Control.Monad.Trans.State.Strict
  hiding (liftCallCC, liftCatch, liftListen, liftPass) -- Avoid conflicting exports
import Data.Tuple (swap)

-- Internal
import Data.MonadicStreamFunction

-- * State MSF running/wrapping/unwrapping

-- | Build an MSF in the 'State' monad from one that takes the state as an
-- extra input. This is the opposite of 'runStateS'.
stateS :: (Functor m, Monad m) => MSF m (s, a) (s, b) -> MSF (StateT s m) a b
stateS = hoistGen $ \f a -> StateT $ \s -> (\((s, b), c) -> ((b, c), s))
     <$> f (s, a)

-- | Build an MSF that takes a state as an extra input from one on the
-- 'State' monad. This is the opposite of 'stateS'.
runStateS :: (Functor m, Monad m) => MSF (StateT s m) a b -> MSF m (s, a) (s, b)
runStateS = hoistGen $ \f (s, a) -> (\((b, c), s) -> ((s, b), c))
        <$> runStateT (f a) s

-- | Build an MSF /function/ that takes a fixed state as additional input, from
-- an MSF in the 'State' monad, and outputs the new state with every
-- transformation step.
runStateS_ :: (Functor m, Monad m) => MSF (StateT s m) a b -> s -> MSF m a (s, b)
runStateS_ msf s = feedback s
                 $ arr swap >>> runStateS msf >>> arr (\(s,b) -> ((s,b), s))

-- TODO Rename this to execStateS!
-- | Build an MSF /function/ that takes a fixed state as additional
-- input, from an MSF in the 'State' monad.
runStateS__ :: (Functor m, Monad m) => MSF (StateT s m) a b -> s -> MSF m a b
runStateS__ msf s = runStateS_ msf s >>> arr snd
