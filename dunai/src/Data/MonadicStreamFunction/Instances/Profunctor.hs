{-# LANGUAGE CPP                 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -ddump-simpl #-}

-- | Instances of 'Profunctor' and 'Traversing' for Monadic Stream Functions ('MSF').
--
--   Import this module to include the (orphan) instances.
module Data.MonadicStreamFunction.Instances.Profunctor where

-- base
import Control.Arrow

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (traverse)
import Control.Applicative (Applicative)
#endif

-- profunctors
import Data.Profunctor
import Data.Profunctor.Traversing

-- dunai
import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.InternalCore
import Data.MonadicStreamFunction.Instances.ArrowChoice ()

-- transformers
import Control.Monad.Trans.State.Strict

instance Monad m => Choice (MSF m) where
  left' = left

instance Monad m => Strong (MSF m) where
  first' = first

instance Monad m => Profunctor (MSF m) where
  dimap l r p = arr l >>> p >>> arr r

-- | This 'Traversing' instance will step the internal state of the 'MSF' once
-- for every element of the input 'Traversable'.
instance (Functor m, Applicative m, Monad m) => Traversing (MSF m) where
  traverse' msf =
    MSF $ \xs -> do
      -- `StateT (MSF m a b) m a` is isomorphic to `a -> m (b, MSF m a b)`, and
      -- `StateT`'s Applicative instance is needed for writing this traversal
      (fb, msf') <- runStateT (traverse (StateT . flip unMSF) xs) msf
      return (fb, traverse' msf')
