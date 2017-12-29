{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instance of 'ArrowPlus' for Monadic Stream Functions ('MSF').
--
--   Import this module to include that (orphan) instance.
--
--   This is only defined for monads that are instances of 'MonadPlus'.
module Data.MonadicStreamFunction.Instances.ArrowPlus where

-- base
import Control.Arrow
import Control.Monad
import Control.Applicative

-- dunai
import Data.MonadicStreamFunction.Core

-- | Instance of 'ArrowZero' for Monadic Stream Functions ('MSF').
--   The monad must be an instance of 'MonadPlus'.
instance (Monad m, MonadPlus m) => ArrowZero (MSF m) where
  zeroArrow = MSF $ const mzero

-- | Instance of 'ArrowPlus' for Monadic Stream Functions ('MSF').
--   The monad must be an instance of 'MonadPlus'.
instance (Monad m, MonadPlus m) => ArrowPlus (MSF m) where
  sf1 <+> sf2 = MSF $ \a -> unMSF sf1 a `mplus` unMSF sf2 a

instance (Monad m, MonadPlus m) => Alternative (MSF m a) where
  empty = zeroArrow
  (<|>) = (<+>)
