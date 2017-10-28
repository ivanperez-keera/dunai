{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instance of 'ArrowPlus' for Monadic Stream Functions ('MSF').
--
--   Import this module to include that (orphan) instance.
--
--   This is only defined for monads that are instances of 'MonadPlus'.
module Data.MonadicStreamFunction.Instances.ArrowPlus where

import Control.Arrow
import Control.Monad

import Data.MonadicStreamFunction.Core

instance (Monad m, MonadPlus m) => ArrowZero (MSF m) where
  zeroArrow = MSF $ const mzero

instance (Monad m, MonadPlus m) => ArrowPlus (MSF m) where
  sf1 <+> sf2 = MSF $ \a -> unMSF sf1 a `mplus` unMSF sf2 a
