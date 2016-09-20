{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MonadicStreamFunction.ArrowPlus where

import Control.Arrow
import Control.Monad

import Data.MonadicStreamFunction.Core

instance (Monad m, MonadPlus m) => ArrowZero (MSF m) where
  zeroArrow = MSF $ const mzero

instance (Monad m, MonadPlus m) => ArrowPlus (MSF m) where
  sf1 <+> sf2 = MSF $ \a -> unMSF sf1 a `mplus` unMSF sf2 a
