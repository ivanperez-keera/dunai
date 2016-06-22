{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MonadicStreamFunction.ArrowPlus where

import Control.Arrow
import Control.Monad

import Data.MonadicStreamFunction.Core

instance (Monad m, MonadPlus m) => ArrowZero (MStreamF m) where
  zeroArrow = MStreamF $ const mzero

instance (Monad m, MonadPlus m) => ArrowPlus (MStreamF m) where
  sf1 <+> sf2 = MStreamF $ \a -> unMStreamF sf1 a `mplus` unMStreamF sf2 a
