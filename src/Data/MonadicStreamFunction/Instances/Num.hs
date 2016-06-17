{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MonadicStreamFunction.Instances.Num where

import Control.Arrow.Util
import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.Instances

instance (Monad m, Num b) => Num (MStreamF m a b) where
  (+)         = elementwise2 (+)
  (-)         = elementwise2 (-)
  (*)         = elementwise2 (*)
  abs         = elementwise abs
  signum      = elementwise signum
  negate      = elementwise negate
  fromInteger = constantly . fromInteger

instance (Monad m, Fractional b) => Fractional (MStreamF m a b) where
  fromRational = constantly . fromRational
  (/)          = elementwise2 (/)
  recip        = elementwise recip

instance (Monad m, Floating b) => Floating (MStreamF m a b) where
  pi      = constantly   pi
  exp     = elementwise  exp
  log     = elementwise  log
  sqrt    = elementwise  sqrt
  (**)    = elementwise2 (**)
  logBase = elementwise2 logBase
  sin     = elementwise  sin
  cos     = elementwise  cos
  tan     = elementwise  tan
  asin    = elementwise  asin
  acos    = elementwise  acos
  atan    = elementwise  atan
  sinh    = elementwise  sinh
  cosh    = elementwise  cosh
  tanh    = elementwise  tanh
  asinh   = elementwise  asinh
  acosh   = elementwise  acosh
  atanh   = elementwise  atanh
