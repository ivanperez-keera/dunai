{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Number instances for 'MSF's that produce numbers. This allows you to use
-- numeric operators with 'MSF's that output numbers, for example, you can
-- write:
--
-- @
-- msf1 :: MSF Input Double -- defined however you want
-- msf2 :: MSF Input Double -- defined however you want
-- msf3 :: MSF Input Double
-- msf3 = msf1 + msf2
-- @
--
-- instead of
--
-- @
-- msf3 = (msf1 &&& msf2) >>> arr (uncurry (+))
-- @
--
-- Instances are provided for the type classes 'Num', 'Fractional' and
-- 'Floating'.
module Data.MonadicStreamFunction.Instances.Num where

-- Internal imports
import Control.Arrow.Util              (constantly, elementwise, elementwise2)
import Data.MonadicStreamFunction.Core (MSF)

-- | 'Num' instance for 'MSF's.
instance (Monad m, Num b) => Num (MSF m a b) where
  (+)         = elementwise2 (+)
  (-)         = elementwise2 (-)
  (*)         = elementwise2 (*)
  abs         = elementwise abs
  signum      = elementwise signum
  negate      = elementwise negate
  fromInteger = constantly . fromInteger

-- | 'Fractional' instance for 'MSF's.
instance (Monad m, Fractional b) => Fractional (MSF m a b) where
  fromRational = constantly . fromRational
  (/)          = elementwise2 (/)
  recip        = elementwise recip

-- | 'Floating' instance for 'MSF's.
instance (Monad m, Floating b) => Floating (MSF m a b) where
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
