{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Instance of 'ArrowChoice' for Monadic Stream Functions ('MSF').
--
-- Import this module to include that (orphan) instance.
module Data.MonadicStreamFunction.Instances.ArrowChoice where

-- External imports
import Control.Arrow (ArrowChoice (..))

-- Internal imports
import Data.MonadicStreamFunction.Core         ()
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- | 'ArrowChoice' instance for MSFs.
instance Monad m => ArrowChoice (MSF m) where
  left :: MSF m a b -> MSF m (Either a c) (Either b c)
  left sf = MSF f
    where
      f (Left a) = do (b, sf') <- unMSF sf a
                      return (Left b, left sf')
      f (Right c) = return (Right c, left sf)
