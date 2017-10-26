{-# LANGUAGE InstanceSigs         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instance of 'ArrowChoice' for Monadic Stream Functions ('MSF').
--
--   Import this module to include that (orphan) instance.
module Data.MonadicStreamFunction.ArrowChoice where

import Control.Arrow

import Data.MonadicStreamFunction.Core

instance Monad m => ArrowChoice (MSF m) where
  left :: MSF m a b -> MSF m (Either a c) (Either b c)
  left sf = MSF f
    where
      f (Left a) = do (b, sf') <- unMSF sf a
                      return (Left b, left sf')
      f (Right c) = return (Right c, left sf)
