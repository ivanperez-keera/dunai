{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MonadicStreamFunction.ArrowChoice where

import Control.Arrow

import Data.MonadicStreamFunction.Core

instance Monad m => ArrowChoice (MStreamF m) where
  left sf = MStreamF f
    where
      f (Left a) = do (b, sf') <- unMStreamF sf a
                      return (Left b, left sf')
      f (Right c) = return (Right c, left sf)
