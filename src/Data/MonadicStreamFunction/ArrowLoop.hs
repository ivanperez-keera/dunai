{-# LANGUAGE RecursiveDo          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.MonadicStreamFunction.ArrowLoop where

import Data.MonadicStreamFunction.Core

-- External
import Control.Arrow
import Control.Monad.Fix

instance (Monad m, MonadFix m) => ArrowLoop (MSF m) where
  -- loop :: a (b, d) (c, d) -> a b c
  loop sf = MSF $ \a -> do
              rec ((b,c), sf') <- unMSF sf (a, c)
              return (b, loop sf')
