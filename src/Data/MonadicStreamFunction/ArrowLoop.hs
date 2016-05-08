{-# LANGUAGE RecursiveDo #-}
module Data.MonadicStreamFunction.ArrowLoop where

import Data.MonadicStreamFunction.Core

-- External
import Control.Arrow
import Control.Category (Category(..))
import Control.Monad
import Control.Monad.Fix

instance (Monad m, MonadFix m) => ArrowLoop (MStreamF m) where
  -- loop :: a (b, d) (c, d) -> a b c
  loop sf = MStreamF $ \a -> do
              rec ((b,c), sf') <- unMStreamF sf (a, c)
              return (b, loop sf')
