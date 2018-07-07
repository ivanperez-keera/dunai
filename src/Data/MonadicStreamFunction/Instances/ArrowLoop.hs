{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RecursiveDo          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Instance of 'ArrowLoop' for Monadic Stream Functions ('MSF').
--
--   Import this module to include that (orphan) instance.
--
--   This is only defined for monads that are instances of 'MonadFix'.
module Data.MonadicStreamFunction.Instances.ArrowLoop where

import Data.MonadicStreamFunction.Core

-- External
import Control.Arrow
import Control.Monad.Fix

-- | 'ArrowLoop' instance for MSFs. The monad must be an instance of
-- 'MonadFix'.
instance MonadFix m => ArrowLoop (MSF m) where
  loop :: MSF m (b, d) (c, d) -> MSF m b c
  loop sf = MSF $ \a -> do
              rec ((b,c), sf') <- unMSF sf (a, c)
              return (b, loop sf')
