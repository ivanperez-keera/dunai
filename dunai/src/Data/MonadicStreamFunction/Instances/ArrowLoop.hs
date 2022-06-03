{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecursiveDo  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Instance of 'ArrowLoop' for Monadic Stream Functions ('MSF').
--
-- Import this module to include that (orphan) instance.
--
-- This is only defined for monads that are instances of 'MonadFix'.
module Data.MonadicStreamFunction.Instances.ArrowLoop where

-- External imports
import Control.Arrow     (ArrowLoop (..))
import Control.Monad.Fix (MonadFix)

-- Internal imports
import Data.MonadicStreamFunction.Core         ()
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- | 'ArrowLoop' instance for MSFs. The monad must be an instance of
-- 'MonadFix'.
instance MonadFix m => ArrowLoop (MSF m) where
  loop :: MSF m (b, d) (c, d) -> MSF m b c
  loop sf = MSF $ \a -> do
              rec ((b, c), sf') <- unMSF sf (a, c)
              return (b, loop sf')
