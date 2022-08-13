-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Useful auxiliary functions and definitions.
module Data.MonadicStreamFunction.Util.Effect
    ( withSideEffect
    , withSideEffect_
    )
   where

-- External imports
import Control.Arrow    (arr, (&&&), (>>>))
import Control.Category (id)
import Prelude          hiding (id)

-- Internal imports
import Data.MonadicStreamFunction.Core (MSF, arrM)

-- | Applies a function to produce an additional side effect and passes the
-- input unchanged.
withSideEffect :: Monad m => (a -> m b) -> MSF m a a
withSideEffect method = (id &&& arrM method) >>> arr fst

-- | Produces an additional side effect and passes the input unchanged.
withSideEffect_ :: Monad m => m b -> MSF m a a
withSideEffect_ method = withSideEffect $ const method
