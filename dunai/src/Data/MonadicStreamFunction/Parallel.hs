-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Versions of arrow combinators that run things in parallel using 'par', if
-- possible.
module Data.MonadicStreamFunction.Parallel
    {-# DEPRECATED "Use Data.MonadicStreamFunction.Util instead" #-}
    ( (&|&)
    , (*|*)
    )
  where

-- Internal imports
import Data.MonadicStreamFunction.Util ((&|&), (*|*))
