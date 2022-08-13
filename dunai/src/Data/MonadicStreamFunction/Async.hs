-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- This module contains operations on monadic streams that are asynchronous,
-- i.e. that change the speed at which data enters or leaves the 'MSF'.
module Data.MonadicStreamFunction.Async
    {-# DEPRECATED "Use Data.MonadicStreamFunction.Util instead" #-}
    (concatS)
  where


-- Internal imports
import Data.MonadicStreamFunction.Util (concatS)
