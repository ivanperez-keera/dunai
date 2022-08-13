-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'ReactHandle's.
--
-- Sometimes it is beneficial to give control to an external main loop, for
-- example OpenGL or a hardware-clocked audio server like JACK. This module
-- makes Dunai compatible with external main loops.
module Data.MonadicStreamFunction.ReactHandle
    {-# DEPRECATED "Use Data.MonadicStreamFunction.Util instead" #-}
    ( ReactHandle
    , reactInit
    , react
    )
  where

-- Internal imports
import Data.MonadicStreamFunction.Util (ReactHandle, reactInit, react)
