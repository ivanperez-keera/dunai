-- |
-- Copyright  : (c) Ivan Perez, 2017
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Debug FRP networks by inspecting their behaviour inside.
module FRP.Dunai.Debug where

-- External imports
import Data.MonadicStreamFunction hiding (trace)
import Debug.Trace                (trace)
import System.IO.Unsafe           ()

-- ** Debugging

-- | Monadic Stream Function that prints the value passing through using
-- 'trace'.
traceMSF :: Monad m
         => Show a
        => MSF m a a
traceMSF = traceMSFWith show

-- | Monadic Stream Function that prints the value passing through using
-- 'trace', and a customizable 'show' function.
traceMSFWith :: Monad m
             => (a -> String)
             -> MSF m a a
traceMSFWith f = arr (\x -> trace (f x) x)

-- | Execute an IO action at every step, and ignore the result.
traceMSFWithIO :: (a -> IO b)
               -> MSF IO a a
traceMSFWithIO f = arrM (\x -> f x >> return x)
