module FRP.Dunai.Debug where

import Debug.Trace
import Data.MonadicStreamFunction hiding (trace)
import System.IO.Unsafe

-- ** Debugging

traceMSF :: Monad m
         => Show a
        => MSF m a a
traceMSF = traceMSFWith show

traceMSFWith :: Monad m
             => (a -> String)
             -> MSF m a a
traceMSFWith f = arr (\x -> trace (f x) x)

traceMSFWithIO :: (a -> IO b)
               -> MSF IO a a
traceMSFWithIO f = arrM (\x -> f x >> return x)
