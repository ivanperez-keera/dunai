-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Useful auxiliary functions and definitions.
module Data.MonadicStreamFunction.Util.Delay
    ( iPre
    , iPost
    , next
    , fifo
    )
   where

-- External imports
import Control.Arrow (arr, (>>>))

-- Internal imports
import Data.MonadicStreamFunction.Core (MSF, feedback)

-- | Delay a signal by one sample.
iPre :: Monad m
     => a         -- ^ First output
     -> MSF m a a
iPre firsta = feedback firsta $ arr swap
  where
    swap (a, b) = (b, a)

-- | Preprends a fixed output to an 'MSF'. The first input is completely
-- ignored.
iPost :: Monad m => b -> MSF m a b -> MSF m a b
iPost b sf = sf >>> (feedback (Just b) $ arr $ \(c, ac) -> case ac of
  Nothing -> (c, Nothing)
  Just b' -> (b', Nothing))

-- | Preprends a fixed output to an 'MSF', shifting the output.
next :: Monad m => b -> MSF m a b -> MSF m a b
next b sf = sf >>> iPre b

-- | Buffers and returns the elements in FIFO order, returning 'Nothing'
-- whenever the buffer is empty.
fifo :: Monad m => MSF m [a] (Maybe a)
fifo = feedback [] (arr (safeSnoc . uncurry fifoAppend))
  where
    -- | Append a new list to an accumulator in FIFO order.
    fifoAppend :: [x] -> [x] -> [x]
    fifoAppend as accum = accum ++ as

    -- | Split a list into the head and the tail.
    safeSnoc :: [x] -> (Maybe x, [x])
    safeSnoc []     = (Nothing, [])
    safeSnoc (x:xs) = (Just x, xs)
