{-# LANGUAGE CPP #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Useful auxiliary functions and definitions.
module Data.MonadicStreamFunction.Util.Fold
   where

-- External imports
import Control.Arrow    (arr, (>>>))
import Data.VectorSpace (VectorSpace, zeroVector, (^+^))

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid, mempty, mappend)
#endif

-- Internal imports
import Data.MonadicStreamFunction.Core (MSF, feedback)

-- * Folding

-- ** Folding for 'VectorSpace' instances

-- | Count the number of simulation steps. Produces 1, 2, 3,...
count :: (Num n, Monad m) => MSF m a n
count = arr (const 1) >>> accumulateWith (+) 0

-- | Sums the inputs, starting from zero.
sumS :: (VectorSpace v s, Monad m) => MSF m v v
sumS = sumFrom zeroVector

-- | Sums the inputs, starting from an initial vector.
sumFrom :: (VectorSpace v s, Monad m) => v -> MSF m v v
sumFrom = accumulateWith (^+^)

-- ** Folding for monoids

-- | Accumulate the inputs, starting from 'mempty'.
mappendS :: (Monoid n, Monad m) => MSF m n n
mappendS = mappendFrom mempty
{-# INLINE mappendS #-}

-- | Accumulate the inputs, starting from an initial monoid value.
mappendFrom :: (Monoid n, Monad m) => n -> MSF m n n
mappendFrom = accumulateWith mappend

-- ** Generic folding \/ accumulation

-- | Applies a function to the input and an accumulator, outputting the updated
-- accumulator. Equal to @\f s0 -> feedback s0 $ arr (uncurry f >>> dup)@.
accumulateWith :: Monad m => (a -> s -> s) -> s -> MSF m a s
accumulateWith f s0 = feedback s0 $ arr g
  where
    g (a, s) = let s' = f a s in (s', s')

-- | Applies a transfer function to the input and an accumulator, returning the
-- updated accumulator and output.
mealy :: Monad m => (a -> s -> (b, s)) -> s -> MSF m a b
mealy f s0 = feedback s0 $ arr $ uncurry f

-- * Unfolding

-- | Generate outputs using a step-wise generation function and an initial
-- value.
unfold :: Monad m => (a -> (b, a)) -> a -> MSF m () b
unfold f a = feedback a (arr (snd >>> f))

-- | Generate outputs using a step-wise generation function and an initial
-- value. Version of 'unfold' in which the output and the new accumulator are
-- the same. Should be equal to @\f a -> unfold (f >>> dup) a@.
repeatedly :: Monad m => (a -> a) -> a -> MSF m () a
repeatedly f = unfold $ f >>> dup
  where
    dup a = (a, a)
