module Data.MonadicStreamFunction.Core
  ( module Data.MonadicStreamFunction.Core
  , module Data.MonadicStreamFunction.InternalCore
  , module Control.Arrow
  )
  where

import Control.Arrow
import qualified Control.Category as C

import Data.MonadicStreamFunction.InternalCore (MSF, morphGS)

-- | 'Arrow' instance for 'MSF's.
instance Monad m => Arrow (MSF m) where

  arr f = arrM (return . f)

  -- first sf = MSF $ \(a,c) -> do
  --   (b, sf') <- unMSF sf a
  --   b `seq` return ((b, c), first sf')

  first = morphGS $ \f (a,c) -> do
            (b, msf') <- f a
            return ((b, c), msf')


-- * Functor and applicative instances

-- | 'Functor' instance for 'MSF's.
instance Monad m => Functor (MSF m a) where
  fmap f msf = msf >>> arr f
  -- fmap f msf = MSF $ fmap fS . unMSF msf
  --   where
  --     fS (b, cont) = (f b, fmap f cont)

-- | 'Applicative' instance for 'MSF's.
instance (Functor m, Monad m) => Applicative (MSF m a) where
  -- It is possible to define this instance with only Applicative m
  pure = arr . const
  fs <*> bs = (fs &&& bs) >>> arr (uncurry ($))


-- ** Lifting point-wise computations

-- | Apply a monadic transformation to every element of the input stream.
--
-- Generalisation of 'arr' from 'Arrow' to monadic functions.
arrM :: Monad m => (a -> m b) -> MSF m a b
--arrM f = go
--  where go = MSF $ \a -> do
--               b <- f a
--               return (b, go)
arrM f = morphGS (\i a -> i a >>= \(_,c) -> f a >>= \b -> return (b, c)) C.id

