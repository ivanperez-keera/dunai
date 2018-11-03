-- | Monadic Stream Functions are synchronized stream functions
--   with side effects.
--
--   'MSF's are defined by a function
--   @unMSF :: MSF m a b -> a -> m (b, MSF m a b)@
--   that executes one step of a simulation, and produces an output in a
--   monadic context, and a continuation to be used for future steps.
--
--   'MSF's are a generalisation of the implementation mechanism used by Yampa,
--   Wormholes and other FRP and reactive implementations.
--
--   When combined with different monads, they produce interesting effects. For
--   example, when combined with the 'Maybe' monad, they become transformations
--   that may stop producing outputs (and continuations). The 'Either' monad
--   gives rise to 'MSF's that end with a result (akin to Tasks in Yampa, and
--   Monadic FRP).
--
--   Flattening, that is, going from some structure @MSF (t m) a b@ to @MSF m a b@
--   for a specific transformer @t@ often gives rise to known FRP constructs.
--   For instance, flattening with 'EitherT' gives rise to switching, and
--   flattening with 'ListT' gives rise to parallelism with broadcasting.
--
--   'MSF's can be used to implement many FRP variants, including Arrowized FRP,
--   Classic FRP, and plain reactive programming. Arrowized and applicative
--   syntax are both supported.
--
--   For a very detailed introduction to 'MSF's, see:
--   <http://dl.acm.org/citation.cfm?id=2976010>
--   (mirror: <http://www.cs.nott.ac.uk/~psxip1/#FRPRefactored>).
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

