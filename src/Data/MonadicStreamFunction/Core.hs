{-# LANGUAGE Rank2Types #-}
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
  ( -- * Types
    MSF
    -- * Lifting and Monadic transformations
    -- ** Lifting point-wise computations
  , constM
  , arrM
  , liftBaseM
    -- ** Trans-monadic MSF combinators
    -- *** MonadBase
  , liftBaseS
  , (^>>>)
  , (>>>^)
    -- *** MonadTrans
  , liftTransS
    -- *** Generic Monadic Transformations
  , morphS
  , morphGS
    -- * Depending on the past
  , feedback
    -- * Simulation
  , reactimate
  , embed
  , module Control.Arrow
  )
  where

import Control.Applicative
import Control.Arrow
import Control.Category as C
import Control.Monad.Base
import Control.Monad.Trans.Class
import Data.Tuple (swap)
import Prelude hiding ((.), id, sum)

import Data.MonadicStreamFunction.InternalCore (MSF, morphGS, feedback, reactimate, embed)

-- * Definitions

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

-- | Lifts a monadic computation into a Stream.
constM :: Monad m => m b -> MSF m a b
constM = arrM . const

-- | Apply a monadic transformation to every element of the input stream.
--
-- Generalisation of 'arr' from 'Arrow' to monadic functions.
arrM :: Monad m => (a -> m b) -> MSF m a b
--arrM f = go
--  where go = MSF $ \a -> do
--               b <- f a
--               return (b, go)
arrM f = morphGS (\i a -> i a >>= \(_,c) -> f a >>= \b -> return (b, c)) C.id

-- | Monadic lifting from one monad into another
liftBaseM :: (Monad m2, MonadBase m1 m2) => (a -> m1 b) -> MSF m2 a b
liftBaseM = arrM . (liftBase .)

-- ** MSF combinators that apply monad transformations

-- | Lift innermost monadic actions in monad stack (generalisation of
-- 'liftIO').
liftBaseS :: (Monad m2, MonadBase m1 m2) => MSF m1 a b -> MSF m2 a b
liftBaseS = morphS liftBase

-- *** MonadBase
-- | Lift the first 'MSF' into the monad of the second.
(^>>>) :: MonadBase m1 m2 => MSF m1 a b -> MSF m2 b c -> MSF m2 a c
sf1 ^>>> sf2 = liftBaseS sf1 >>> sf2
{-# INLINE (^>>>) #-}

-- | Lift the second 'MSF' into the monad of the first.
(>>>^) :: MonadBase m1 m2 => MSF m2 a b -> MSF m1 b c -> MSF m2 a c
sf1 >>>^ sf2 = sf1 >>> liftBaseS sf2
{-# INLINE (>>>^) #-}

-- *** MonadTrans

-- | Lift inner monadic actions in monad stacks.

liftTransS :: (MonadTrans t, Monad m, Monad (t m))
           => MSF m a b
           -> MSF (t m) a b
liftTransS = morphS lift

-- *** Generic monadic transformation

-- | Apply trans-monadic actions (in an arbitrary way).
--
-- This is just a convenience function when you have a function to move across
-- monads, because the signature of 'morphGS' is a bit complex.
morphS :: (Monad m2, Monad m1)
      => (forall c . m1 c -> m2 c)
      -> MSF m1 a b
      -> MSF m2 a b
morphS morph = morphGS morph'
  where
    -- The following makes the a's and the b's the same, and it just says:
    -- whatever function m1F you give me to apply to every sample, I use morph
    -- on the result to go from m1 to m2.
    --
    -- Remember that:
    -- morphGS :: Monad m2
    --         => (forall c . (a1 -> m1 (b1, c)) -> (a2 -> m2 (b2, c)))
    --           -- ^ The natural transformation. @mi@, @ai@ and @bi@ for @i = 1, 2@
    --           --   can be chosen freely, but @c@ must be universally quantified
    --         -> MSF m1 a1 b1
    --         -> MSF m2 a2 b2
    --
    --  morph' :: (forall c . (a -> m1 (b, c)) -> (a -> m2 (b, c)))
        morph' m1F = morph . m1F

-- IPerez: There is an alternative signature for liftMStreamPurer that also
-- works, and makes the code simpler:
--
-- morphS :: Monad m => (m1 (b, MSF m1 a b) -> m (b, MSF m1 a b)) -> MSF m1 a b -> MSF m a b
--
-- Then we can express:
--
-- liftTransS = morphS lift
-- liftBaseS  = morphS liftBase
--
-- We could also define a strict version of morphS as follows:
--
-- morphS'  f = morphS (f >=> whnfVal)
--   where whnfVal p@(b,_) = b `seq` return p
--
-- and leave morphS as a lazy version (by default).
