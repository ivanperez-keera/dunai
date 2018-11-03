{-# LANGUAGE Rank2Types     #-}
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
import Control.Monad.Base
import Control.Monad.Trans.Class

import Data.MonadicStreamFunction.InternalCore (MSF, morphGS, feedback, reactimate, embed)

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

-- | Lifts a computation into a Stream.
arrM_ :: Monad m => m b -> MSF m a b
arrM_ = arrM . const

-- | Monadic lifting from one monad into another
liftS :: (Monad m2, MonadBase m1 m2) => (a -> m1 b) -> MSF m2 a b
liftS = arrM . (liftBase .)

-- | Lift the first 'MSF' into the monad of the second.
(^>>>) :: MonadBase m1 m2 => MSF m1 a b -> MSF m2 b c -> MSF m2 a c
sf1 ^>>> sf2 = liftMSFBase sf1 >>> sf2
{-# INLINE (^>>>) #-}

-- | Lift the second 'MSF' into the monad of the first.
(>>>^) :: MonadBase m1 m2 => MSF m2 a b -> MSF m1 b c -> MSF m2 a c
sf1 >>>^ sf2 = sf1 >>> liftMSFBase sf2
{-# INLINE (>>>^) #-}

-- | Lift innermost monadic actions in monad stack (generalisation of
-- 'liftIO').
liftMSFBase :: (Monad m2, MonadBase m1 m2) => MSF m1 a b -> MSF m2 a b
liftMSFBase = liftMSFPurer liftBase

-- | Lift inner monadic actions in monad stacks.

liftMSFTrans :: (MonadTrans t, Monad m, Monad (t m))
             => MSF m a b
             -> MSF (t m) a b
liftMSFTrans = liftMSFPurer lift

-- | Lifting purer monadic actions (in an arbitrary way)
liftMSFPurer :: (Monad m2, Monad m1)
             => (forall c . m1 c -> m2 c) -> MSF m1 a b -> MSF m2 a b
liftMSFPurer morph = morphGS (morph .)


-- IPerez: There is an alternative signature for liftMStreamPurer that also
-- works, and makes the code simpler:
--
-- liftMSFPurer :: Monad m => (m1 (b, MSF m1 a b) -> m (b, MSF m1 a b)) -> MSF m1 a b -> MSF m a b
--
-- Then we can express:
--
-- liftMSFTrans = liftMSFPurer lift
-- liftMSFBase  = liftMSFPurer liftBase
--
-- We could also define a strict version of liftMSFPurer as follows:
--
-- liftMStreamPurer' f = liftMSFPurer (f >=> whnfVal)
--   where whnfVal p@(b,_) = b `seq` return p
--
-- and leave liftMSFPurer as a lazy version (by default).


-- | Lifting combinator to move from one monad to another, if one has a
-- function to run computations in one monad into another. Note that, unlike a
-- polymorphic lifting function @forall a . m a -> m1 a@, this auxiliary
-- function needs to be a bit more structured, although less structured than
-- 'lifterS'.

transS :: (Monad m1, Monad m2)
       => (a2 -> m1 a1)
       -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, c))
       -> MSF m1 a1 b1 -> MSF m2 a2 b2
transS transInput transOutput = morphGS $ \f a2 -> transOutput a2 $ do
  a1 <- transInput a2
  f a1
