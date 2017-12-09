{-# LANGUAGE Rank2Types          #-}

-- | More generic lifting combinators.
--
-- This module contains more generic lifting combinators. It includes several
-- implementations, and obviously should be considered work in progress.  The
-- goal is to make this both simple and conceptually understandable.
module Control.Monad.Trans.MSF.GenLift where

import Control.Applicative
import Data.MonadicStreamFunction

-- | Lifting combinator to move from one monad to another, if one has a
-- function to run computations in one monad into another. Note that, unlike a
-- polymorphic lifting function @forall a . m a -> m1 a@, this auxiliary
-- function needs to be a bit more structured.
lifterS :: (Monad m, Monad m1)
        => ((a1 -> m1 (b1, MSF m1 a1 b1)) -> a -> m (b, MSF m1 a1 b1))
        -> MSF m1 a1 b1
        -> MSF m  a  b
lifterS f msf = MSF $ \a -> do
  (b, msf') <- f (unMSF msf) a
  return (b, lifterS f msf')

-- | Lifting combinator to move from one monad to another, if one has a
-- function to run computations in one monad into another. Note that, unlike a
-- polymorphic lifting function @forall a . m a -> m1 a@, this auxiliary
-- function needs to be a bit more structured, although less structured than
-- 'lifterS'.

transS :: (Monad m1, Monad m2)
       => (a2 -> m1 a1)
       -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, c))
       -> MSF m1 a1 b1 -> MSF m2 a2 b2
transS transInput transOutput = hoistGen $ \f a2 -> transOutput a2 $ do
  a1 <- transInput a2
  f a1

-- | Lifting combinator to move from one monad to another, if one has a
-- function to run computations in one monad into another. Note that, unlike a
-- polymorphic lifting function @forall a . m a -> m1 a@, this auxiliary
-- function needs to be a bit more structured, although less structured than
-- 'lifterS'.
transG1 :: (Monad m1, Functor m2, Monad m2)
        => (a2 -> m1 a1)
        -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, c))
        -> MSF m1 a1 b1 -> MSF m2 a2 b2
transG1 = transS

-- | More general lifting combinator that enables recovery. Note that, unlike a
-- polymorphic lifting function @forall a . m a -> m1 a@, this auxiliary
-- function needs to be a bit more structured, and produces a Maybe value. The
-- previous MSF is used if a new one is not produced.
transG :: (Monad m1, Monad m2)
       => (a2 -> m1 a1)
       -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, Maybe c))
       -> MSF m1 a1 b1 -> MSF m2 a2 b2
transG transformInput transformOutput msf = go
  where go = MSF $ \a2 -> do
               (b2, msf') <- transformOutput a2 $ unMSF msf =<< transformInput a2
               case msf' of
                 Just msf'' -> return (b2, transG transformInput transformOutput msf'')
                 Nothing    -> return (b2, go)

-- transGN :: (Monad m1, Monad m2)
--         => (a2 -> m1 a1)
--         -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, [c]))
--         -> MSF m1 a1 b1 -> MSF m2 a2 b2
-- transGN transformInput transformOutput msf = go
--   where go = MSF $ \a2 -> do
--                (b2, msf') <- transformOutput a2 $ unMSF msf =<< transformInput a2
--                case msf' of
--                  []      -> return (b2, go)
--                  [msf''] -> return (b2, transGN transformInput transformOutput msf'')
--                  ms      ->

-- IP: Alternative formulation (typechecks just fine):
--
-- FIXME: The foralls may get in the way. They may not be necessary.  MB
-- raised the issue already for similar code in Core.
--
-- type Wrapper   m1 m2 t1 t2 = forall a b . (t1 a -> m2 b     ) -> (a    -> m1 (t2 b))
-- type Unwrapper m1 m2 t1 t2 = forall a b . (a    -> m1 (t2 b)) -> (t1 a -> m2 b     )
--
-- Helper type, for when we need some identity * -> * type constructor that
-- does not get in the way.
--
-- type Id a = a
