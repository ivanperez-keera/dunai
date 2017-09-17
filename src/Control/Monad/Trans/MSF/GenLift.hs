{-# LANGUAGE Rank2Types          #-}
module Control.Monad.Trans.MSF.GenLift where

import Control.Applicative
import Data.MonadicStreamFunction

-- * Attempt at writing a more generic MSF lifting combinator.  This is
-- here only to make it easier to find, in a perfect world we'd move
-- this to a different module/branch, or at least to the bottom of the
-- file.
--
-- TODO: does this also work well with the state and the writer monads?
--
-- Even if this code works, it's difficult to understand the concept.
--
-- It is also unclear how much it helps. Ideally, the auxiliary function
-- should operate only on monadic values, not monadic stream functions.
-- That way we could separate concepts: namely the recursion pattern
-- from the monadic lifting/unlifting/sequencing.
--
-- Maybe if we split f in several functions, one that does some sort of
-- a -> a1 transformation, another that does some b1 -> b
-- transformation, with the monads and continuations somewhere, it'll
-- make more sense.
--
-- Based on this lifting function we can also defined all the other
-- liftings we have in Core:
--
-- liftMSFPurer' :: (Monad m1, Monad m)
--                    => (m1 (b, MSF m1 a b) -> m (b, MSF m1 a b))
--                    -> MSF m1 a b
--                    -> MSF m  a b
-- liftMSFPurer' f = lifterS (\g a -> f $ g a)
--
-- More liftings:
-- liftMSFTrans = liftMSFPurer lift
-- liftMSFBase  = liftMSFPurer liftBase
--
-- And a strict version of liftMSFPurer:
-- liftMStreamPurer' f = liftMSFPurer (f >=> whnfVal)
--   where whnfVal p@(b,_) = b `seq` return p
--
-- MB: I'm not sure we're gaining much insight by rewriting all the lifting
-- functions like that.
-- IP: I said the same thing above ("It is also unclear how much it
-- helps."). It's work in progress.
--
-- MB: The type (a1 -> m1 (b1, MSF m1 a1 b1)) is just MSF m1 a1 b1.
-- IP: I'm looking for a lifting pattern in terms of m m1 a b a1 and b1. By
-- exposing the function, I'm hoping to *eventually see* the pattern. If I hide
-- it in the MSF, then it'll always remain hidden.
lifterS :: (Monad m, Monad m1)
        => ((a1 -> m1 (b1, MSF m1 a1 b1)) -> a -> m (b, MSF m1 a1 b1))
        -> MSF m1 a1 b1
        -> MSF m  a  b
lifterS f msf = MSF $ \a -> do
  (b, msf') <- f (unMSF msf) a
  return (b, lifterS f msf')

-- ** Another wrapper idea
transS :: (Monad m1, Monad m2)
       => (a2 -> m1 a1)
       -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, c))
       -> MSF m1 a1 b1 -> MSF m2 a2 b2
transS transformInput transformOutput msf = MSF $ \a2 -> do
    (b2, msf') <- transformOutput a2 $ unMSF msf =<< transformInput a2
    return (b2, transS transformInput transformOutput msf')

-- ** A more general lifting mechanism that enables recovery.
transG1 :: (Monad m1, Functor m2, Monad m2)
        => (a2 -> m1 a1)
        -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, c))
        -> MSF m1 a1 b1 -> MSF m2 a2 b2
transG1 transformInput transformOutput msf =
  transG transformInput transformOutput' msf
    where
      -- transformOutput' :: forall c. a2 -> m1 (b1, c) -> m2 (b2, Maybe c)
      transformOutput' a b = second Just <$> transformOutput a b

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

-- ** Wrapping/unwrapping
--
-- IP: Alternative formulation (typechecks just fine):
--
-- FIXME: The foralls may get in the way. They may not be necessary.  MB
-- raised the issue already for similar code in Core.
--
type Wrapper   m1 m2 t1 t2 = forall a b . (t1 a -> m2 b     ) -> (a    -> m1 (t2 b))
type Unwrapper m1 m2 t1 t2 = forall a b . (a    -> m1 (t2 b)) -> (t1 a -> m2 b     )
--
-- Helper type, for when we need some identity * -> * type constructor that
-- does not get in the way.
--
type Id a = a
