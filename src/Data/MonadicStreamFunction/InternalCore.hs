{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}
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
--   This modules defines only the minimal core. Hopefully, other functions can be
--   defined in terms of the functions in this module without accessing the
--   MSF constuctor.


-- NOTE TO IMPLEMENTORS:
--
-- This module contains the core. Only the core. It should be possible
-- to define every function and type outside this module, except for the
-- instances for ArrowLoop, ArrowChoice, etc., without access to the
-- internal constructor for MSF and the function 'unMSF'.
--
-- It's very hard to know what IS essential to framework and if we start
-- adding all the functions and instances that *may* be useful in one
-- module.
--
-- By separating some instances and functions in other modules, we can
-- easily understand what is the essential idea and then analyse how it
-- is affected by an extension. It also helps demonstrate that something
-- works for MSFs + ArrowChoice, or MSFs + ArrowLoop, etc.
--
-- To address potential violations of basic design principles (like 'not
-- having orphan instances'), the main module Data.MonadicStreamFunction
-- exports everything. Users should *never* import this module here
-- individually, but the main module instead.

module Data.MonadicStreamFunction.InternalCore where

-- External
import Control.Applicative
import Control.Category (Category(..))
import Control.Monad

import Prelude hiding ((.), id, sum)

-- * Definitions

-- | Stepwise, side-effectful 'MSF's without implicit knowledge of time.
--
-- 'MSF's should be applied to streams or executed indefinitely or until they
-- terminate. See 'reactimate' and 'reactimateB' for details. In general,
-- calling the value constructor 'MSF' or the function 'unMSF' is discouraged.
data MSF m a b = MSF { unMSF :: a -> m (b, MSF m a b) }

-- Instances

-- | Instance definition for 'Category'. Defines 'id' and '.'.
instance Monad m => Category (MSF m) where
  id = go
    where go = MSF $ \a -> return (a, go)
  sf2 . sf1 = MSF $ \a -> do
    (b, sf1') <- unMSF sf1 a
    (c, sf2') <- unMSF sf2 b
    let sf' = sf2' . sf1'
    c `seq` return (c, sf')

-- * Monadic computations and 'MSF's

-- | Generic lifting of a morphism to the level of 'MSF's.
--
-- Natural transformation to the level of 'MSF's.
--
-- __Mathematical background:__ The type @a -> m (b, c)@ is a functor in @c@,
-- and @MSF m a b@ is its greatest fixpoint, i.e. it is isomorphic to the type
-- @a -> m (b, MSF m a b)@, by definition.
-- The types @m@, @a@ and @b@ are parameters of the functor.
-- Taking a fixpoint is functorial itself, meaning that a morphism
-- (a natural transformation) of two such functors gives a morphism
-- (an ordinary function) of their fixpoints.
--
-- This is in a sense the most general "abstract" lifting function,
-- i.e. the most general one that only changes input, output and side effect
-- types, and doesn't influence control flow.
-- Other handling functions like exception handling or 'ListT' broadcasting
-- necessarily change control flow.
morphGS :: Functor m2
        => (InMorph m1 a1 b1 ~> InMorph m2 a2 b2)
          -- ^ The natural transformation. @mi@, @ai@ and @bi@ for @i = 1, 2@
          --   can be chosen freely, but @c@ must be universally quantified
        -> MSF m1 a1 b1
        -> MSF m2 a2 b2
morphGS morph msf = MSF $ \ a2 -> morphU <$> morph (unMSF msf) a2 where
  morphU (b2, cont) = (b2, morphGS morph cont)

type InMorph m a b c = a -> m (b, c)

-- * Feedback loops

-- | Well-formed looped connection of an output component as a future input.
feedback :: Functor m => c -> MSF m (a, c) (b, c) -> MSF m a b
feedback c sf = MSF $ \a -> unMSF sf (a, c) `pamf` feed where
  feed ((b, c'), cont) = (b, feedback c' cont)

-- * Execution/simulation

-- | Apply a monadic stream function to a list.
--
-- Because the result is in a monad, it may be necessary to
-- traverse the whole list to evaluate the value in the results to WHNF.
-- For example, if the monad is the maybe monad, this may not produce anything
-- if the 'MSF' produces 'Nothing' at any point, so the output stream cannot
-- consumed progressively.
--
-- To explore the output progressively, use 'liftMSF' and '(>>>)'', together
-- with some action that consumes/actuates on the output.
--
-- This is called 'runSF' in Liu, Cheng, Hudak, "Causal Commutative Arrows and
-- Their Optimization"
embed :: Monad m => MSF m a b -> [a] -> m [b]
embed _  []     = return []
embed sf (a:as) = do
  (b, sf') <- unMSF sf a
  bs       <- embed sf' as
  return (b:bs)

-- | Run an 'MSF' indefinitely passing a unit-carrying input stream.
reactimate :: Monad m => MSF m () () -> m ()
reactimate sf = do
  (_, sf') <- unMSF sf ()
  reactimate sf'

-- Implementation of some common operations for type-class instances, without the instances
map_msf :: Functor m => (b -> c) -> MSF m a b -> MSF m a c
map_msf f sf = MSF $ \ a -> fS <$> unMSF sf a where
  fS (b, cont) = (f b, map_msf f cont)

-- Pure: a constant MSF that always returns the given value, without effects, and continues with itself.
pure_msf :: Applicative m => b -> MSF m a b
pure_msf b = MSF $ const . pure $ (b, pure_msf b)

-- Zips, one-by-one, the function output of first stream with the output of the second one.
ap_msf :: Applicative m => MSF m a (b -> c) -> MSF m a b -> MSF m a c
ap_msf sff sfb = MSF $ \a -> liftA2 mix (unMSF sff a) (unMSF sfb a) where
  mix (f, sff') (b, sfb')= (f b, ap_msf sff' sfb')

-- Raises a Kleisli-like function into an MSF,
arrM_msf :: Functor m => (a -> m b) -> MSF m a b
arrM_msf f = MSF $ \a -> f a <&> (\b -> (b, arrM_msf f))

-- An effect-less MSF that just applies the function and continues with itself.
arr_msf :: Applicative m => (a -> b) -> MSF m a b
arr_msf f = MSF $ \a -> pure (f a, arr_msf f)

-- Tuples the inputs and outputs of the MSF with a value that is left unchanged.
first_msf :: Functor m => MSF m a b -> MSF m (a,c) (b,c)
first_msf sf = MSF $ \ (a,c) -> fST c <$> unMSF sf a where
  fST c (b, cont) = ((b, c), first_msf cont)

-- Changes the container functor
-- Simplified version of morphGS, which only changes the monadic type
mapK :: Functor n => (m ~> n) -> MSF m a b -> MSF n a b
mapK nt sf = MSF $ fmap inM . nt . unMSF sf  where
  inM (b, cont) = (b, mapK nt cont)

-- | Apply an 'MSF' to every input. Freezes temporarily if the input is
-- 'Nothing', and continues as soon as a 'Just' is received.
mapMaybeMsf :: Applicative m => MSF m a b -> MSF m (Maybe a) (Maybe b)
mapMaybeMsf msf = MSF $ maybe ifNothing ifJust where
  ifNothing = pure (Nothing, mapMaybeMsf msf)
  ifJust a  = unMSF msf a <&> (\ (b, cont) -> (Just b, mapMaybeMsf cont) )


-- | Applies a function to produce an additional side effect and passes the
-- input unchanged.
sideEffectMsf :: Functor m => (a -> m b) -> MSF m a a
sideEffectMsf f = MSF $ \a -> f a <&> \ _ -> (a, sideEffectMsf f)

-- In newer versions of Base, there is Functor. <&> operator
pamf :: Functor m => m a -> (a -> b) -> m b
pamf = flip fmap

-- | A natural transformation from @f@ to @g@.
-- Copied from the natural-transformations package
-- http://hackage.haskell.org/package/natural-transformation-0.4/docs/Control-Natural.html
infixr 0 ~>
type (~>) f g = forall x. f x -> g x

-- infix alias for flipped fmap
-- copied from the base package
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

infixl 1 <&>
