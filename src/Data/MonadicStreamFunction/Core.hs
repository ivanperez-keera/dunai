{-# LANGUAGE ExplicitForAll #-}
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
-- By separating some instances and functions in other modules , we can
-- easily understand what is the essential idea and then analyse how it
-- is affected by an extension. It also helps demonstrate that something
-- works for MSFs + ArrowChoice, or MSFs + ArrowLoop, etc.
--
-- To address potential violations of basic design principles (like 'not
-- having orphan instances'), the main module Data.MonadicStreamFunction
-- exports everything. Users should *never* import this module here
-- individually, but the main module instead.

module Data.MonadicStreamFunction.Core where

-- External
import Control.Arrow
import Control.Applicative
import Control.Category (Category(..))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class
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

-- ** Lifting point-wise computations

-- | Apply a monadic transformation to every element of the input stream.
--
-- Generalisation of 'arr' from 'Arrow' to monadic functions.
arrM :: Monad m => (a -> m b) -> MSF m a b
arrM f = go
  where go = MSF $ \a -> do
               b <- f a
               return (b, go)

-- ** Lifting 'MSF's

-- *** Generic 'MSF' Lifting

-- | Generic lifting of a morphism to the level of 'MSF's.
--
-- natural transformation to the level of 'MSF's.
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
morphGS :: Monad m2
        => (forall c . (a1 -> m1 (b1, c)) -> (a2 -> m2 (b2, c)))
          -- ^ The natural transformation. @mi@, @ai@ and @bi@ for @i = 1, 2@
          --   can be chosen freely, but @c@ must be universally quantified
        -> MSF m1 a1 b1
        -> MSF m2 a2 b2
morphGS morph msf = MSF $ \a2 -> do
  (b2, msf') <- morph (unMSF msf) a2
  return (b2, morphGS morph msf')

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


-- * Switching

-- | Switching applies one 'MSF' until it produces a 'Just' output, and then
-- "turns on" a continuation and runs it.
--
-- A more advanced and comfortable approach to switching is given by Exceptions
-- in 'Control.Monad.Trans.MSF.Except'
switch :: Monad m => MSF m a (b, Maybe c) -> (c -> MSF m a b) -> MSF m a b
switch sf f = MSF $ \a -> do
  ((b, c), sf') <- unMSF sf a
  return (b, maybe (switch sf' f) f c)

-- * Feedback loops

-- | Well-formed looped connection of an output component as a future input.
feedback :: Monad m => c -> MSF m (a, c) (b, c) -> MSF m a b
feedback c sf = MSF $ \a -> do
  ((b', c'), sf') <- unMSF sf (a, c)
  return (b', feedback c' sf')

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
