{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}
-- | Monadic Stream Functions are synchronized stream functions
-- with side effects.

-- This module contains the core. Only the core. It should be possible
-- to define every function and type outside this module, except for the
-- instances for ArrowLoop, ArrowChoice, etc., without access to the
-- internal constructor for MStreamF and the function 'unMStreamF'.
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
-- exports everything. Users should *never* import this module
-- individually, but the main module instead.
module Data.MonadicStreamFunction.Core where


-- External
import Control.Applicative
import Control.Arrow
import Control.Category (Category(..))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class
import Prelude hiding ((.), id, sum)

-- MStreamF: Stepwise, side-effectful MStreamFs without implicit knowledge of time
data MStreamF m a b = MStreamF { unMStreamF :: a -> m (b, MStreamF m a b) }

instance Monad m => Category (MStreamF m) where
  id = go
    where go = MStreamF $ \a -> return (a, go)
  sf2 . sf1 = MStreamF $ \a -> do
    (b, sf1') <- unMStreamF sf1 a
    (c, sf2') <- unMStreamF sf2 b
    let sf' = sf2' . sf1'
    c `seq` return (c, sf')

instance Monad m => Arrow (MStreamF m) where

  arr f = go
    where go = MStreamF $ \a -> return (f a, go)

  first sf = MStreamF $ \(a,c) -> do
    (b, sf') <- unMStreamF sf a
    b `seq` return ((b, c), first sf')
    -- This is called the "monadic strength" of m

-- ** Lifts
liftMStreamF :: Monad m => (a -> m b) -> MStreamF m a b
liftMStreamF f = go
 where go = MStreamF $ \a -> do
              b <- f a
              return (b, go)

-- * Monadic lifting from one monad into another

-- ** Purer monads

-- IPerez: There is an alternative signature for liftMStreamPurer that also
-- works, and makes the code simpler:
--
-- liftMStreamFPurer :: Monad m => (m1 (b, MStreamF m1 a b) -> m (b, MStreamF m1 a b)) -> MStreamF m1 a b -> MStreamF m a b
--
-- Then we can express:
--
-- liftMStreamFTrans = liftMStreamFPurer lift
-- liftMStreamFBase  = liftMStreamFPurer liftBase
--
-- We could also define a strict version of liftMStreamFPurer as follows:
--
-- liftMStreamPurer' f = liftMStreamFPurer (f >=> whnfVal)
--   where whnfVal p@(b,_) = b `seq` return p
--
-- and leave liftMStreamFPurer as a lazy version (by default).

-- | Lifting purer monadic actions (in an arbitrary way)
liftMStreamFPurer :: (Monad m2, Monad m1) => (forall c . m1 c -> m2 c) -> MStreamF m1 a b -> MStreamF m2 a b
liftMStreamFPurer liftPurer sf = MStreamF $ \a -> do
  (b, sf') <- liftPurer $ unMStreamF sf a
  b `seq` return (b, liftMStreamFPurer liftPurer sf')

-- ** Monad stacks

-- | Lifting inner monadic actions in monad stacks
-- TODO Should be able to express this in terms of MonadBase
liftMStreamFTrans :: (MonadTrans t, Monad m, Monad (t m)) => MStreamF m a b -> MStreamF (t m) a b
liftMStreamFTrans sf = MStreamF $ \a -> do
  (b, sf') <- lift $ unMStreamF sf a
  return (b, liftMStreamFTrans sf')

-- | Lifting the innest monadic actions in a monad stacks (generalisation of liftIO)
liftMStreamFBase :: (Monad m2, MonadBase m1 m2) => MStreamF m1 a b -> MStreamF m2 a b
liftMStreamFBase sf = MStreamF $ \a -> do
  (b, sf') <- liftBase $ unMStreamF sf a
  b `seq` return (b, liftMStreamFBase sf')

-- * MSFs within monadic actions

-- | Extract MSF from a monadic action
performOnFirstSample :: Monad m => m (MStreamF m a b) -> MStreamF m a b
performOnFirstSample sfaction = MStreamF $ \a -> do
  sf <- sfaction
  unMStreamF sf a

-- ** Delays and signal overwriting

iPre :: Monad m => a -> MStreamF m a a
iPre firsta = MStreamF $ \a -> return (firsta, delay a)
-- iPre firsta = feedback firsta $ lift swap
--   where swap (a,b) = (b, a)
-- iPre firsta = next firsta identity

-- FIXME: Remove delay from this module. We should try to make this module
-- small, keeping only primitives.
delay :: Monad m => a -> MStreamF m a a
delay = iPre

-- ** Switching

switch :: Monad m => MStreamF m a (b, Maybe c) -> (c -> MStreamF m a b) -> MStreamF m a b
switch sf f = MStreamF $ \a -> do
  ((b, c), sf') <- unMStreamF sf a
  return (b, maybe (switch sf' f) f c)

-- ** Feedback loops

feedback :: Monad m => c -> MStreamF m (a, c) (b, c) -> MStreamF m a b
feedback c sf = MStreamF $ \a -> do
  ((b', c'), sf') <- unMStreamF sf (a, c)
  return (b', feedback c' sf')

-- * Reactimating

-- | Apply a monadic stream function to a list.
--
-- Because the result is in a monad, it may be necessary to
-- traverse the whole list to evaluate the value in the results to WHNF.
-- For example, if the monad is the maybe monad, this may not produce anything
-- if the MSF produces Nothing at any point, so the output stream cannot
-- consumed progressively.
--
-- To explore the output progressively, use liftMStreamF and (>>>), together
-- with some action that consumes/actuates on the output.
--
-- This is called "runSF" in Liu, Cheng, Hudak, "Causal Commutative Arrows and
-- Their Optimization"
embed :: Monad m => MStreamF m a b -> [a] -> m [b]
embed _  []     = return []
embed sf (a:as) = do
  (b, sf') <- unMStreamF sf a
  bs       <- embed sf' as
  return (b:bs)

-- | Runs an MSF indefinitely passing a unit-carrying input stream.
reactimate :: Monad m => MStreamF m () () -> m ()
reactimate sf = do
  (_, sf') <- unMStreamF sf ()
  reactimate sf'

-- | Runs an MSF indefinitely passing a unit-carrying input stream.
reactimateB :: Monad m => MStreamF m () Bool -> m ()
reactimateB sf = do
  (b, sf') <- unMStreamF sf ()
  if b then return () else reactimateB sf'
