{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types     #-}
-- | Monadic Stream Functions are synchronized stream functions
-- with side effects.

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
-- exports everything. Users should *never* import this module
-- individually, but the main module instead.
module Data.MonadicStreamFunction.Core where

-- External
import Control.Arrow
import Control.Category (Category(..))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Class
import Prelude hiding ((.), id, sum)

-- MSF: Stepwise, side-effectful MSFs without implicit knowledge of time
data MSF m a b = MSF { unMSF :: a -> m (b, MSF m a b) }

instance Monad m => Category (MSF m) where
  id = go
    where go = MSF $ \a -> return (a, go)
  sf2 . sf1 = MSF $ \a -> do
    (b, sf1') <- unMSF sf1 a
    (c, sf2') <- unMSF sf2 b
    let sf' = sf2' . sf1'
    c `seq` return (c, sf')

instance Monad m => Arrow (MSF m) where

  arr f = go
    where go = MSF $ \a -> return (f a, go)

  first sf = MSF $ \(a,c) -> do
    (b, sf') <- unMSF sf a
    b `seq` return ((b, c), first sf')

-- ** Lifts
liftMSF :: Monad m => (a -> m b) -> MSF m a b
liftMSF f = go
 where go = MSF $ \a -> do
              b <- f a
              return (b, go)


liftS :: (Monad m2, MonadBase m1 m2) => (a -> m1 b) -> MSF m2 a b
liftS = liftMSF . (liftBase .)

-- * Monadic lifting from one monad into another

-- ** Purer monads

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

-- | Lifting purer monadic actions (in an arbitrary way)
liftMSFPurer :: (Monad m2, Monad m1) => (forall c . m1 c -> m2 c) -> MSF m1 a b -> MSF m2 a b
liftMSFPurer liftPurer sf = MSF $ \a -> do
  (b, sf') <- liftPurer $ unMSF sf a
  b `seq` return (b, liftMSFPurer liftPurer sf')

-- ** Monad stacks

-- | Lifting inner monadic actions in monad stacks
-- TODO Should be able to express this in terms of MonadBase
liftMSFTrans :: (MonadTrans t, Monad m, Monad (t m)) => MSF m a b -> MSF (t m) a b
liftMSFTrans sf = MSF $ \a -> do
  (b, sf') <- lift $ unMSF sf a
  return (b, liftMSFTrans sf')

-- | Lifting the innermost monadic actions in a monad stacks (generalisation of liftIO)
liftMSFBase :: (Monad m2, MonadBase m1 m2) => MSF m1 a b -> MSF m2 a b
liftMSFBase sf = MSF $ \a -> do
  (b, sf') <- liftBase $ unMSF sf a
  b `seq` return (b, liftMSFBase sf')

-- * MSFs within monadic actions

-- | Extract MSF from a monadic action
performOnFirstSample :: Monad m => m (MSF m a b) -> MSF m a b
performOnFirstSample sfaction = MSF $ \a -> do
  sf <- sfaction
  unMSF sf a

-- ** Delays and signal overwriting

iPre :: Monad m => a -> MSF m a a
iPre firsta = MSF $ \a -> return (firsta, delay a)
-- iPre firsta = feedback firsta $ lift swap
--   where swap (a,b) = (b, a)
-- iPre firsta = next firsta identity

-- FIXME: Remove delay from this module. We should try to make this module
-- small, keeping only primitives.
delay :: Monad m => a -> MSF m a a
delay = iPre

-- ** Switching

switch :: Monad m => MSF m a (b, Maybe c) -> (c -> MSF m a b) -> MSF m a b
switch sf f = MSF $ \a -> do
  ((b, c), sf') <- unMSF sf a
  return (b, maybe (switch sf' f) f c)

-- ** Feedback loops

feedback :: Monad m => c -> MSF m (a, c) (b, c) -> MSF m a b
feedback c sf = MSF $ \a -> do
  ((b', c'), sf') <- unMSF sf (a, c)
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
-- To explore the output progressively, use liftMSF and (>>>), together
-- with some action that consumes/actuates on the output.
--
-- This is called "runSF" in Liu, Cheng, Hudak, "Causal Commutative Arrows and
-- Their Optimization"
embed :: Monad m => MSF m a b -> [a] -> m [b]
embed _  []     = return []
embed sf (a:as) = do
  (b, sf') <- unMSF sf a
  bs       <- embed sf' as
  return (b:bs)

-- | Runs an MSF indefinitely passing a unit-carrying input stream.
reactimate :: Monad m => MSF m () () -> m ()
reactimate sf = do
  (_, sf') <- unMSF sf ()
  reactimate sf'

-- | Runs an MSF indefinitely passing a unit-carrying input stream.
reactimateB :: Monad m => MSF m () Bool -> m ()
reactimateB sf = do
  (b, sf') <- unMSF sf ()
  if b then return () else reactimateB sf'
