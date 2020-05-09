{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FRP.Dunai.LTLFuture
    ( TPred(..)
    , tPredMap
    , evalT
    )
  where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif

import Control.Monad.Trans.MSF.Reader
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (unMSF)
import FRP.Dunai.Stream

-- * Temporal Logics based on SFs

-- | Type representing future-time linear temporal logic with until and next.
data TPred m a where
  Prop       :: MSF m a Bool -> TPred m a
  And        :: TPred m a -> TPred m a -> TPred m a
  Or         :: TPred m a -> TPred m a -> TPred m a
  Not        :: TPred m a -> TPred m a
  Implies    :: TPred m a -> TPred m a -> TPred m a
  Always     :: TPred m a -> TPred m a
  Eventually :: TPred m a -> TPred m a
  Next       :: TPred m a -> TPred m a
  Until      :: TPred m a -> TPred m a -> TPred m a

-- | Apply a transformation to the leaves of a temporal predicate (to the SFs).
tPredMap :: (Functor m, Applicative m, Monad m)
         => (MSF m a Bool -> m (MSF m a Bool))  -- ^ Transformation to apply
         -> TPred m a                           -- ^ Temporal predicate
         -> m (TPred m a)
tPredMap f (Prop sf)       = Prop       <$> f sf
tPredMap f (And t1 t2)     = And        <$> tPredMap f t1 <*> tPredMap f t2
tPredMap f (Or t1 t2)      = Or         <$> tPredMap f t1 <*> tPredMap f t2
tPredMap f (Not t1)        = Not        <$> tPredMap f t1
tPredMap f (Implies t1 t2) = Implies    <$> tPredMap f t1 <*> tPredMap f t2
tPredMap f (Always t1)     = Always     <$> tPredMap f t1
tPredMap f (Eventually t1) = Eventually <$> tPredMap f t1
tPredMap f (Next t1)       = Next       <$> tPredMap f t1
tPredMap f (Until t1 t2)   = Until      <$> tPredMap f t1 <*> tPredMap f t2

-- * Temporal Evaluation

-- | Evaluates a temporal predicate at time T=0 against a sample stream.
--
-- Returns 'True' if the temporal proposition is currently true.
evalT :: (Functor m, Applicative m, Monad m)
      => TPred (ReaderT DTime m) a -> SignalSampleStream a -> m Bool
evalT (Prop sf)       [] = return False
evalT (And t1 t2)     [] = (&&) <$> evalT t1 [] <*> evalT t2 []
evalT (Or  t1 t2)     [] = (||) <$> evalT t1 [] <*> evalT t2 []
evalT (Not t1)        [] = not  <$> evalT t1 []
evalT (Implies t1 t2) [] = (||) <$> (not <$> evalT t1 []) <*> evalT t2 []
evalT (Always t1)     [] = return True
evalT (Eventually t1) [] = return False
evalT (Next t1)       [] = return False
evalT (Until t1 t2)   [] = (||) <$> evalT t1 [] <*> evalT t2 []
evalT op              (x:xs) = do
  (r, op') <- stepF op x
  case (r, xs) of
    (Def x,    _) -> return x
    (SoFar x, []) -> return x
    (SoFar x, xs) -> evalT op' xs

-- ** Multi-valued temporal evaluation

-- | Multi-valued logic result
data MultiRes
    = Def Bool    -- ^ Definite value known
    | SoFar Bool  -- ^ Value so far, but could change

-- | Multi-valued implementation of @and@.
andM :: MultiRes -> MultiRes -> MultiRes
andM (Def False)   _             = Def False
andM _             (Def False)   = Def False
andM (Def True)    x             = x
andM x             (Def True)    = x
andM (SoFar False) (SoFar x)     = SoFar False
andM (SoFar x)     (SoFar False) = SoFar False
andM (SoFar True)  (SoFar x)     = SoFar x
andM (SoFar x)     (SoFar True)  = SoFar x

-- | Multi-valued implementation of @or@.
orM :: MultiRes -> MultiRes -> MultiRes
orM (Def False)   x             = x
orM _             (Def False)   = Def False
orM (Def True)    x             = x
orM x             (Def True)    = x
orM (SoFar False) (SoFar x)     = SoFar False
orM (SoFar x)     (SoFar False) = SoFar False
orM (SoFar True)  (SoFar x)     = SoFar x
orM (SoFar x)     (SoFar True)  = SoFar x

-- | Perform one step of evaluation of a temporal predicate.
stepF :: Monad m
      => TPred (ReaderT DTime m) a
      -> (DTime, a)
      -> m (MultiRes, TPred (ReaderT DTime m) a)

stepF (Prop sf) x  = do
  (b, sf') <- unMSF (runReaderS sf) x
  return (Def b, Prop (readerS sf'))

stepF (Always sf) x = do
  (b, sf') <- stepF sf x
  case b of
    Def True    -> pure (SoFar True, Always sf')
    Def False   -> pure (Def False, Always sf')
    SoFar True  -> pure (SoFar True, Always sf')
    SoFar False -> pure (SoFar False, Always sf')

stepF (Eventually sf) x = do
  (b, sf') <- stepF sf x
  case b of
    Def   True  -> pure (SoFar True,  Always sf')
    Def   False -> pure (SoFar False, Always sf')
    SoFar True  -> pure (SoFar True,  Always sf')
    SoFar False -> pure (SoFar False, Always sf')

stepF (Not sf) x = do
  (b, sf') <- stepF sf x
  case b of
    Def x   -> pure (Def (not x), Not sf')
    SoFar x -> pure (SoFar (not x), Not sf')

stepF (And sf1 sf2) x = do
  (b1, sf1') <- stepF sf1 x
  (b2, sf2') <- stepF sf2 x
  let r = andM b1 b2
  pure (r, And sf1' sf2')

stepF (Or sf1 sf2) x = do
  (b1, sf1') <- stepF sf1 x
  (b2, sf2') <- stepF sf2 x
  let r = orM b1 b2
  pure (r, Or sf1' sf2')

stepF (Implies sf1 sf2) x =
  stepF (Not sf1 `Or` sf2) x
