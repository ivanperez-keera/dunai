{-# LANGUAGE GADTs  #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- TODO

-- Important question: because this FRP implement uses CPS,
-- it is stateful, and sampling twice in one time period
-- is not necessarily the same as sampling once. This means that
-- tauApp, or next, might not work correctly. It's important to
-- see what is going on there... :(

module FRP.Dunai.LTLFuture where

------------------------------------------------------------------------------
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

-- | Apply a transformation to the leaves (to the SFs)
tPredMap :: Monad m => (MSF m a Bool -> m (MSF m a Bool)) -> TPred m a -> m (TPred m a)
tPredMap f (Prop sf)       = Prop       <$> f sf
tPredMap f (And t1 t2)     = And        <$> (tPredMap f t1) <*> (tPredMap f t2)
tPredMap f (Or t1 t2)      = Or         <$> (tPredMap f t1) <*> (tPredMap f t2)
tPredMap f (Not t1)        = Not        <$> (tPredMap f t1)
tPredMap f (Implies t1 t2) = Implies    <$> (tPredMap f t1) <*> (tPredMap f t2)
tPredMap f (Always t1)     = Always     <$> (tPredMap f t1)
tPredMap f (Eventually t1) = Eventually <$> (tPredMap f t1)
tPredMap f (Next t1)       = Next       <$> (tPredMap f t1)
tPredMap f (Until t1 t2)   = Until      <$> (tPredMap f t1) <*> (tPredMap f t2)

-- * Temporal Evaluation

-- | Evaluates a temporal predicate at time T=0 against a sample stream.
--
-- Returns 'True' if the temporal proposition is currently true.
evalT :: Monad m => TPred (ReaderT DTime m) a -> SignalSampleStream a -> m Bool
evalT (Prop sf)       = \stream -> (myHead . fst)  <$> evalSF sf stream
evalT (And t1 t2)     = \stream -> (&&) <$> (evalT t1 stream)           <*> (evalT t2 stream)
evalT (Or  t1 t2)     = \stream -> (||) <$> (evalT t1 stream)           <*> (evalT t2 stream)
evalT (Not  t1)       = \stream -> not  <$> (evalT t1 stream)
evalT (Implies t1 t2) = \stream -> (||) <$> (not <$> (evalT t1 stream)) <*> (evalT t2 stream)
evalT (Always  t1)    = \stream -> (&&) <$> (evalT t1 stream)           <*> (evalT (Next (Always t1)) stream)
evalT (Eventually t1) = \stream -> (||) <$> (evalT t1 stream)           <*> (evalT (Next (Eventually t1)) stream)
evalT (Until t1 t2)   = \stream -> (||) <$> ((&&) <$> (evalT t1 stream) <*> (evalT (Next (Until t1 t2)) stream)) <*> (evalT t2 stream)
evalT (Next t1)       = \stream -> case stream of
                                    ([])   -> return False  -- This is important.
                                    (a:[]) -> return True   -- This is important. It determines how
                                                            -- eventually, always and next behave at the
                                                            -- end of the stream, which affects that is and isn't
                                                            -- a tautology. It should be reviewed very carefully.
                                    (a1:as) -> tauApp t1 a1 >>= (`evalT` as)

-- Tau-application (transportation to the future)
tauApp :: forall m a . Monad m => TPred (ReaderT DTime m) a -> (DTime, a) -> m (TPred (ReaderT DTime m) a)
tauApp pred (dtime, sample) = runReaderT f dtime
 where
    f :: ReaderT DTime m (TPred (ReaderT DTime m) a)
    f = (tPredMap (\s -> snd <$> unMSF s sample) pred)


myHead :: [a] -> a
myHead [] = error "My head: empty list"
myHead (x:_) = x
