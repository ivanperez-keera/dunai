{-# LANGUAGE Rank2Types          #-}
module Control.Monad.Trans.MSF.Reader
  ( module Control.Monad.Trans.MSF.Reader
  , module Control.Monad.Trans.Reader
  ) where

-- External
import Control.Monad.Trans.Reader
  hiding (liftCallCC, liftCatch) -- Avoid conflicting exports


-- Internal
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction


-- * Reader monad
readerS :: Monad m => MSF m (s, a) b -> MSF (ReaderT s m) a b
readerS msf = MSF $ \a -> do
  (b, msf') <- ReaderT $ \s -> unMSF msf (s, a)
  return (b, readerS msf')

runReaderS :: Monad m => MSF (ReaderT s m) a b -> MSF m (s, a) b
runReaderS msf = MSF $ \(s,a) -> do
  (b, msf') <- runReaderT (unMSF msf a) s
  return (b, runReaderS msf')

-- ** Alternative wrapping/unwrapping MSF combinators using generic lifting

runReaderS' :: Monad m => MSF (ReaderT s m) a b -> MSF m (s, a) b
runReaderS' = lifterS unwrapReaderT


type ReaderWrapper   s m = Wrapper   (ReaderT s m) m ((,) s) Id
type ReaderUnwrapper s m = Unwrapper (ReaderT s m) m ((,) s) Id
-- and use the types:
-- wrapReaderT   :: ReaderWrapper s m
-- unwrapReaderT :: ReaderUnwrapper s m

wrapReaderT :: ((s, a) -> m b) -> a -> ReaderT s m b
wrapReaderT g i = ReaderT $ g . flip (,) i

unwrapReaderT :: (a -> ReaderT s m b) -> (s, a) -> m b
unwrapReaderT g i = uncurry (flip runReaderT) $ second g i

readerS' :: Monad m => MSF m (s, a) b -> MSF (ReaderT s m) a b
readerS' = lifterS wrapReaderT

runReaderS'' :: Monad m => MSF (ReaderT s m) a b -> MSF m (s, a) b
runReaderS'' = transG transformInput transformOutput
  where
    transformInput  (_, a) = return a
    transformOutput (s, _) m1 = do (r, c) <- runReaderT m1 s
                                   return (r, Just c)

{-
readerS'' :: Monad m => MSF m (s, a) b -> MSF (ReaderT s m) a b
readerS'' = transS transformInput transformOutput
  where
    transformInput :: a -> m (s, a)
    transformInput a = (,) <$> asks <*> pure a
    transformOutput _ = lift
-}

-- ** Auxiliary functions related to ReaderT

-- IP: Is runReaderS_ msf s = arr (\a -> (s,a)) >>> runReaderS msf ?
-- MB: Yes, but possibly more efficient.
runReaderS_ :: Monad m => MSF (ReaderT s m) a b -> s -> MSF m a b
runReaderS_ msf s = MSF $ \a -> do
  (b, msf') <- runReaderT (unMSF msf a) s
  return (b, runReaderS_ msf' s)
