{-# LANGUAGE Rank2Types          #-}

-- | MSFs with a Reader monadic layer.
--
-- This module contains functions to work with MSFs that include a 'Reader'
-- monadic layer. This includes functions to create new MSFs that include an
-- additional layer, and functions to flatten that layer out of the MSF's
-- transformer stack.
module Control.Monad.Trans.MSF.Reader
  ( module Control.Monad.Trans.Reader
  -- * Reader MSF wrapping/unwrapping.
  , readerS
  , runReaderS
  , runReaderS_
  -- ** Alternative implementation using internal type.
  , readerS'
  , runReaderS'
  -- ** Alternative implementation using generic lifting.
  , runReaderS''
  ) where

-- External
import Control.Monad.Trans.Reader
  hiding (liftCallCC, liftCatch) -- Avoid conflicting exports

-- Internal
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction

-- * Reader MSF wrapping/unwrapping

-- | Build an MSF in the 'Reader' monad from one that takes the reader
-- environment as an extra input. This is the opposite of 'runReaderS'.
readerS :: Monad m => MSF m (s, a) b -> MSF (ReaderT s m) a b
readerS msf = MSF $ \a -> do
  (b, msf') <- ReaderT $ \s -> unMSF msf (s, a)
  return (b, readerS msf')

-- | Build an MSF that takes an environment as an extra input from one on the
-- 'Reader' monad. This is the opposite of 'readerS'.
runReaderS :: Monad m => MSF (ReaderT s m) a b -> MSF m (s, a) b
runReaderS msf = MSF $ \(s,a) -> do
  (b, msf') <- runReaderT (unMSF msf a) s
  return (b, runReaderS msf')


-- | Build an MSF /function/ that takes a fixed environment as additional
-- input, from an MSF in the 'Reader' monad.
--
-- This should be always equal to:
--
-- @
-- runReaderS_ msf s = arr (\a -> (s,a)) >>> runReaderS msf
-- @
--
-- although possibly more efficient.

-- IP: Is runReaderS_ msf s = arr (\a -> (s,a)) >>> runReaderS msf ?
-- MB: Yes, but possibly more efficient.
runReaderS_ :: Monad m => MSF (ReaderT s m) a b -> s -> MSF m a b
runReaderS_ msf s = MSF $ \a -> do
  (b, msf') <- runReaderT (unMSF msf a) s
  return (b, runReaderS_ msf' s)

-- ** Alternative implementation using internal type.

-- TODO: One one should exist, ideally.

-- | Alternative version of 'readerS'.
readerS' :: Monad m => MSF m (s, a) b -> MSF (ReaderT s m) a b
readerS' = lifterS wrapReaderT

-- | Alternative version of 'runReaderS' wrapping/unwrapping functions.
runReaderS' :: Monad m => MSF (ReaderT s m) a b -> MSF m (s, a) b
runReaderS' = lifterS unwrapReaderT

wrapReaderT :: ((s, a) -> m b) -> a -> ReaderT s m b
wrapReaderT g i = ReaderT $ g . flip (,) i

unwrapReaderT :: (a -> ReaderT s m b) -> (s, a) -> m b
unwrapReaderT g i = uncurry (flip runReaderT) $ second g i

-- ** Alternative implementation using generic lifting.

-- | Alternative version of 'runReaderS'.
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


-- Another alternative:
--
-- type ReaderWrapper   s m = Wrapper   (ReaderT s m) m ((,) s) Id
-- type ReaderUnwrapper s m = Unwrapper (ReaderT s m) m ((,) s) Id
--
-- and use the types:
--
-- wrapReaderT   :: ReaderWrapper s m
-- unwrapReaderT :: ReaderUnwrapper s m
