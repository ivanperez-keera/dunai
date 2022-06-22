-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'MSF's with a 'Reader' monadic layer.
--
-- This module contains functions to work with 'MSF's that include a 'Reader'
-- monadic layer. This includes functions to create new 'MSF's that include an
-- additional layer, and functions to flatten that layer out of the 'MSF`'s
-- transformer stack.
module Control.Monad.Trans.MSF.Reader
    ( module Control.Monad.Trans.Reader
    -- * 'Reader' 'MSF' running and wrapping.
    , readerS
    , runReaderS
    , runReaderS_
    )
  where

-- External imports
import Control.Arrow              (arr, (>>>))
import Control.Monad.Trans.Reader hiding (liftCallCC, liftCatch)

-- Internal imports
import Data.MonadicStreamFunction (MSF, morphGS)

-- * Reader 'MSF' running and wrapping

-- | Build an 'MSF' in the 'Reader' monad from one that takes the reader
-- environment as an extra input. This is the opposite of 'runReaderS'.
readerS :: Monad m => MSF m (r, a) b -> MSF (ReaderT r m) a b
readerS = morphGS $ \f a -> ReaderT $ \r -> f (r, a)

-- | Build an 'MSF' that takes an environment as an extra input from one on the
-- 'Reader' monad. This is the opposite of 'readerS'.
runReaderS :: Monad m => MSF (ReaderT r m) a b -> MSF m (r, a) b
runReaderS = morphGS $ \f (r, a) -> runReaderT (f a) r

-- | Build an 'MSF' /function/ that takes a fixed environment as additional
-- input, from an MSF in the 'Reader' monad.
runReaderS_ :: Monad m => MSF (ReaderT s m) a b -> s -> MSF m a b
runReaderS_ msf s = arr (\a -> (s, a)) >>> runReaderS msf
