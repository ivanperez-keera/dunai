{-# LANGUAGE CPP #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'MSF's with a 'Writer' monadic layer.
--
-- This module contains functions to work with 'MSF's that include a 'Writer'
-- monadic layer. This includes functions to create new 'MSF's that include an
-- additional layer, and functions to flatten that layer out of the 'MSF`'s
-- transformer stack.
--
-- It is based on the _strict_ writer monad 'Control.Monad.Trans.Writer.Strict',
-- so when combining it with other modules such as @mtl@'s,
-- the strict version has to be included, i.e. 'Control.Monad.Writer.Strict'
-- instead of 'Control.Monad.Writer' or 'Control.Monad.Writer.Lazy'.
module Control.Monad.Trans.MSF.Writer
    ( module Control.Monad.Trans.Writer.Strict
    -- * 'Writer' 'MSF' running and wrapping
    , writerS
    , runWriterS
    )
  where

-- External imports
import Control.Monad.Trans.Writer.Strict hiding (liftCallCC, liftCatch, pass)

#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$>))
import Data.Monoid  (Monoid)
#endif

-- Internal imports
import Data.MonadicStreamFunction (MSF, morphGS)

-- * 'Writer' 'MSF' running and wrapping

-- | Build an 'MSF' in the 'Writer' monad from one that produces the log as an
-- extra output. This is the opposite of 'runWriterS'.
writerS :: (Functor m, Monad m, Monoid w)
        => MSF m a (w, b) -> MSF (WriterT w m) a b
writerS = morphGS $ \f a -> WriterT $ (\((w, b), c) -> ((b, c), w)) <$> f a

-- | Build an 'MSF' that produces the log as an extra output from one on the
-- 'Writer' monad. This is the opposite of 'writerS'.
runWriterS :: (Functor m, Monad m)
           => MSF (WriterT s m) a b -> MSF m a (s, b)
runWriterS = morphGS $ \f a -> (\((b, c), s) -> ((s, b), c))
         <$> runWriterT (f a)
