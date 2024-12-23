{-# LANGUAGE CPP #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'MSF's with a list monadic layer.
--
-- This module contains functions to work with MSFs that include a 'ListT'
-- monadic layer. MSFs on a list monad may produce multiple outputs and
-- continuations, or none. This enables the possibility for spawning new MSFs,
-- or stopping MSFs, at will.
--
-- A common use case is to be able to dynamically spawn new interactive
-- elements in applications (e.g., a game object that splits in two, or that
-- fires to an enemy).
module Control.Monad.Trans.MSF.List
    ( module Control.Monad.Trans.MSF.List
    , module List
    )
  where

-- External imports
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import           Control.Monad    (sequence)
import           List.Transformer (ListT (ListT, next), Step (..), fold, select)
import qualified List.Transformer as List

-- Internal imports
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- * List monad

-- | Run an 'MSF' in the 'ListT' transformer (i.e., multiple MSFs producing
-- each producing one output), by applying the input stream to each MSF in the
-- list transformer and concatenating the outputs of the MSFs together.
--
-- An MSF in the ListT transformer can spawn into more than one MSF, or none,
-- so the outputs produced at each individual step are not guaranteed to all
-- have the same length.
widthFirst :: (Functor m, Monad m) => MSF (ListT m) a b -> MSF m a [b]
widthFirst msf = widthFirst' [msf]
  where
    widthFirst' msfs = MSF $ \a -> do
      (bs, msfs') <- unzip . concat <$> mapM (toList . flip unMSF a) msfs
      return (bs, widthFirst' msfs')

    toList :: (Functor m, Monad m) => ListT m a -> m [a]
    toList = fmap reverse . fold (flip (:)) [] id

-- | Build an 'MSF' in the 'ListT' transformer by broadcasting the input stream
-- value to each MSF in a given list.
sequenceS :: Monad m => [MSF m a b] -> MSF (ListT m) a b
sequenceS msfs = MSF $ \a -> sequence' $ apply a <$> msfs
  where
    sequence' :: Monad m => [m a] -> ListT m a
    sequence' xs = ListT $ next <$> select =<< sequence xs

    apply :: Monad m => a -> MSF m a b -> m (b, MSF (ListT m) a b)
    apply a msf = do
      (b, msf') <- unMSF msf a
      return (b, sequenceS [msf'])

-- | Apply an 'MSF' to every input.
mapMSF :: Monad m => MSF m a b -> MSF m [a] [b]
mapMSF = MSF . consume
  where
    consume :: Monad m => MSF m a t -> [a] -> m ([t], MSF m [a] [t])
    consume sf []     = return ([], mapMSF sf)
    consume sf (a:as) = do
      (b, sf')   <- unMSF sf a
      (bs, sf'') <- consume sf' as
      b `seq` return (b:bs, sf'')
