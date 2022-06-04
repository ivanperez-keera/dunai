{-# LANGUAGE CPP #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
module Control.Monad.Trans.MSF.List
  ( module Control.Monad.Trans.MSF.List
  , module Control.Monad.Trans.List
  ) where

-- External imports
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Monad.Trans.List hiding (liftCallCC, liftCatch)

-- Internal imports
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- * List monad

widthFirst :: (Functor m, Monad m) => MSF (ListT m) a b -> MSF m a [b]
widthFirst msf = widthFirst' [msf]
  where
    widthFirst' msfs = MSF $ \a -> do
        (bs, msfs') <- unzip . concat <$> mapM (runListT . flip unMSF a) msfs
        return (bs, widthFirst' msfs')

sequenceS :: Monad m => [MSF m a b] -> MSF (ListT m) a b
sequenceS msfs = MSF $ \a -> ListT $ sequence $ apply a <$> msfs
  where
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
