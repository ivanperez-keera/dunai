{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.MonadicStream where

-- External
import Control.Comonad
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Identity
import Control.Monad.Trans.Reader

-- Internal
import Data.MonadicStreamFunction hiding (MStream)

data MStream m a = MStream { decons :: m (a, MStream m a) }

instance Functor m => Functor (MStream m) where
    fmap f as = MStream $ fStream <$> decons as where
        fStream (a, as') = (f a, fmap f as')
{-
instance Applicative m => Applicative (MStream m) where
    pure a = MStream $ return (a, pure a)
    fs <*> as = MStream $ do
        (f, fs') <- decons fs
        (a, as') <- decons as
        return (f a, fs' <*> as')
-}
readerToMStreamF :: Monad m => MStream (ReaderT a m) b -> MStreamF m a b
readerToMStreamF bs = MStreamF $ \a -> do
    (b, bs') <- runReaderT (decons bs) a
    return (b, readerToMStreamF bs')

mStreamFToReader :: Monad m => MStreamF m a b -> MStream (ReaderT a m) b
mStreamFToReader msf = MStream $ ReaderT $ \a -> do
    (b, msf') <- unMStreamF msf a
    return (b, mStreamFToReader msf')

liftS :: Functor m => m a -> MStream m a
liftS ma = go where
    go = MStream $ (,go) <$> ma

runReaderS :: Monad m => MStream (ReaderT a m) b -> MStream m a -> MStream m b
runReaderS bs as = MStream $ do
    (a, as') <- decons as
    (b, bs') <- runReaderT (decons bs) a
    return (b, runReaderS bs' as')

readerS :: Monad m => MStream m (a -> b) -> MStream (ReaderT a m) b
readerS fs = MStream $ ReaderT $ \a -> do
    (f, fs') <- decons fs
    return (f a, readerS fs')

data Stream a = Stream a (Stream a)

widthFirst :: [MStream [] a] -> Stream [a]
widthFirst asList = let (aList, as') = unzip $ concatMap decons asList
                in Stream aList $ widthFirst as'

functorUnzip :: Functor f => f (a, b) -> (f a, f b)
functorUnzip t = (fmap fst t, fmap snd t)

applicativeZip :: Applicative f => f a -> f b -> f (a, b)
applicativeZip a b = (,) <$> a <*> b

bundle' :: (Monad m, ProductPreserving m) => m (MStream m a) -> Stream (m a)
bundle' as = let (a, as') = funzip $ join $ fmap decons as
             in Stream a $ bundle' as'

bundle :: (Monad m, ProductPreserving m) => MStream m a -> Stream (m a)
bundle = bundle' . return

strange :: (IO Integer, IO Integer)
strange = functorUnzip $ do
    i <- readLn
    return (i, i)

class Functor f => ProductPreserving f where
    funzip :: f (a, b) -> (f a, f b)
    funzip t = (fmap fst t, fmap snd t)
    fzip :: f a -> f b -> f (a, b)
    -- Must satisfy:
    -- funzip $ fzip as bs = (as, bs)
    -- uncurry fzip . funzip = id

instance ProductPreserving [] where
    funzip = unzip
    fzip = zip


class MonadTrans t => ProductPreservingT t where
    tunzip :: t m (a, b) -> m (t m a, t m b)
    tzip :: t m a -> t m b -> t m (a, b)
    -- Must satisfy:
    -- tunzip $ tzip as bs = (as, bs)
    -- uncurry tzip . tunzip = id

{-
bundleT :: (Monad m, MonadTrans t) => (t m (MStream (t m) a)) -> MStream m (t m a)
bundleT as = MStream $ do
    (a, as') <- _ $ join $ fmap decons as
    return (a, bundleT as')
-}

runTS :: (Monad m1, Monad m2) => (forall c . m1 (a1, c) -> m2 (a2, c)) -> MStream m1 a1 -> MStream m2 a2
runTS trans a1s = MStream $ do
    (a2, a1s') <- trans $ decons a1s
    return (a2, runTS trans a1s')

{-
instance Comonad (MStream Identity) where
    extract = fst . runIdentity . decons
    duplicate as = MStream $
        let (_, as') = runIdentity $ decons as
        in return (as, duplicate as')
-}

data MStream' m a = MStream' { headS :: a, tailS :: MStream m a } deriving Functor
tick :: Monad m => MStream m a -> m (MStream' m a)
tick as = do
    (a, as') <- decons as
    return $ MStream' a as'

joinS :: Monad m => m (MStream m a) -> MStream m a
joinS as = MStream $ do
    MStream asm <- as
    asm
{-
instance Monad m => Comonad (MStream' m) where
    extract = headS
    duplicate as = MStream' as $ joinS $ do
        as' <- tick $ tailS as
        return $ MStream $ return (as', _)
-}
data MStream'' m a = MStream'' { head'' :: a, tail'' :: m (MStream'' m a)} deriving Functor

instance Monad m => Comonad (MStream'' m) where
    extract = head''
    duplicate as = MStream'' as $ do
        as' <- tail'' as
        return $ duplicate as'
