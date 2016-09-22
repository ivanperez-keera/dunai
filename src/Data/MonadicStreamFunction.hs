-- | Monadic Stream Functions are synchronized stream functions
-- with side effects.
module Data.MonadicStreamFunction
  ( module Control.Arrow
  , module Data.MonadicStreamFunction
  , module X
  )
 where

-- External
import Control.Arrow
import Control.Category (Category(..))
import Control.Monad
import Control.Monad.Base
import Data.Monoid
import Prelude hiding ((.), id)

-- Internal (generic)
import Data.VectorSpace
import Data.VectorSpace.Instances()

import Data.MonadicStreamFunction.Core          as X
import Data.MonadicStreamFunction.ArrowChoice   as X
import Data.MonadicStreamFunction.ArrowLoop     as X
import Data.MonadicStreamFunction.ArrowPlus     as X


-- ** Lifts

{-# DEPRECATED insert "Don't use this. liftMSF id instead" #-}
insert :: Monad m => MSF m (m a) a
insert = liftMSF id

arrM_ :: Monad m => m b -> MSF m a b
arrM_ = arrM . const

-- * Monadic lifting from one monad into another

-- ** Monad stacks

(^>>>) :: MonadBase m1 m2 => MSF m1 a b -> MSF m2 b c -> MSF m2 a c
sf1 ^>>> sf2 = liftMSFBase sf1 >>> sf2
{-# INLINE (^>>>) #-}

(>>>^) :: MonadBase m1 m2 => MSF m2 a b -> MSF m1 b c -> MSF m2 a c
sf1 >>>^ sf2 = sf1 >>> liftMSFBase sf2
{-# INLINE (>>>^) #-}

-- ** Delays and signal overwriting

-- See also: 'iPre'

iPost :: Monad m => b -> MSF m a b -> MSF m a b
iPost b sf = MSF $ \_ -> return (b, sf)

next :: Monad m => b -> MSF m a b -> MSF m a b
next b sf = MSF $ \a -> do
  (b', sf') <- unMSF sf a
  return (b, next b' sf')
-- rather, once delay is tested:
-- next b sf = sf >>> delay b


-- * Adding side effects
withSideEffect :: Monad m => (a -> m b) -> MSF m a a
withSideEffect method = (id &&& liftMSF method) >>> arr fst

withSideEffect_ :: Monad m => m b -> MSF m a a
withSideEffect_ method = withSideEffect $ const method

-- * Debugging

traceGeneral :: (Monad m, Show a) => (String -> m ()) -> String -> MSF m a a
traceGeneral method msg =
  withSideEffect (method . (msg ++) . show)

trace :: Show a => String -> MSF IO a a
trace = traceGeneral putStrLn

-- FIXME: This does not seem to be a very good name.  It should be
-- something like traceWith. It also does too much.
pauseOnGeneral :: (Monad m, Show a) => (a -> Bool) -> (String -> m ()) -> String -> MSF m a a
pauseOnGeneral cond method msg = withSideEffect $ \a ->
  when (cond a) $ method $ msg ++ show a

pauseOn :: Show a => (a -> Bool) -> String -> MSF IO a a
pauseOn cond = pauseOnGeneral cond $ \s -> print s >> getLine >> return ()

-- * Tests and examples

mappendS :: (Monoid n, Monad m) => MSF m n n
mappendS = mappendFrom mempty
{-# INLINE mappendS #-}

mappendFrom :: (Monoid n, Monad m) => n -> MSF m n n
mappendFrom n0 = MSF $ \n -> let acc = n0 `mappend` n
                              -- in acc `seq` return (acc, mappendFrom acc)
                              in return (acc, mappendFrom acc)

sumFrom :: (RModule v, Monad m) => v -> MSF m v v
sumFrom v = feedback v (arr (uncurry (^+^) >>> dup))
  where dup x = (x,x)

sumS :: (RModule v, Monad m) => MSF m v v
sumS = sumFrom zeroVector

count :: (Num n, Monad m) => MSF m a n
count = arr (const 1) >>> sumS

unfold :: Monad m => (a -> (b,a)) -> a -> MSF m () b
unfold f a = MSF $ \_ -> let (b,a') = f a in b `seq` return (b, unfold f a')
-- unfold f x = feedback x (arr (snd >>> f))

repeatedly :: Monad m => (a -> a) -> a -> MSF m () a
repeatedly f = repeatedly'
 where repeatedly' a = MSF $ \() -> let a' = f a in a' `seq` return (a, repeatedly' a')
-- repeatedly f x = feedback x (arr (f >>> \x -> (x,x)))

-- FIXME: This should *not* be in this module
mapMSF :: Monad m => MSF m a b -> MSF m [a] [b]
mapMSF = MSF . consume
  where
    consume :: Monad m => MSF m a t -> [a] -> m ([t], MSF m [a] [t])
    consume sf []     = return ([], mapMSF sf)
    consume sf (a:as) = do
      (b, sf')   <- unMSF sf a
      (bs, sf'') <- consume sf' as
      b `seq` return (b:bs, sf'')
