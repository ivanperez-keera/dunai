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
import Control.Monad.Base
import Data.Monoid
import Prelude hiding ((.), id)

-- Internal

import Data.MonadicStreamFunction.Core          as X
import Data.MonadicStreamFunction.ArrowChoice   as X
import Data.MonadicStreamFunction.ArrowLoop     as X
import Data.MonadicStreamFunction.ArrowPlus     as X
import Data.MonadicStreamFunction.Util          as X


-- ** Lifts

{-# DEPRECATED insert "Don't use this. liftMSF id instead" #-}
insert :: Monad m => MSF m (m a) a
insert = arrM id

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
