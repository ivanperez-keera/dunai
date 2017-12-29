-- | The 'ListT' monad transformer allows for branching effects.
--   For 'MSF's, this means that the continuation can branch in any tick.
--   This opens a (non-exhaustive) lists of possibilities:
--
--   * Several stream functions can run in parallel
--   * A stream function can split up into several ones
--     (e.g. in a simulation, this would correspond to the creation of objects)
--   * A stream function can decide to stop and delete itself
--
--   Some of the most important operations are given by the 'ArrowPlus' instance
--   of 'MSF's, which is usually available whenever 'ListT' is in the
--   transformer stack.
--   For example:
--
--   * @zeroArrow :: MSF (ListT m) a b@ corresponds to an empty list
--     of continuations
--   * @(\<+\>) :: MSF (ListT m) a b -> MSF (ListT m) a b -> MSF (ListT m) a b@
--     runs the two 'MSF's in parallel by appending the continuations.
--
--   Note though that the 'Monad' instance of lists is given by 'concat',
--   i.e. performing 'ListT' effects in subsequent ticks will result
--   in the cartesian products of the individual effects.
--
--   This module uses "'ListT' done right" as provided by the @list-t@ package.

module Control.Monad.Trans.MSF.List
  ( module Control.Monad.Trans.MSF.List
  , module ListT
  ) where

-- base
import Control.Arrow

-- list-t
import ListT

-- Internal
import Data.MonadicStreamFunction


-- * Entering the 'ListT' monad transformer

-- | For each input list element, create a cartesian product of identity arrows.
--
--   For example:
--
-- @
-- >>> let lists = [ [1,2,3], [10], [100, 101], [], [1000] ] :: [[Int]]
-- >>> embed (collect inListT >>> arrM print) lists
-- [1,2,3]
-- [10,10,10]
-- [100,101,100,101,100,101]
-- []
-- []
-- ...
-- @

inListT :: Monad m => MSF (ListT m) [a] a
inListT = arrM fromFoldable

-- | Run several 'MSF's jointly in the 'ListT' monad.
jointly :: Monad m => [MSF m a b] -> MSF (ListT m) a b
jointly = foldl (<+>) zeroArrow . map liftMSFTrans


-- * Leaving the 'ListT' transformer

-- | Broadcasts the @a@ input to all 'MSF's
--   and performs a width-first traversal to collect the results.
collect :: (Functor m, Monad m) => MSF (ListT m) a b -> MSF m a [b]
collect msf = collect' [msf] where
  collect' msfs = MSF $ \a -> do
    (bs, msfs') <- unzip . concat <$> mapM (toList . flip unMSF a) msfs
    return (bs, collect' msfs')

-- * Utilities implemented in terms of 'ListT'

-- | Run several 'MSF's jointly and collect the results.
sequenceS :: Monad m => [MSF m a b] -> MSF m a [b]
sequenceS = collect . jointly
