-- | This module combines the wrapping and running functions
--   for the 'Reader', 'Writer' and 'State' monad layers in a single layer.
--
-- It is based on the _strict_ 'RWS' monad 'Control.Monad.Trans.RWS.Strict',
-- so when combining it with other modules such as @mtl@'s,
-- the strict version has to be included, i.e. 'Control.Monad.RWS.Strict'
-- instead of 'Control.Monad.RWS' or 'Control.Monad.RWS.Lazy'.
module Control.Monad.Trans.MSF.RWS
  ( module Control.Monad.Trans.MSF.RWS
  , module Control.Monad.Trans.RWS.Strict
  ) where

-- External
import Control.Monad.Trans.RWS.Strict
  hiding (liftCallCC, liftCatch) -- Avoid conflicting exports
import Data.Monoid
import Data.Functor ((<$>))

-- Internal
import Data.MonadicStreamFunction

-- * 'RWS' (Reader-Writer-State) monad


-- | Wrap an 'MSF' with explicit state variables in 'RWST' monad.
rwsS :: (Functor m, Monad m, Monoid w)
     => MSF m (r, s, a) (w, s, b)
     -> MSF (RWST r w s m) a b
rwsS = morphGS $ \f a -> RWST $ \r s -> (\((w, s', b), c) -> ((b, c), s', w))
   <$> f (r, s, a)

-- | Run the 'RWST' layer by making the state variables explicit.
runRWSS :: (Functor m, Monad m, Monoid w)
        => MSF (RWST r w s m) a b
        -> MSF m (r, s, a) (w, s, b)
runRWSS = morphGS $ \f (r, s, a) -> (\((b, c), s', w) -> ((w, s', b), c))
      <$> runRWST (f a) r s
