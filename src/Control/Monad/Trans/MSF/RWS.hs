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
import Data.Functor ((<$>))

-- Internal
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction

-- * 'RWS' (Reader-Writer-State) monad

-- | Run the 'RWST' layer by making the state variables explicit.
runRWSS :: (Functor m, Monad m, Monoid w)
        => MSF (RWST r w s m) a b
        -> MSF m (r, s, a) (w, s, b)
runRWSS = transS transformInput transformOutput
  where
    transformInput  (_, _, a) = return a
    transformOutput (r, s, _) msfaction = sym <$> runRWST msfaction r s
    sym ((b, msf'), s, w) = ((w, s, b), msf')

-- | Wrap an 'MSF' with explicit state variables in 'RWST' monad.
rwsS :: (Functor m, Monad m, Monoid w)
     => MSF m (r, s, a) (w, s, b)
     -> MSF (RWST r w s m) a b
rwsS = lifterS wrapRWST
  where
    wrapRWST :: Monad m
             => ((r, s, a) -> m ((w, s, b), c)) -> a -> RWST r w s m (b, c)
    wrapRWST f a = RWST $ \r s -> do
      ((w, s', b), c) <- f (r, s, a)
      return ((b, c), s', w)
