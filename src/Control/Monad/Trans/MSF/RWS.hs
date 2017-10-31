module Control.Monad.Trans.MSF.RWS
  ( module Control.Monad.Trans.MSF.RWS
  , module Control.Monad.Trans.RWS.Strict
  ) where

-- External
import Control.Applicative
import Control.Monad.Trans.RWS.Strict
  hiding (liftCallCC, liftCatch) -- Avoid conflicting exports

-- Internal
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction

-- * RWS (Reader-Writer-State) monad

runRWSS :: (Functor m, Monad m, Monoid w)
        => MSF (RWST r w s m) a b
        -> MSF m (r, s, a) (w, s, b)
runRWSS = transS transformInput transformOutput
  where
    transformInput  (_, _, a) = return a
    transformOutput (r, s, _) msfaction = sym <$> runRWST msfaction r s
    sym ((b, msf'), s, w) = ((w, s, b), msf')
