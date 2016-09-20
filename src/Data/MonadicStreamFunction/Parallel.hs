module Data.MonadicStreamFunction.Parallel where

-- External
import Control.Arrow
-- import Control.Parallel
import GHC.Conc

-- Internal
import Data.MonadicStreamFunction

-- IPerez: This should be similar to the following:
-- (msf1 *** msf2) >>> parS
-- where parS             = arr parTuple
--       parTuple p@(a,b) = (a `par` b `pseq` p)
-- Manuel: but we added strictness annotations to first
-- and so (***) might be strict in both arguments and not take
-- full advantage of parallelism.
--
(*|*) :: Monad m => MSF m a b -> MSF m c d -> MSF m (a, c) (b, d)
msf1 *|* msf2 = MSF $ \(a, c) -> do
    (b, msf1') <- unMSF msf1 a
    (d, msf2') <- unMSF msf2 c
    b `par` d `pseq` return ((b, d), msf1' *|* msf2')


(&|&) :: Monad m => MSF m a b -> MSF m a c -> MSF m a (b, c)
msf1 &|& msf2 = arr (\a -> (a, a)) >>> (msf1 *|* msf2)
