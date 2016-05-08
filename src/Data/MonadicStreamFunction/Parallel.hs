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
(*|*) :: Monad m => MStreamF m a b -> MStreamF m c d -> MStreamF m (a, c) (b, d)
msf1 *|* msf2 = MStreamF $ \(a, c) -> do
    (b, msf1') <- unMStreamF msf1 a
    (d, msf2') <- unMStreamF msf2 c
    b `par` d `pseq` return ((b, d), msf1' *|* msf2')


(&|&) :: Monad m => MStreamF m a b -> MStreamF m a c -> MStreamF m a (b, c)
msf1 &|& msf2 = arr (\a -> (a, a)) >>> (msf1 *|* msf2)
