module FRP.BearRiver.List where

import           Control.Applicative
import           Control.Arrow                as X
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.MStreamF
import           Data.Functor.Identity
import           Data.Maybe
import           Data.MonadicStreamFunction   as X hiding (iPre, reactimate, switch)
import qualified Data.MonadicStreamFunction   as MSF
import           FRP.Yampa.VectorSpace        as X
import           FRP.BearRiver

newtype ListSF a b = ListSF { listSF :: SF a (b, Bool, [ListSF a b]) }

dlSwitch :: [ListSF a b] -> SF a [b]
dlSwitch' sfs = MStreamF $ \a -> do
  -- results of applying the initial input
  bsfs0 <- mapM ((`unMStreamF` a).listSF) sfs

      -- Gather outputs
  let bs = fmap (\((b,_d,_nfs),_) -> b) bsfs0

  -- Gather new SFs
  -- The initial output of each new sf is discarded!
  nsfs = (\sf -> fst (unMStreamF (listSF sf) a0)) <$> 
         concatMap (\((_b,_d,nfs),_sf) -> nfs) bsfs0

      -- Gather old continuations
      osfs  = map (\(sf,(_b,_d,_nfs)) -> sf) $
                filter (\(_sf,(_b,d,_nfs)) -> not d) bsfs0

      cts   = osfs ++ nsfs
  in (dlSwitch' cts, bs)
