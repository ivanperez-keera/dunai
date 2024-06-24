{-# LANGUAGE Arrows     #-}
{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
-- The following warning is disabled so that we do not see warnings due to
-- using ListT on an MSF to implement parallelism with broadcasting.
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#else
{-# OPTIONS_GHC -Wno-deprecations #-}
#endif
{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Copyright  : (c) Ivan Perez, 2019-2022
--              (c) Ivan Perez and Manuel Baerenz, 2016-2018
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Implementation of Yampa using Monadic Stream Processing library.
module FRP.BearRiver
    (module FRP.BearRiver, module X)
  where

-- External imports
import Control.Arrow         as X
import Data.Functor.Identity (Identity (..))
import Data.Maybe            (fromMaybe)
import Data.VectorSpace      as X

-- Internal imports (dunai)
import           Control.Monad.Trans.MSF                 hiding (dSwitch)
import qualified Control.Monad.Trans.MSF                 as MSF
import           Data.MonadicStreamFunction              as X hiding (count,
                                                               embed, iPre,
                                                               next, once,
                                                               reactimate,
                                                               repeatedly,
                                                               switch, trace)
import qualified Data.MonadicStreamFunction              as MSF
import           Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))
import           FRP.BearRiver.Arrow                     as X
import           FRP.BearRiver.Basic                     as X
import           FRP.BearRiver.Conditional               as X
import           FRP.BearRiver.Delays                    as X
import           FRP.BearRiver.Event                     as X
import           FRP.BearRiver.EventS                    as X
import           FRP.BearRiver.Hybrid                    as X
import           FRP.BearRiver.Integration               as X
import           FRP.BearRiver.InternalCore              as X
import           FRP.BearRiver.Random                    as X
import           FRP.BearRiver.Scan                      as X
import           FRP.BearRiver.Simulation                as X
import           FRP.BearRiver.Switches                  as X
import           FRP.BearRiver.Task                      as X
import           FRP.BearRiver.Time                      as X

-- Internal imports (dunai, instances)
import Data.MonadicStreamFunction.Instances.ArrowLoop () -- not needed, just
                                                         -- re-exported

-- ** Relation to other types

-- | Convert an 'Event' into a 'Maybe' value.
--
-- Both types are isomorphic, where a value containing an event is mapped to a
-- 'Just', and 'NoEvent' is mapped to 'Nothing'. There is, however, a semantic
-- difference: a signal carrying a Maybe may change constantly, but, for a
-- signal carrying an 'Event', there should be a bounded frequency such that
-- sampling the signal faster does not render more event occurrences.
eventToMaybe :: Event a -> Maybe a
eventToMaybe = event Nothing Just

-- | Create an event if a 'Bool' is 'True'.
boolToEvent :: Bool -> Event ()
boolToEvent True  = Event ()
boolToEvent False = NoEvent

-- * State keeping combinators

-- ** Loops with guaranteed well-defined feedback

-- | Loop with an initial value for the signal being fed back.
loopPre :: Monad m => c -> SF m (a, c) (b, c) -> SF m a b
loopPre = feedback
