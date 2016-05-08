-- {-# LANGUAGE ExistentialQuantification #-}
module FRP.BearRiver
  (module FRP.BearRiver, module X)
 where
-- This is an implementation of Yampa using our Monadic Stream Processing
-- library. We focus only on core Yampa. We will use this module later to
-- reimplement an example of a Yampa system.
--
-- While we may not introduce all the complexity of Yampa today (all kinds of
-- switches, etc.) our goal is to show that the approach is promising and that
-- there do not seem to exist any obvious limitations.

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

type ClockInfo = Reader DTime

type DTime = Double

type SF = MStreamF ClockInfo

iPre :: a -> SF a a
iPre i = MStreamF $ \i' -> return (i, iPre i')

integral :: VectorSpace a s => SF a a
integral = integralFrom zeroVector

integralFrom :: VectorSpace a s => a -> SF a a
integralFrom n0 = MStreamF $ \n -> do
  dt <- ask
  let acc = n0 ^+^ realToFrac dt *^ n
  return (acc, integralFrom acc)

data Event a = Event a | NoEvent

event :: a -> (b -> a) -> Event b -> a
event _ f (Event x) = f x
event x _ NoEvent   = x

tag :: Event a -> b -> Event b
tag NoEvent   _ = NoEvent
tag (Event _) b = Event b

eventToMaybe (Event x) = Just x
eventToMaybe NoEvent   = Nothing

boolToEvent :: Bool -> Event ()
boolToEvent True  = Event ()
boolToEvent False = NoEvent

edge :: SF Bool (Event ())
edge = arr boolToEvent

switch sf sfC = MSF.switch (sf >>> second (arr eventToMaybe)) sfC

reactimate :: IO a -> (Bool -> IO (DTime, Maybe a)) -> (Bool -> b -> IO Bool) -> SF a b -> IO ()
reactimate senseI sense actuate sf = do
  runMaybeT $ MSF.reactimate $ liftMStreamFTrans (senseSF >>> sfIO) >>> actuateSF
  return ()
 where sfIO       = liftMStreamFPurer (return.runIdentity) (runReaderS sf)

       senseSF    = switch senseFirst senseRest
       senseFirst = liftMStreamF_ senseI >>> (arr $ \x -> ((0, x), Event x))
       senseRest a = liftMStreamF_ (sense True) >>> (arr id *** keepLast a)
       keepLast :: Monad m => a -> MStreamF m (Maybe a) a
       keepLast a = MStreamF $ \ma -> let a' = fromMaybe a ma in return (a', keepLast a')

       -- actuateSF  :: MStreamF IO b ()
       actuateSF  = arr (\x -> (True, x)) >>> liftMStreamF (lift . uncurry actuate) >>> exitIf
