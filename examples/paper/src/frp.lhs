\begin{code}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Arrows #-}
import Data.MonadicStreamFunction
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import "bearriver" FRP.Yampa (SF, integral, DTime, Time, edge, tag, Event(..))

fallingMass :: Double -> Double -> SF () Double
fallingMass p0 v0 = arr (const (-9.8))
   >>> integral >>> arr (+ v0)
   >>> integral >>> arr (+ p0)

fallingMass' :: Double -> Double -> MSF (CCDGameMonad Identity) () (Double, Double)
fallingMass' p0 v0 = proc () -> do
   v <- (v0 +) ^<< integral -< -9.8
   p <- (p0 +) ^<< integral -< v
   returnA -< (p, v)

addPair      :: Num a => (a,a) -> (a,a) -> (a,a)
addPair      = undefined
getMousePos  :: IO (Double, Double)
getMousePos  = undefined
time         :: Signal Time
time         = undefined

gravity      :: Double
gravity      = -9.8

liftLM :: Monad m0
       => (forall c. ReaderT DTime m0 c -> ReaderT DTime m0 c)
       -> MSF (ReaderT DTime m0) a b
       -> MSF (ReaderT DTime m0) a b
liftLM = liftMSFPurer
\end{code}


Using lifting MSF combinators we can embed SFs running on one clock
inside others running on different clocks or time domains. For
instance, the following runs MSFs at different speeds:

\begin{code}
game = twiceAsFast fallingBall &&& fallingBall

twiceAsFast  ::  Monad m
             =>  MSF (ReaderT DTime m) a b
             ->  MSF (ReaderT DTime m) a b
twiceAsFast = liftLM (withReaderT (*2))

fallingBall = fallingMass 100 10  -- defined in section 2
\end{code}
% withReaderT :: (r' -> r) -> ReaderT r' a -> ReaderT r a

A useful variation would be to sample an MSF with a fixed sampling period,
regardless of the external clock.

The function |twiceAsFast| above runs both clocks with the same precision: both
``tick'' in synchrony, even if one advances twice as much.  Using the low-level
API of our library, we can make one clock actually tick twice as many times per
sampling period:

% Renamed to avoid name clash
\begin{code}
twiceAsFast' msf = MSF $ \a -> do
  dt <- ask
  (_,msf1) <- runReaderT (unMSF msf   a) (dt/2)
  (b,msf2) <- runReaderT (unMSF msf1  a) (dt/2)
  return (b, twiceAsFast' msf2)
\end{code}

The introduction of subsampling mechanisms like the latter needs to be
addressed with care. Arbitrarily fine subsampling can lead to inherent memory
leaks.

\paragraph{Continuous Collision Detection}

Physics simulations in Yampa are implemented by looking at overlaps
between objects, at the sampling time. This leads
\emph{tunnelling} or \emph{bullet-through-paper} effects, in which
moving objects can pass through other objects if the simulation is not
executed at a time when they actually overlap.

With MSFs we can implement some forms of Continuous Collision Detection
(CCD), by letting signal functions tell the top level simulation when
the next sampling should occur.  The top-level |reactimate| can then
decide whether the real application time delta should be used for the
simulation, or whether several steps and higher precision are needed for
a particular step, as seen before.

We implement a form of CCD with a Writer monad. We use the monoid of
time deltas with the minimum and infinity as identity:

\begin{code}
data FutureTime  =  AtTime DTime |  Infinity
  deriving (Eq, Ord)

instance Monoid FutureTime where
  mempty   = Infinity
  mappend  = min
\end{code}

Signal Functions that try to change the |Writer| state with a requested
sampling time will only do so when such time is smaller than one currently
in the writer state. At the end of each simulation iteration, the log will
contain the closest suggested sampling time.

Using this approach, we can define a bouncing ball that never goes below the
floor. We do so by calculating the expected time of impact (|nextT|) and by
recording that time in the Writer context (|liftS . lift . tell|).  For
clarity, we use Paterson's arrow
notation~\cite{2001:paterson:arrownotation}:

\begin{code}
type CCDGameMonad m =
  ReaderT DTime (WriterT FutureTime m)

bouncingBall :: Double -> Double
             -> MSF (CCDGameMonad Identity) () (Double, Double)
bouncingBall p0 v0 = switch
  (proc () ->  do
    (p,v)    <-  fallingMass' p0 v0  -< ()
    bounce   <-  edge               -< (p <= 0 && v < 0)

    let nextT =  if v < 0  then  sqrt (2*p / gravity)  -- Down
                           else  2 * v0 / gravity      -- Up

    arrM (lift . tell) -< AtTime nextT -- Put "next" time

    returnA  -<  ((p,v), eventToMaybe (bounce `tag` (p,v))))
  (\(p,v) ->  bouncingBall p (-v))

eventToMaybe :: Event a -> Maybe a
eventToMaybe (Event x) = Just x
eventToMaybe NoEvent   = Nothing
\end{code}



\subsection{Classic FRP}
\label{sec:classicfrp}

Classic FRP lets users define time-varying values, or \emph{signals}, using
combinators and applying functions to other signals. Signals may represent
external information sources (e.g. mouse position).

With a |Reader| monad with time deltas in the environment, like in Arrowized
FRP, |Stream|s can model FRP Signals. To allow for external sources, we nest IO
with the monad, obtaining:

\begin{code}
type Signal a = MStream (ReaderT DTime IO) a
\end{code}

A simple simulation of a ball moving around the mouse position could be written
in Classic FRP style as follows:

\begin{code}
ballInCirclesAroundMouse :: Signal (Double, Double)
ballInCirclesAroundMouse =
  addPair <$> mousePos <*> ballInCircles

ballInCircles :: Signal (Double, Double)
ballInCircles = (\x -> (rad * cos x, rad * sin x)) <$> time
  where rad = 45 -- radius in pixels

mousePos :: Signal (Double, Double)
mousePos = liftS (\() -> getMousePos)

\end{code}
% -- Predefined
% addPair      :: Num a => (a,a) -> (a,a) -> (a,a)
% getMousePos  :: IO (Double, Double)
% time         :: Signal Time

With non-commutative monads like IO, additional measures must be taken
to ensure referential transparency at the same conceptual time. If
several signals depend on, for example, |mousePos|, the mouse position
might be sampled twice, with different results at the same conceptual
time. This can be addressed with a monad that caches results and enables
garbage collection~\cite{2010:patai:efficient}.
