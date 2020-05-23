\begin{code}
import Data.MonadicStreamFunction
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
\end{code}
% import "bearriver" FRP.Yampa (SF, integral, DTime, Time, edge, tag, Event(..))

\section{Monads, Modularity and Control in MSFs}
\label{sec:monads}

This section motivates and explores the use of different monads with Monadic
Stream Functions. Monads like |Reader| and |State| help modularise and
increase the expressiveness of programs. Others like |Maybe|, |Exception| and
the list monad give rise to control combinators for termination, higher order
and parallelism.

\paragraph{Temporal Execution Functions}


\subsection{Reader}
\label{sec:readermonad}

We now want to make our example MSFs parametric on the player positions. One
option is to pass the player position as an argument, as in |ballToRight ::
Monad m => Int -> MSF m () Ball|.  However, this complicates the implementation
of every MSF that uses |ballToRight|, which needs to manually pass those
settings down the reactive network.
We can define such a parametrisation in a modular way using a Reader monad with the game
preferences in an environment (we use |ReaderT| for forward compatibility):
\begin{code}
type GameEnv = ReaderT GameSettings

data GameSettings = GameSettings
  {  leftPlayerPos   :: Int
  ,  rightPlayerPos  :: Int
  }
\end{code}

We rewrite the game to pass this environment in the context:

\begin{code}

ballToRight  ::   Monad m => MSF (GameEnv m) () Ball
ballToRight  =
  count >>> liftS (\n -> (n +) <$> asks leftPlayerPos)

hitRight    ::  Monad m => MSF (GameEnv m) Ball Bool
hitRight    =   liftS (\i -> (i >=) <$> asks rightPlayerPos)
\end{code}

To run a game with a fixed environment we could use \linebreak |runReaderT :: ReaderT r m
a -> r -> m a| as before. We test the expression
|testMSF = ballToRight >>> (arr id &&& hitRight)| with different settings as
follows:

% \begin{code}
% > runReaderT  (embed testMSF (repeat 5 ()))
%               (GameSettings 0 3)
%
% [(1, False), (2, False), (3, True), (4, True), (5, True)]
%
% > runReaderT  (embed testMSF (repeat 5 ()))
%               (GameSettings 0 2)
% [(1, False), (2, True), (3, True), (4, True), (5, True)]
% \end{code}

This execution method, however, is outside the invocation of |embed|, so we
cannot make the game settings vary during runtime. To keep the |ReaderT| layer
local to an MSF, we define a \emph{temporal execution function}
analogous to |runReaderT| (implemented using an unwrapping mechanism presented
in Section~\ref{sec:lifting}):
\begin{code}
runReaderS  :: MSF (ReaderT r  m)  a b
            -> r
            -> MSF m   a b
\end{code}

Now we can run two games in parallel with different settings:

% \begin{code}
% > embed  (    runReaderS testMSF  (GameSettings 0 3)
%          &&&  runReaderS testMSF  (GameSettings 0 2))
%          (repeat 5 ()))
% [ ((1, False),  (1, False)),  ((2, False),  (2, False))
% , ((3, False),  (3, True)) ,  ((4, True),   (4, True))
% , ((5, True),   (5, True)) ]
% \end{code}

We could run the MSF obtaining a new Reader environment from the input signal
at every iteration, giving us |runReaderS :: MSF (ReaderT r  m)  a b
-> MSF m (r, a) b|. In Section~\ref{sec:lifting} we will see that both
definitions follow from the type of the run function of the Reader monad, and
there is a systematic way of defining various temporal monadic execution
functions.

\subsection{Writer}
We can use a similar approach to introduce monads like |Writer| or |State|, for
instance, to log debug messages from MSFs.

We first extend our environment with a |WriterT| wrapper:

\begin{code}
type GameEnv'' m =
  WriterT [String] (ReaderT GameSettings m)
\end{code}

We now modify |ballToRight| to print a message when the position is past the
right player (indicating a goal):
\begin{code}
ballToRight      :: Monad m => MSF (GameEnv'' m) () Ball
ballToRight      =
  count >>> liftS addLeftPlayerPos >>> liftS checkHitR

  where  checkHitR    :: n -> GameEnv'' m Int
         checkHitR n  = do
           rp <- asks rightPlayerPos
           when (rp > n) $ tell [ "Ball at " ++ show n ]
\end{code}

Notice that we have changed the monad and |ballToRight|, but the rest of
the game remains unchanged. Having used the transformer |ReaderT| instead of |Reader|
in the previous step now pays off in the form of added modularity.

Like with the reader monad, we may be interested in consuming the context
(for instance, to print accumulated messages and empty the log). We provide the
\emph{temporal execution function}:

% \begin{code}
% runWriterS  :: Monad m
%             => MSF (WriterT r  m)  a b
%             -> MSF m   a (b, r)
% \end{code}
%
% We can test this combinator as follows:
% \begin{code}
% > embed  (runWriterS
%            (runReaderS testMSF (GameSettings 0 3)))
%          (repeat 5 ()))
%
% [((1, False), [])  ,((2, False), []) ,((3, True),  [])
% ,((4, True),  ["Ball at 4"]) ,((5, True),  ["Ball at 5"])]
% \end{code}

Similarly we could have used a |State| monad to define configurable game
settings (for instance, settings that can be adjusted using an options menu,
but remain immutable during a game run).

\subsection{Exceptions and Control Flow}
\label{sec:switch}

MSFs can use different monads to define control structures. One common
construct is \emph{switching}, that is, applying a transformation until a
certain time, and then applying a different transformation.

We can implement an equivalent construct using monads like
|Either| or |Maybe|. We could define a potentially-terminating MSF as an
MSF in a |MaybeT m| monad.  Following the same pattern as before, the
associated execution function would have type:
\begin{code}
runMaybeS  ::  Monad m
           =>  MSF (MaybeT m)  a b
           ->  MSF m           a (Maybe b)
\end{code}

Our evaluation function |step|, for this monad, would have
type |MSF Maybe a b -> a -> Maybe (b, MSF Maybe a b)| indicating
that it may produce \emph{no continuation}.
|runMaybeS| outputs |Nothing| continuously once the internal |MSF|
produces no result. ``Recovering'' from failure requires
an additional continuation:

%\begin{code}
%catchM  ::  Monad m
%        =>  MSF  (MaybeT  m)  a b
%        ->  MSF  m a b
%        ->  MSF  m a b
%\end{code}

% catchM msf1 msf2 = MSF $ \a -> do
%     cont <- runMaybeT $ unMSF msf1 a
%     case cont of
%       Just (b, msf1')  -> return (b, catchM msf1' msf2)
%       Nothing          -> unMSF msf2 a

We can now make the ball bounce when it hits the right player:
\begin{code}
ballBounceOnce  ::  MSF (GameEnv m) () Ball
ballBounceOnce  =   ballUntilHitRight `catchM` ballLeft

ballUntilRight  ::  MSF (MaybeT (GameEnv m)) () Ball
ballUntilRight  =   liftST (ballToRight
                    >>> (arr id &&& hitRight))
                    >>> liftS filterHit
  where
    filterHit (b, c) = MaybeT $ return $
      if c then Nothing else Just b
\end{code}
The utility function |liftST| is defined in Section~\ref{sec:lifting}.

We define |ballUntilLeft| analogously and complete the game:
\begin{code}
game ::  Monad m => MSF m () Ball
game =   ballUntilRight `catchM` ballUntilLeft `catchM` game
\end{code}
Let us interpret the game by inserting a list as input stream.  The
output shows the ball position going up and bouncing back between $10$
(the right player's position) and $0$ (the left player's position).

% \begin{code}
% > embed game $ replicate 23 ()
% [1,2,3,4,5,6,7,8,9,10,9,8,7,6,5,4,3,2,1,0,1,2,3]
% \end{code}

The implementation of switching in our libary is based on a more general
monad |ExceptT c m|, but the key idea is the same.

\subsection{Creating and Destroying Objects with |ListT|}

In games it is often necessary to create and destroy ``objects''. For example,
a gun may fire a bullet or a target vanish when hit. Such dynamicity requires
specific combinators in other reactive frameworks. In ours, the list monad
provides the sought-after behaviour.

A stream function over the list monad can produce zero, one or several output
values and continuations. How we continue depends on our interpretation of that
list. If we explore all continuations simultaneously, we will be implementing parallel
broadcasting.

To produce multiple outputs we provide |zeroA|, which produces no
outputs or continuations, and |<+>|, which concatenates lists produced by
two MSFs in a list monad. (In our library these are available for any
instance of |Alternative| and |MonadPlus|.)

% \begin{code}
% zeroA  :: Monad m  => MSF (ListT m) a b
% (<+>)  :: Monad m  => MSF (ListT m) a b
%                    -> MSF (ListT m) a b
%                    -> MSF (ListT m) a b
% \end{code}

We now change the game logic such that, each time the ball starts moving
left, it splits into two balls (note the use of |(<+>)|):
\begin{code}
type GameEnv' m = ReaderT GameSettings (ListT m)

ballLeft   ::  Monad m => MSF (GameEnv' m) () Ball
ballLeft   =   singleBallLeft <+> singleBallLeft
  where
    singleBallLeft =
      count >>>
        liftS (\n -> (\p -> p - n) <$> asks rightPlayerPos)
\end{code}

We can escape the list monad or the |ListT| transformer by collecting the
outputs from all continuations into a list:
\begin{code}
runListS :: Monad m  =>  MSF (ListT   m)  a  b
                     ->  MSF          m   a  [b]
\end{code}
% collect msf = collect' [msf]
%   where
%     collect' msfs = MSF $ \a -> do
%         results      <- map (runListS . flip unMSF a) msfs)
%         flatResults  <- concat  <$> sequence results
%         (bs, msfs')  <- unzip   <$> flatResults
%         return (bs, widthFirst' msfs')

Our approach proves to be very modular, and we only need to modify our
top function slightly to extract the list effect:
\begin{code}
mainMSF :: MSF IO () ()
mainMSF =
  runListS  (    runReaderS game (GameSettings 20 17)
            &&&  runReaderS game (GameSettings 10  4))
  >>> liftS print
\end{code}
Note that the |ReaderT| layer is inside the |ListT| layer, and therefore
both games are duplicated when either ball starts moving left. Running
the above MSF prints the following output (presented in two columns for reasons
of space):
% \begin{code}
% [(18,5)]
% [(19,6)]
% [(20,7)]
% [(19,8),(19,8)]
% [(18,9),(18,9)]
% [(17,10),(17,10)]
% [(18,9),(18,9),(18,9),(18,9)]
% ...
% \end{code}

% \begin{minipage}[t]{0.4\columnwidth}
% \begin{code}
% [(18,5)]
% [(19,6)]
% [(20,7)]
% [(19,8),(19,8)]
% \end{code}
% \end{minipage}
% \begin{minipage}[t]{0.5\columnwidth}
% \begin{code}
% [(18,9),(18,9)]
% [(17,10),(17,10)]
% [(18,9),(18,9),(18,9),(18,9)]
% ...
% \end{code}
% \hfill
% \end{minipage}

The standard implementation of |ListT| is only valid for commutative
monads. Alternative implementations exist, but the discussion is beyond
the scope of this article.

\subsection{State}

Keeping type signatures parametric, like in the stream function |ballLeft ::
Monad m => MSF (GameEnv m) () Ball|, renders more reusable definitions, since
we can stack more monads on top.  For example, it is easy to introduce a global
state using the |State| monad, with a counter of the number of rounds played.
The counter can be increased with:
\begin{code}
incOneRound  ::  Monad m => StateT Integer m ()
incOneRound  =   modify (+1)
\end{code}
\noindent which we use in the game, accounting for the side effect in the
type:
\begin{code}
game  :: Monad m
      => MSF (GameEnv (StateT Integer m)) () Ball
game  =     ballToRight  `untilM` hitRight
  `catchM`  ballToLeft   `untilM` hitLeft
  `catchM`  (lift incOneRound `andThen` game)
\end{code}

The function |andThen :: Monad m => m () -> MSF m a b| | -> MSF m a b|
\todo{MB}{|andThen| different than in the library. A better name?} performs the
monadic action in the first argument once and immediately carries on processing
the MSF in the second argument.  The function |lift :: (MonadTrans t, Monad m)
=> m a -> t m a| from the transformers package lifts the state modification
into the |GameEnv| monad.

To run this reactive program we have to pass the initial state,
in a similar way to |runStateT :: StateT s m a -> s -> m (a, s)|,
which passes an initial state to a monadic state transformation and
extracts the computed value and the final state.
The corresponding function for streams has type:
\begin{code}
runStateS  :: Monad m  =>  MSF (StateT s  m)  a      b
           -> s        ->  MSF            m   a (s,  b)
\end{code}
\noindent
Using this function in the main loop is a simple change:
\begin{code}
mainMSF :: MSF IO () ()
mainMSF = runStateS parallelGame 0 >>> liftS print
 where
   parallelGame  =    runReaderS game (GameSettings 20 17)
                 &&&  runReaderS game (GameSettings 10  4)
\end{code}

% The output of running this MSF (presented in two columns) is:
%
% \vspace{-0.3cm}
% \begin{minipage}[t]{0.5\columnwidth}
% \begin{code}
% (0,(18,5))
% (0,(19,6))
% (0,(20,7))
% (0,(19,8))
% (0,(18,9))
% (0,(17,10))
% (1,(18,9))
% \end{code}
% \end{minipage}
% \begin{minipage}[t]{0.5\columnwidth}
% \begin{code}
% (1,(19,8))
% (1,(20,7))
% (1,(19,6))
% (1,(18,5))
% (1,(17,4))
% (3,(18,5))
% ...
% \end{code}
% \hfill
% \end{minipage}
% \vspace{-0.4cm}

The first value is the counter of total rounds, the other two values
are the positions of the ball in the two games, respectively.

We have introduced this change without altering any code that did not
use the state variable. In standard Arrowized FRP, we would have had to
alter all signal functions and pass the state manually.
