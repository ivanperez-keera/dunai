\begin{code}
module DunaiExamplesConcepts where
import Data.MonadicStreamFunction
\end{code}

\paragraph{Example}

One trivial way of using these combinators is the stream function that
adds a constant number to the input:

\begin{code}
add     :: (Num n, Monad m) =>  n -> MSF m n n
add n0  = arr (\n -> n + n0)
\end{code}

\noindent which we test in a session (in GHC, monad-parametric computations are
run in the IO monad and the results printed, if possible):

% \begin{spec}
% > embed (add 7) [1,2,3]
% [8, 9, 10]
% \end{spec}

\paragraph{Examples}
Extending the previous example, we can write:

% \begin{spec}
% > embed (second (add 7)) [(1,1),(2,2),(3,3)]
% [(1,8), (2,9), (3, 10)]
% \end{spec}

\paragraph{Example}

The following example of palindromes demonstrates monadic and composition
combinators:

\begin{code}
testSerial  ::   MSF IO () ()
testSerial  =    (liftS (\() -> getLine))
            >>>  (arr id &&& arr reverse)
            >>>  (liftS print)
\end{code}

% \begin{spec}
% > embed testSerial (replicate 1 ())
% Text
% ("Text", "txeT")
% \end{spec}

\paragraph{Example}
The following calculates the cumulative sum of its inputs, initializing
an accumulator and using a feedback loop:

% Included with apostrophe because it's in the library.
\begin{code}
sumFrom'     :: (Num n, Monad m) =>  n -> MSF m n n
sumFrom' n0  = feedback n0 (arr add2)
  where add2(n, acc) = let n' = n + acc in (n', n')
\end{code}

A counter can now be defined as follows:

% Included with apostrophe because it's in the library.
\begin{code}
count'  ::  (Num n, Monad m) => MSF m () n
count'  =   arr (const 1) >>> sumFrom' 0
\end{code}

\subsection{Example: One-Dimensional Pong}

Before moving on to control structures and monadic MSFs, let us present part of
an example of a game of pong in one dimension.

The game state is just the position of the ball at each point.
We assume the players sit at fixed positions:

\begin{code}
type Game  = Ball
type Ball  = Int

rightPlayerPos  = 5
leftPlayerPos   = 0
\end{code}

The ball will move in either direction, one step at a time:

\begin{code}
ballToRight  ::  Monad m => MSF m () Ball
ballToRight  =   count' >>> arr (\n -> leftPlayerPos + n)

ballToLeft   ::  Monad m => MSF m () Ball
ballToLeft   =   count' >>> arr (\n -> rightPlayerPos - n)
\end{code}

We can detect when the ball should switch direction with:

\begin{code}
hitRight  ::  Monad m => MSF m Ball Bool
hitRight  =   arr (>= rightPlayerPos)

hitLeft   ::  Monad m => MSF m Ball Bool
hitLeft   =   arr (<= leftPlayerPos)
\end{code}

Switching itself will be introduced in Section~\ref{sec:switch}, when we
talk about Control Flow and Exceptions.
