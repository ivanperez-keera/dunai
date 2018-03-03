\section{Reactive Programming and Monadic Streams}
\label{sec:reactiveprogramming}

Reactive Programming is a programming paradigm organised around information
producers and consumers, or \emph{streams} and \emph{sinks}.  In this section
we present definitions of Streams and Sinks based on Monadic Stream Functions.
Some properties of monadic streams also apply to stream functions or are easier
to prove in that setting. The existing research on streams and causal stream
functions makes establishing this relation useful in its own right.

We present definitions of Streams and Sinks based on Monadic Stream
Functions, and demonstrate how to do Reactive Programming using the
ideas introduced in the previous section. We also present an
extension suitable for event-driven settings like GUIs. In
Section~\ref{sec:classicfrp} we use similar concepts to implement
Classic FRP.

\subsection{Streams and Sinks}

% \paragraph{Streams}

Monadic Stream Functions that do not depend on their input model
Monadic Streams. We can capture that idea with:

\begin{code}
type Stream m b  =  MSF m () b
\end{code}

% The meaning of an MSF is given by the |step| function, with signature:
%
% \begin{code}
% step :: MSF m a b -> a -> m (b, MSF m a b)
% \end{code}

Disregarding bottoms and applying unit as the only possible argument,
the above expands to |Stream m b  cong   m (b, Stream m b)|, and |Stream
Identity| is isomorphic to standard infinite streams.
%
% \todo{IP}{Can we really apply |()|? Is the symbol |cong| the right one
% here?}
%
% \begin{code}
% Stream m b  cong   m (b, Stream m b)
% \end{code}
% \begin{code}
% Stream m b  cong  MSF m () b
%             cong  { definition of MSF }
%                   () -> m (b, Stream m b)
%             cong  { applying unit }
%                   m (b, Stream m b)
% \end{code}
% The type |Stream Identity| is isomorphic to coinductive infinite
% streams.
% |newtype IStream b = Cons (b, IStream b)|.

% NOTE for ourselves: we can do this because, according to the Haskell
%report, for the Identity monad, defined with a newtype, we know
% Identity _|_ = _|_
%
% For details see:
% https://www.haskell.org/onlinereport/decls.html#datatype-renaming
% http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Functor-Identity.html

% \paragraph{Sinks}

If streams can be seen as MSF that do not depend on their inputs, \emph{sinks}
can be seen as MSF do not produce any output:

\begin{code}
type Sink m b = MSF m b ()
\end{code}

These represent dead-ends of information flow, useful when we are only
interested in the side effects.

% Sinks are present in reactive implementations like reactive
% banana~\footnote{https://hackage.haskell.org/package/reactive-banana},
% Wormholes~\cite{2012:winograd:wormholes}, and Reactive
% Values~\cite{2015:perez:reactivevalues}, presented below.

Monadic Streams, as defined above, are |Functors| and \linebreak
|Applicatives|. Sinks, in
turn, are \emph{contravariant functors}:

\begin{code}
instance Contravariant (Sink m) where
  contramap :: (a -> b) -> Sink m b -> Sink m a
  contramap f msf = arr f >>> msf
\end{code}

\paragraph{Examples}

These abstractions allow us to write more declarative code.  For
instance, given |mouseX :: Stream IO Int|, represeting the changing X
coordinate of the mouse position, we can write:
% \footnote{These instances use a |Monad| constraint
% for readability; our library only needs |Functor|/|Applicative| constraints on
% |m|. For the same reason, here we give definitions for the type synonym
% |Stream|. Our library defines them for the more general type |MSF m a|,
% parameterised over any input type.}:

% Variable names here are a bit confusing.

% \begin{code}
% instance Monad m => Functor (Stream m) where
%   fmap f   = (>>> arr f)
%
% instance Monad m => Applicative (Stream m) where
%   pure x   = arr (const x)
%   f <*> x  = (f &&& x) >>> arr (uncurry ($))
% \end{code}
% Is the above the same as the following? Is there an even
% simpler way of stating it?
% MSF $ \a -> do
%     (f'  , cf)  <- runMSF f  a
%     (x'  , cx)  <- runMSF x  a
%     return (f' x', cf <*> cx)

% IP: Explained in footnote.
%
% \iperez{TypeSynonyms used in instances.}
% \manuel{These instances also exist for MSFs with nontrivial input.
% This should be definitely mentioned.\\ iperez: It's difficult to
% introduce though because it complicates the type signature and the
% analogy to streams is a bit lost.}


\begin{code}
mirroredMouseX :: Stream IO Int
mirroredMouseX = (-) <$> 1024 <*> mouseX
\end{code}

We can sometimes simplify code further. For example,
% using the previous instances
we can give a |Num| instance for |Num|-carrying |Stream|s, and
overload the standard numeric operators:

% instance (Monad m, Num b) => Num (Stream m b) where
%  ...

% IMPORTANT: This code needs to be moved to wherever the previous citation
% points to.
%
% \begin{code}
% instance (Monad m, Num b) => Num (Stream m b) where
%   f  +  g          = (+)  <$> f <*> g
%   f  *  g          = (*)  <$> f <*> g
%   abs              = fmap abs
%   signum           = fmap signum
%   fromInteger x    = pure (fromInteger a)
%   negate           = fmap negate
% \end{code}

\begin{code}
mirroredMouseX  :: Stream IO Int
mirroredMouseX  = 1024 - mouseX
\end{code}

\noindent Note that, in this new definition, $1024$ has type |Stream IO Int|.

Streams and sinks are MSFs, so we can use MSF combinators to transform them and
connect them. The following reactive program chains a stream and a sink
to print the mouse position:
\begin{code}
reactiveProgram = mouseX >>> arr show >>> printSink

printSink   ::  Sink IO String
printSink   =   liftS putStrLn
\end{code}

