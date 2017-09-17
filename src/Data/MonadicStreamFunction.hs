-- | Monadic Stream Functions are synchronized stream functions
--   with side effects.
--   .
--   MSFs are defined by a function @step :: MSF m a b -> a -> m (b, MSF m a b)@
--   that executes one step of a simulation, and produces an output in a
--   monadic context, and a continuation to be used for future steps.
--   .
--   MSFs are a generalisation of the implementation mechanism used by Yampa,
--   Wormholes and other FRP and reactive implementations.
--   .
--   When combined with different monads, they produce interesting effects. For
--   example, when combined with the @Maybe@ monad, they become transformations
--   that may stop producing outputs (and continuations). The @Either@ monad
--   gives rise to MSFs that end with a result (akin to Tasks in Yampa, and
--   Monadic FRP).
--   .
--   Flattening, that is, going from some structure @MSF (t m) a b@ to @MSF m a b@
--   for a specific transformer @t@ often gives rise to known FRP constructs.
--   For instance, flattening with @EitherT@ gives rise to switching, and
--   flattening with @ListT@ gives rise to parallelism with broadcasting.
--   .
--   MSFs can be used to implement many FRP variants, including Arrowized FRP,
--   Classic FRP, and plain reactive programming. Arrowized and applicative
--   syntax are both supported.
--   .
--   For a very detailed introduction to MSFs, see:
--   <http://dl.acm.org/citation.cfm?id=2976010>
--   (mirror: <http://www.cs.nott.ac.uk/~psxip1/#FRPRefactored>).
module Data.MonadicStreamFunction
  ( module Control.Arrow
  , module X
  )
 where

-- External

import Control.Arrow

-- Internal

import Data.MonadicStreamFunction.Core        as X
import Data.MonadicStreamFunction.Util        as X

-- Internal (Instances)

import Data.MonadicStreamFunction.ArrowChoice ()
import Data.MonadicStreamFunction.ArrowLoop   ()
import Data.MonadicStreamFunction.ArrowPlus   ()
