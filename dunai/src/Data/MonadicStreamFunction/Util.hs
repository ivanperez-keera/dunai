-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Useful auxiliary functions and definitions.
module Data.MonadicStreamFunction.Util
    ( -- * Streams and sinks
      MStream
    , MSink

    -- * Asynchronous processing
    , concatS
    , mapMaybeS

    -- * Adding side effects
    , withSideEffect
    , withSideEffect_

    -- * Delays
    , iPre
    , iPost
    , next
    , fifo

    -- * Folding
    -- ** Folding for 'VectorSpace' instances
    , count
    , sumS
    , sumFrom

    -- ** Folding for monoids
    , mappendS
    , mappendFrom

    -- ** Generic folding \/ accumulation
    , accumulateWith
    , mealy

    -- ** Unfolding
    , unfold
    , repeatedly

    -- ** Parallelism
    , (&|&)
    , (*|*)

    -- * Debugging
    , trace
    , traceWith
    , traceWhen
    , pauseOn

    -- * Step-based reactimation
    , ReactHandle
    , reactInit
    , react
    )
   where

-- Internal imports
import Data.MonadicStreamFunction.Util.Async         (concatS, mapMaybeS)
import Data.MonadicStreamFunction.Util.Debug         (pauseOn, trace, traceWhen,
                                                      traceWith)
import Data.MonadicStreamFunction.Util.Delay         (fifo, iPost, iPre, next)
import Data.MonadicStreamFunction.Util.Effect        (withSideEffect,
                                                      withSideEffect_)
import Data.MonadicStreamFunction.Util.Fold          (accumulateWith, count,
                                                      mappendFrom, mappendS,
                                                      mealy, repeatedly,
                                                      sumFrom, sumS, unfold)
import Data.MonadicStreamFunction.Util.MonadicStream (MSink, MStream)
import Data.MonadicStreamFunction.Util.Parallel      ((&|&), (*|*))
import Data.MonadicStreamFunction.Util.ReactHandle   (ReactHandle, react,
                                                      reactInit)
