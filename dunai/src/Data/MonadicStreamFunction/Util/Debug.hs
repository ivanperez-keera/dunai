-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Useful auxiliary functions and definitions.
module Data.MonadicStreamFunction.Util.Debug
    ( trace
    , traceWith
    , traceWhen
    , pauseOn
    )
  where

-- External imports
import Control.Monad (when)

-- Internal imports
import Data.MonadicStreamFunction.Core        (MSF)
import Data.MonadicStreamFunction.Util.Effect (withSideEffect)

-- | Outputs every input sample, with a given message prefix.
trace :: Show a => String -> MSF IO a a
trace = traceWith putStrLn

-- | Outputs every input sample, with a given message prefix, using an
-- auxiliary printing function.
traceWith :: (Monad m, Show a) => (String -> m ()) -> String -> MSF m a a
traceWith method msg =
  withSideEffect (method . (msg ++) . show)

-- | Outputs every input sample, with a given message prefix, using an
-- auxiliary printing function, when a condition is met.
traceWhen :: (Monad m, Show a)
          => (a -> Bool)
          -> (String -> m ())
          -> String
          -> MSF m a a
traceWhen cond method msg = withSideEffect $ \a ->
  when (cond a) $ method $ msg ++ show a

-- | Outputs every input sample, with a given message prefix, when a condition
-- is met, and waits for some input \/ enter to continue.
pauseOn :: Show a => (a -> Bool) -> String -> MSF IO a a
pauseOn cond = traceWhen cond $ \s -> print s >> getLine >> return ()
