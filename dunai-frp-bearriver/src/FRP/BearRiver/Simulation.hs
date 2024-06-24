{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : FRP.BearRiver.Simulation
-- Copyright   : (c) Ivan Perez, 2014-2024
--               (c) George Giorgidze, 2007-2012
--               (c) Henrik Nilsson, 2005-2006
--               (c) Antony Courtney and Henrik Nilsson, Yale University, 2003-2004
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : ivan.perez@keera.co.uk
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Execution/simulation of signal functions.
--
-- SFs can be executed in two ways: by running them, feeding input samples one
-- by one, obtained from a monadic environment (presumably, @IO@), or by passing
-- an input stream and calculating an output stream. The former is called
-- /reactimation/, and the latter is called /embedding/.
--
-- * Running:
-- Normally, to run an SF, you would use 'reactimate', providing input samples,
-- and consuming the output samples in the 'IO' monad. This function takes over
-- the program, implementing a "main loop". If you want more control over the
-- evaluation loop (for instance, if you are using BearRiver in combination
-- with a backend that also implements some main loop), you may want to use the
-- lower-level API for reactimation ('ReactHandle', 'reactInit', 'react').
--
-- * Embedding:
-- You can use 'embed' for testing, to evaluate SFs in a terminal, and to embed
-- an SF inside a larger system. The helper functions 'deltaEncode' and
-- 'deltaEncodeBy' facilitate producing input /signals/ from plain lists of
-- input samples.
--
-- This module also includes debugging aids needed to execute signal functions
-- step by step, which are used by BearRiver's testing facilities.
module FRP.BearRiver.Simulation
    (
      -- * Reactimation
      reactimate

      -- ** Low-level reactimation interface
    , ReactHandle
    , reactInit
    , react

      -- * Embedding
    , embed
    , embedSynch
    , deltaEncode
    , deltaEncodeBy

      -- * Debugging / Step by step simulation

    , FutureSF
    , evalAtZero
    , evalAt
    , evalFuture
    )
  where

-- External imports
import Control.Arrow             (arr, (***), (>>>))
import Control.Monad             (unless)
import Control.Monad.Fail        (MonadFail)
import Control.Monad.Trans.Class (lift)
import Data.Functor.Identity     (Identity, runIdentity)
import Data.IORef                (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe                (fromMaybe)

-- Internal imports: dunai
import           Control.Monad.Trans.MSF                 hiding (dSwitch)
import qualified Control.Monad.Trans.MSF                 as MSF
import           Data.MonadicStreamFunction              hiding (embed,
                                                          reactimate)
import qualified Data.MonadicStreamFunction              as MSF
import           Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- Internal imports
import FRP.BearRiver.InternalCore (DTime, SF (..))

-- * Reactimation

-- | Convenience function to run a signal function indefinitely, using a IO
-- actions to obtain new input and process the output.
--
-- This function first runs the initialization action, which provides the
-- initial input for the signal transformer at time 0.
--
-- Afterwards, an input sensing action is used to obtain new input (if any) and
-- the time since the last iteration. The argument to the input sensing function
-- indicates if it can block. If no new input is received, it is assumed to be
-- the same as in the last iteration.
--
-- After applying the signal function to the input, the actuation IO action is
-- executed. The first argument indicates if the output has changed, the second
-- gives the actual output). Actuation functions may choose to ignore the first
-- argument altogether. This action should return True if the reactimation must
-- stop, and False if it should continue.
--
-- Note that this becomes the program's /main loop/, which makes using this
-- function incompatible with GLUT, Gtk and other graphics libraries. It may
-- also impose a sizeable constraint in larger projects in which different
-- subparts run at different time steps. If you need to control the main loop
-- yourself for these or other reasons, use 'reactInit' and 'react'.
reactimate :: Monad m
           => m a                          -- ^ Initialization action
           -> (Bool -> m (DTime, Maybe a)) -- ^ Input sensing action
           -> (Bool -> b -> m Bool)        -- ^ Actuation (output processing)
                                           --   action
           -> SF m a b                     -- ^ Signal function
           -> m ()
reactimate senseI sense actuate sf = do
    MSF.reactimateB $ senseSF >>> sfIO >>> actuateSF
    return ()
  where
    sfIO = runReaderS sf

    -- Sense
    senseSF = MSF.dSwitch senseFirst senseRest

    -- Sense: First sample
    senseFirst = constM senseI >>> arr (\x -> ((0, x), Just x))

    -- Sense: Remaining samples
    senseRest a = constM (sense True) >>> (arr id *** keepLast a)

    keepLast :: Monad m => a -> MSF m (Maybe a) a
    keepLast a = MSF $ \ma ->
      let a' = fromMaybe a ma
      in a' `seq` return (a', keepLast a')

    -- Consume/render
    actuateSF = arr (\x -> (True, x)) >>> arrM (uncurry actuate)

-- An API for animating a signal function when some other library needs to own
-- the top-level control flow:

-- reactimate's state, maintained across samples:
data ReactState a b = ReactState
  { rsActuate :: ReactHandle a b -> Bool -> b -> IO Bool
  , rsSF      :: SF Identity a b
  , rsA       :: a
  , rsB       :: b
  }

-- | A reference to reactimate's state, maintained across samples.
newtype ReactHandle a b = ReactHandle
  { reactHandle :: IORef (ReactState a b) }

-- | Initialize a top-level reaction handle.
reactInit :: IO a                                      -- init
          -> (ReactHandle a b -> Bool -> b -> IO Bool) -- actuate
          -> SF Identity a b
          -> IO (ReactHandle a b)
reactInit init actuate (MSF tf0) = do
  a0 <- init
  let (b0, sf) = runReader (tf0 a0) 0
  -- TODO: really need to fix this interface, since right now we just ignore
  -- termination at time 0:
  r' <- newIORef (ReactState { rsActuate = actuate, rsSF = sf
                             , rsA = a0, rsB = b0
                             }
                 )
  let r = ReactHandle r'
  _ <- actuate r True b0
  return r

-- | Process a single input sample.
react :: ReactHandle a b
      -> (DTime, Maybe a)
      -> IO Bool
react rh (dt, ma') = do
  rs <- readIORef (reactHandle rh)
  let ReactState {rsActuate = actuate, rsSF = sf, rsA = a, rsB = _b } = rs

  let a' = fromMaybe a ma'
      (b', sf') = runReader (unMSF sf a') dt
  writeIORef (reactHandle rh) (rs {rsSF = sf', rsA = a', rsB = b'})
  done <- actuate rh True b'
  return done

-- * Embedding

-- | Given a signal function and a pair with an initial input sample for the
-- input signal, and a list of sampling times, possibly with new input samples
-- at those times, it produces a list of output samples.
--
-- This is a simplified, purely-functional version of 'reactimate'.
embed :: Monad m => SF m a b -> (a, [(DTime, Maybe a)]) -> m [b]
embed sf0 (a0, dtas) = do
    (b0, sf) <- runReaderT (unMSF sf0 a0) 0
    bs <- loop a0 sf dtas
    return $ b0 : bs
  where
    loop _ _ [] = return []
    loop aPrev sf ((dt, ma) : dtas) = do
      let a = fromMaybe aPrev ma
      (b, sf') <- runReaderT (unMSF sf a) dt
      bs <- loop a sf' dtas
      return $ a `seq` b `seq` (b : bs)

-- | Synchronous embedding. The embedded signal function is run on the supplied
-- input and time stream at a given (but variable) ratio >= 0 to the outer time
-- flow. When the ratio is 0, the embedded signal function is paused.
embedSynch :: forall m a b
            . (Monad m, MonadFail m)
           => SF m a b -> (a, [(DTime, Maybe a)]) -> SF m Double b
embedSynch sf0 (a0, dtas) = MSF tf0
  where
    tts = scanl (\t (dt, _) -> t + dt) 0 dtas

    tf0 :: Double -> ReaderT DTime m (b, SF m Double b)
    tf0 _ = do
      bbs@(b:_) <- lift (embed sf0 (a0, dtas))
      return (b, esAux 0 (zip tts bbs))

    esAux :: Double -> [(DTime, b)] -> SF m Double b
    esAux _      []    = error "BearRiver: embedSynch: Empty list!"
    -- Invarying below since esAux [] is an error.
    esAux tpPrev tbtbs = MSF tf -- True
      where
        tf r | r < 0     = error "BearRiver: embedSynch: Negative ratio."
             | otherwise = do
                 dt <- ask
                 let tp          = tpPrev + dt * r
                     (b, tbtbs') = advance tp tbtbs
                 return (b, esAux tp tbtbs')

    -- Advance the time stamped stream to the perceived time tp. Under the
    -- assumption that the perceived time never goes backwards (non-negative
    -- ratio), advance maintains the invariant that the perceived time is always
    -- >= the first time stamp.
    advance _  tbtbs@[(_, b)] = (b, tbtbs)
    advance tp tbtbtbs@((_, b) : tbtbs@((t', _) : _))
      | tp <  t' = (b, tbtbtbs)
      | t' <= tp = advance tp tbtbs
    advance _ _ = undefined

-- | Spaces a list of samples by a fixed time delta, avoiding unnecessary
-- samples when the input has not changed since the last sample.
deltaEncode :: Eq a => DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncode _  []        = error "BearRiver: deltaEncode: Empty input list."
deltaEncode dt aas@(_:_) = deltaEncodeBy (==) dt aas

-- | 'deltaEncode' parameterized by the equality test.
deltaEncodeBy :: (a -> a -> Bool) -> DTime -> [a] -> (a, [(DTime, Maybe a)])
deltaEncodeBy _  _  []      = error "BearRiver: deltaEncodeBy: Empty input list."
deltaEncodeBy eq dt (a0:as) = (a0, zip (repeat dt) (debAux a0 as))
  where
    debAux _     []                    = []
    debAux aPrev (a:as) | a `eq` aPrev = Nothing : debAux a as
                        | otherwise    = Just a  : debAux a as

-- * Debugging / Step by step simulation

-- | A wrapper around an initialized SF (continuation), needed for testing and
-- debugging purposes.
newtype FutureSF m a b = FutureSF { unsafeSF :: SF m a b }

-- * Debugging / Step by step simulation

-- | Evaluate an SF, and return an output and an initialized SF.
--
-- /WARN/: Do not use this function for standard simulation. This function is
-- intended only for debugging/testing. Apart from being potentially slower and
-- consuming more memory, it also breaks the FRP abstraction by making samples
-- discrete and step based.
evalAtZero :: SF Identity a b -> a -> (b, SF Identity a b)
evalAtZero sf a = runIdentity $ runReaderT (unMSF sf a) 0

-- | Evaluate an initialized SF, and return an output and a continuation.
--
-- /WARN/: Do not use this function for standard simulation. This function is
-- intended only for debugging/testing. Apart from being potentially slower and
-- consuming more memory, it also breaks the FRP abstraction by making samples
-- discrete and step based.
evalAt :: SF Identity a b -> DTime -> a -> (b, SF Identity a b)
evalAt sf dt a = runIdentity $ runReaderT (unMSF sf a) dt

-- | Given a signal function and time delta, it moves the signal function into
-- the future, returning a new uninitialized SF and the initial output.
--
-- While the input sample refers to the present, the time delta refers to the
-- future (or to the time between the current sample and the next sample).
--
-- /WARN/: Do not use this function for standard simulation. This function is
-- intended only for debugging/testing. Apart from being potentially slower and
-- consuming more memory, it also breaks the FRP abstraction by making samples
-- discrete and step based.
evalFuture :: SF Identity a b -> a -> DTime -> (b, SF Identity a b)
evalFuture sf = flip (evalAt sf)
