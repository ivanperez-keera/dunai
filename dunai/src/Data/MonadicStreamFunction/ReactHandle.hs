-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'ReactHandle's.
--
-- Sometimes it is beneficial to give control to an external main loop, for
-- example OpenGL or a hardware-clocked audio server like JACK. This module
-- makes Dunai compatible with external main loops.
module Data.MonadicStreamFunction.ReactHandle where

-- External imports
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef             (IORef, newIORef, readIORef, writeIORef)

-- Internal imports
import Data.MonadicStreamFunction              (MSF)
import Data.MonadicStreamFunction.InternalCore (unMSF)

-- | A storage for the current state of an 'MSF'. The 'MSF' may not require
-- input or produce output data, all such data must be handled through side
-- effects (such as wormholes).
type ReactHandle m = IORef (MSF m () ())

-- | Needs to be called before the external main loop is dispatched.
reactInit :: MonadIO m => MSF m () () -> m (ReactHandle m)
reactInit = liftIO . newIORef

-- | The callback that needs to be called by the external loop at every cycle.
react :: MonadIO m => ReactHandle m -> m ()
react handle = do
  msf <- liftIO $ readIORef handle
  (_, msf') <- unMSF msf ()
  liftIO $ writeIORef handle msf'
