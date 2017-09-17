-- | ReactHandle

-- Sometimes it is beneficial to give control to an external main loop,
-- for example OpenGL or a hardware-clocked audio server like JACK.
-- This module makes Dunai compatible with external main loops.

module Data.MonadicStreamFunction.ReactHandle where

-- External
import Control.Monad.IO.Class
import Data.IORef

-- Internal
import Data.MonadicStreamFunction


-- | A storage for the current state of an MSF
type ReactHandle m = IORef (MSF m () ())


-- | Needs to be called before the external main loop is dispatched
reactInit :: MonadIO m => MSF m () () -> m (ReactHandle m)
reactInit = liftIO . newIORef


-- | The callback that needs to be called by the main loop at every cycle
react :: MonadIO m => ReactHandle m -> m ()
react handle = do
  msf <- liftIO $ readIORef handle
  (_, msf') <- unMSF msf ()
  liftIO $ writeIORef handle msf'


-- | Creates two ends of a synchronisation wormhole

-- Often, the external framework may have several parallel loops,
-- for example, OpenGL with a display callback, an idle callback and a keyboard callback.
-- In such cases, one would like to let the different parts communicate.
-- This is done through a wormhole, which is a shared mutable variable
-- that can be written from one part and read from the other.

createWormhole :: MonadIO m => a -> m (MSF m a (), MSF m () a)
createWormhole a = liftIO $ do
  ref <- newIORef a
  return (arrM $ liftIO . writeIORef ref, arrM_ $ liftIO $ readIORef ref)
