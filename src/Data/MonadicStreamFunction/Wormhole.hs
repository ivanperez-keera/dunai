-- | As described by Winograd-Court,
-- TODO more precise reference
-- a wormhole consists of a data sink and a data source which are linked
-- by a synchronisation mechanism.
-- The aim of this technique is to abstract from the particular mechanism of synchronisation
-- and supply an interface that is free of deadlocks and race conditions.

-- Often, an external reactive framework may have several parallel loops,
-- for example, OpenGL with a display callback, an idle callback and a keyboard callback.
-- In such cases, one would like to let the different parts communicate.
-- This is done through a wormhole, which is a shared mutable variable
-- that can be written from one part and read from the other.

-- An example implementation is given for IORefs.
-- Usually, the implementation is straightforward for other synchronisation methods.
-- A word of caution has to be said though,
-- against using _blocking_ synchronisation mechanisms,
-- such as MVars, for wormholes. In typical use cases, the side effects of the sink
-- and the source have to be assumed to be non-blocking, ruling e.g. MVars out.

module Data.MonadicStreamFunction.Wormhole where

-- External
import Control.Monad.IO.Class
import Control.Concurrent
import Data.IORef

-- Internal
import Data.MonadicStreamFunction

-- | A wormhole consists of two monadic stream functions,
-- a sink and a source. It should satisfy the following properties:
-- * The data sent into the sink is transferred to the source by means of side effects in m.
-- * Both sink and source are non-blocking.
data Wormhole m a = Wormhole
  { sink   :: MSF m a  ()
  , source :: MSF m () a
  }

-- | Creates a wormhole that encapsulates an initialised IORef
newWormholeIORef :: MonadIO m => a -> m (Wormhole m a)
newWormholeIORef a = liftIO $ do
  ref <- newIORef a
  return $ Wormhole
    ( arrM  $ liftIO . writeIORef ref )
    ( arrM_ $ liftIO $ readIORef  ref )

-- | Creates two wormholes with IORefs at each end of the stream function
-- and runs the inner stream function in a separate thread.
-- It returns the outer stream function, which can be used to send and receive data
-- from the other thread, and a method to kill the thread.
concurrently :: MSF IO a b -> a -> b -> IO (MSF IO a b, IO ())
concurrently msf a b = do
  input    <- newWormholeIORef a
  output   <- newWormholeIORef b
  threadId <- forkIO $ reactimate $ source input >>> msf >>> sink output
  return (sink input >>> source output, killThread threadId)
