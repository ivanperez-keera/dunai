{-# LANGUAGE TupleSections #-}
-- | As described by Winograd-Court in his PhD thesis,
-- a wormhole consists of a data sink and a data source which are linked
-- by a synchronisation mechanism.
-- The aim of this technique is to abstract from the particular mechanism
-- of synchronisation and supply an interface
-- that is free of deadlocks and race conditions.
--
-- Often, an external reactive framework may have several parallel loops,
-- for example, OpenGL with a display callback,
-- an idle callback and a keyboard callback.
-- In such cases, one would like to let the different parts communicate.
-- This is done through a wormhole, which is a shared mutable variable
-- that can be written from one part and read from the other.
--
-- An example implementation is given for 'IORefs'.
-- Usually, the implementation is straightforward
-- for other synchronisation methods.
-- A word of caution has to be said though,
-- against using _blocking_ synchronisation mechanisms,
-- such as 'MVar's, for wormholes. In typical use cases,
-- the side effects of the sink and the source
-- have to be assumed to be non-blocking, ruling e.g. 'MVar's out.

module Data.MonadicStreamFunction.Wormhole where

-- External
import Control.Monad.IO.Class
import Control.Concurrent
import Data.IORef
import Data.Monoid

-- Internal
import Data.MonadicStreamFunction

-- | A wormhole consists of two monadic stream functions,
-- a sink and a source. It should satisfy the following properties:
--
-- * The data sent into the sink is transferred to the source
--   by means of side effects in @m@.
-- * In case of 'IO', sink and source are non-blocking.
data Wormhole m a b = Wormhole
  { sink   :: MSF m a ()
  , source :: MSF m () b
  }

-- | Creates a wormhole that encapsulates an initialised 'IORef'.
-- No synchronisation guarantee is made,
-- so the 'IORef' could be updated several times, or not at all,
-- before it is read again.
newWormholeIORef :: MonadIO m => a -> m (Wormhole m a a)
newWormholeIORef a = liftIO $ do
  ref <- newIORef a
  return $ Wormhole
    { sink   = arrM  $ liftIO . writeIORef ref
    , source = arrM_ $ liftIO $ readIORef  ref
    }

-- | Create a wormhole encapsulating an initialised 'IORef'.
-- Incoming data modifies the stored value atomically and strictly.
newWormholeModifyIORef
  :: MonadIO m
  => b              -- ^ The initial value
  -> (a -> b -> b)  -- ^ This function modifies the stored value for every input
  -> m (Wormhole m a b)
newWormholeModifyIORef b f = liftIO $ do
  ref <- newIORef b
  return $ Wormhole
    { sink   = arrM $ \a -> liftIO $ atomicModifyIORef' ref $ (, ()) . f a
    , source = arrM_ $ liftIO $ readIORef ref
    }

-- | Create a wormhole encapsulating an 'IORef' storing a 'Monoid'.
-- Every input is (atomically and strictly) 'mappend'ed to the stored value,
-- and whenever output is requested, the stored value is retrieved
-- (atomically and strictly) and replaced by 'mempty'.
newWormholeMonoidIORef :: (Monoid a, MonadIO m) => m (Wormhole m a a)
newWormholeMonoidIORef = liftIO $ do
  ref <- newIORef mempty
  return $ Wormhole
    { sink   = arrM $ \a -> liftIO $ atomicModifyIORef' ref $ (, ()) . mappend a
    , source = arrM_ $ liftIO $ atomicModifyIORef' ref $ \a -> (mempty, a)
    }

-- | Creates two wormholes with 'IORef's at each end of the stream function
-- and runs the inner stream function in a separate thread.
-- It returns the outer stream function, which can be used
-- to send and receive data
-- from the other thread, and a method to kill the thread.
-- As for 'newWormholeIORef', there is no synchronisation guarantee.
concurrently :: MSF IO a b -> a -> b -> IO (MSF IO a b, IO ())
concurrently msf a b = do
  input    <- newWormholeIORef a
  output   <- newWormholeIORef b
  threadId <- forkIO $ reactimate $ source input >>> msf >>> sink output
  return (sink input >>> source output, killThread threadId)

-- | Creates two ends of a synchronisation wormhole.
--
-- Often, the external framework may have several parallel loops,
-- for example, OpenGL with a display callback, an idle callback and a keyboard callback.
-- In such cases, one would like to let the different parts communicate.
-- This is done through a wormhole, which is a shared mutable variable
-- that can be written from one part and read from the other.
-- In this implementation, an 'IORef' is used.
createWormhole :: MonadIO m => a -> m (MSF m a (), MSF m () a)
createWormhole a = liftIO $ do
  ref <- newIORef a
  return (arrM $ liftIO . writeIORef ref, arrM_ $ liftIO $ readIORef ref)
