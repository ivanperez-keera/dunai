module Control.Monad.Trans.MSF.Writer
  ( module Control.Monad.Trans.MSF.Writer
  , module Control.Monad.Trans.Writer.Strict
  ) where

-- External
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.Strict
  hiding (liftCallCC, liftCatch, pass) -- Avoid conflicting exports

-- Internal
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction

-- * Writer monad
writerS :: (Monad m, Monoid s) => MSF m a (s, b) -> MSF (WriterT s m) a b
writerS msf = MSF $ \a -> do
    ((s, b), msf') <- lift $ unMSF msf a
    tell s
    return (b, writerS msf')

runWriterS :: Monad m => MSF (WriterT s m) a b -> MSF m a (s, b)
runWriterS msf = MSF $ \a -> do
    ((b, msf'), s') <- runWriterT $ unMSF msf a
    return ((s', b), runWriterS msf')


-- * Alternative running/wrapping MSF combinators using generic lifting

writerS' :: (Monad m, Monoid s) => MSF m a (s, b) -> MSF (WriterT s m) a b
writerS' = lifterS wrapMSFWriterT

runWriterS' :: (Monoid s, Functor m, Monad m) => MSF (WriterT s m) a b -> MSF m a (s, b)
runWriterS' = lifterS unwrapMSFWriterT

writerS'' :: (Monad m, Monoid w) => MSF m a (w, b) -> MSF (WriterT w m) a b
writerS'' = transS transformInput transformOutput
  where
    transformInput = return
    transformOutput _ msfaction = do
        ((w, b), msf') <- lift msfaction
        tell w
        return (b, msf')

runWriterS'' :: (Monoid s, Functor m, Monad m) => MSF (WriterT s m) a b -> MSF m a (s, b)
runWriterS'' = transS transformInput transformOutput
  where
    transformInput              = return
    transformOutput _ msfaction = sym <$> runWriterT msfaction
    sym ((b, msf), s)           = ((s, b), msf)

-- ** Wrapping/unwrapping functions
--
-- TODO: These are *almost*-MSF-agnostic wrapping/unwrapping functions.
-- The continuations (and therefore the stream functions) are still
-- there, but now we know nothing about them, not even their type.
-- Monadic actions carry an extra value, of some polymorphic type ct,
-- which is only necessary to extract the output and the context.
--
-- wrapMSFWriterT :: (Monad m, Functor m) => (a -> WriterT s m (b, ct)) -> a -> m ((s, b), ct)
wrapMSFWriterT :: (Monoid s, Monad m) => (a -> m ((s, b), ct)) -> a -> WriterT s m (b, ct)
wrapMSFWriterT g i = do
  ((s, b), msf) <- lift $ g i
  tell s
  return (b, msf)

unwrapMSFWriterT :: (Monad m, Functor m) => (a -> WriterT s m (b, ct)) -> a -> m ((s, b), ct)
unwrapMSFWriterT g i = resort <$> runWriterT (g i)
  where resort ((b, msf), s) = ((s, b), msf)
