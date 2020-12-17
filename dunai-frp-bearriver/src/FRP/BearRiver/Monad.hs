module FRP.BearRiver.Monad
    (   liftTransSF,
        readerSF,
        runReaderSF,
        runReaderSF_,
        liftReaderSF,
        stateSF,
        runStateSF,
        runStateSF_,
        execStateSF,
        liftStateSF,
        writerSF,
        runWriterSF,
        runWriterSF_,
        liftWriterSF    ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.MSF
import FRP.BearRiver

-- * Monad transformations

-- | Lift inner monadic actions in SFs
liftTransSF :: (MonadTrans t, Monad m, Monad (t m)) => SF m a b -> SF (t m) a b
liftTransSF sf = readerS (liftTransS (runReaderS sf))

-- ** Reader layer

-- | Build an 'SF' in the 'Reader' monad from one that takes the reader
-- environment as an extra input. This is the opposite of 'runReaderSF'.
readerSF :: Monad m => SF m (r, a) b -> SF (ReaderT r m) a b
readerSF sf = morphS commuteReaderReader (readerS sf)

-- | Build an 'SF' that takes an environment as an extra input from one on the
-- 'Reader' monad. This is the opposite of 'readerSF'.
runReaderSF :: Monad m => SF (ReaderT r m) a b -> SF m (r, a) b
runReaderSF sf = runReaderS (morphS commuteReaderReader sf)

-- | Build an 'SF' /function/ that takes a fixed environment as additional
-- input, from an SF in the 'Reader' monad
runReaderSF_ :: Monad m => r -> SF (ReaderT r m) a b -> SF m a b
runReaderSF_ r sf = runReaderS_ (morphS commuteReaderReader sf) r

-- | Lift an 'SF' in a certain monad to one in the same monad transformed
-- with 'ReaderT'
liftReaderSF :: Monad m => SF m a b -> SF (ReaderT r m) a b
liftReaderSF sf = morphS commuteReaderReader (liftTransS sf)

-- | Transform an action in a monad transformer stack with two 'ReaderT' layers
-- into one in another stack where the layers are swapped
commuteReaderReader :: ReaderT r1 (ReaderT r2 m) a -> ReaderT r2 (ReaderT r1 m) a
commuteReaderReader (ReaderT f)
    = ReaderT (\r1 -> ReaderT (\r2 -> runReaderT (f r2) r1))

-- ** State layer

-- | Build an 'SF' in the 'State' monad from one that takes the state as an
-- extra input. This is the opposite of 'runStateSF'
stateSF :: Monad m => SF m (s, a) (s, b) -> SF (StateT s m) a b
stateSF sf = morphS commuteReaderState (stateS sf)

-- | Build an 'SF' that takes a state as an extra input from one on the
-- 'State' monad. This is the opposite of 'stateSF'.
runStateSF :: Monad m => SF (StateT s m) a b -> SF m (s, a) (s, b)
runStateSF sf = runStateS (morphS commuteStateReader sf)

-- | Build an 'SF' /function/ that takes a fixed state as additional input,
-- from an 'SF' in the 'State' monad, and outputs the new state with every
-- transformation step.
runStateSF_ :: Monad m => s -> SF (StateT s m) a b -> SF m a (s, b)
runStateSF_ s sf = runStateS_ (morphS commuteStateReader sf) s

-- | Build an 'SF' /function/ that takes a fixed state as additional
-- input, from an 'SF' in the 'State' monad.
execStateSF :: Monad m => s -> SF (StateT s m) a b -> SF m a b
execStateSF s sf = runStateS__ (morphS commuteStateReader sf) s

-- | Lift an 'SF' in a certain monad to one in the same monad transformed
-- with 'StateT'
liftStateSF :: Monad m => SF m a b -> SF (StateT s m) a b
liftStateSF sf = morphS commuteReaderState (liftTransS sf)

-- | Transform an action in a monad transformer stack with a 'StateT' layer
-- on top of a 'ReaderT' layer into one in another stack where the layers are
-- swapped
commuteReaderState :: StateT s (ReaderT r m) a -> ReaderT r (StateT s m) a
commuteReaderState (StateT f)
    = ReaderT (\r -> StateT (\s -> runReaderT (f s) r))

-- | Transform an action in a monad transformer stack with a 'ReaderT' layer
-- on top of a 'StateT' layer into one in another stack where the layers are
-- swapped
commuteStateReader :: ReaderT r (StateT s m) a -> StateT s (ReaderT r m) a
commuteStateReader (ReaderT f)
    = StateT (\s -> ReaderT (\r -> runStateT (f r) s))

-- ** Writer layer

-- | Build an 'SF' in the 'Writer' monad from one that produces the log as an
-- extra output. This is the opposite of 'runWriterSF'.
writerSF :: (Monad m, Monoid w) => SF m a (w, b) -> SF (WriterT w m) a b
writerSF sf = morphS commuteReaderWriter (writerS sf)

-- | Build an 'SF' that produces the log as an extra output from one on the
-- 'Writer' monad. This is the opposite of 'writerSF'
runWriterSF :: (Monad m, Monoid w) => SF (WriterT w m) a b -> SF m a (w, b)
runWriterSF sf = runWriterS (morphS commuteWriterReader sf)

-- | Build an 'SF' that discards the log from one on the 'Writer' monad.
runWriterSF_ :: (Monad m, Monoid w) => SF (WriterT w m) a b -> SF m a b
runWriterSF_ sf = runWriterSF sf >>> arr snd

-- | Lift an 'SF' in a certain monad to one in the same monad transformed
-- with 'WriterT'
liftWriterSF :: (Monad m, Monoid w) => SF m a b -> SF (WriterT w m) a b
liftWriterSF sf = morphS commuteReaderWriter (liftTransS sf)

-- | Transform an action in a monad transformer stack with a 'WriterT' layer
-- on top of a 'ReaderT' layer into one in another stack where the layers are
-- swapped
commuteReaderWriter :: WriterT w (ReaderT r m) a -> ReaderT r (WriterT w m) a
commuteReaderWriter (WriterT m)
    = ReaderT (\r -> WriterT (runReaderT m r))

-- | Transform an action in a monad transformer stack with a 'ReaderT' layer
-- on top of a 'WriterT' layer into one in another stack where the layers are
-- swapped
commuteWriterReader :: ReaderT r (WriterT w m) a -> WriterT w (ReaderT r m) a
commuteWriterReader (ReaderT f)
    = WriterT (ReaderT (\r -> runWriterT (f r)))
