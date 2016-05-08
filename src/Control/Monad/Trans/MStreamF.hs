{-# LANGUAGE Arrows              #-}
{-# LANGUAGE Rank2Types          #-}

module Control.Monad.Trans.MStreamF where

import Data.Monoid
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Strict hiding (tell, asks, put)
import Control.Monad.Trans.Writer.Strict

import Data.MonadicStreamFunction

-- * Attempt at writing a more generic MSF lifting combinator.  This is
-- here only to make it easier to find, in a perfect world we'd move
-- this to a different module/branch, or at least to the bottom of the
-- file.
--
-- TODO: does this also work well with the state and the writer monads?
--
-- Even if this code works, it's difficult to understand the concept.
--
-- It is also unclear how much it helps. Ideally, the auxiliary function
-- should operate only on monadic values, not monadic stream functions.
-- That way we could separate concepts: namely the recursion pattern
-- from the monadic lifting/unlifting/sequencing.
--
-- Maybe if we split f in several functions, one that does some sort of
-- a -> a1 transformation, another that does some b1 -> b
-- transformation, with the monads and continuations somewhere, it'll
-- make more sense.
--
-- Based on this lifting function we can also defined all the other
-- liftings we have in Core:
--
-- liftMStreamFPurer' :: (Monad m1, Monad m)
--                    => (m1 (b, MStreamF m1 a b) -> m (b, MStreamF m1 a b))
--                    -> MStreamF m1 a b
--                    -> MStreamF m  a b
-- liftMStreamFPurer' f = lifterS (\g a -> f $ g a)
--
-- More liftings:
-- liftMStreamFTrans = liftMStreamFPurer lift
-- liftMStreamFBase  = liftMStreamFPurer liftBase
--
-- And a strict version of liftMStreamFPurer:
-- liftMStreamPurer' f = liftMStreamFPurer (f >=> whnfVal)
--   where whnfVal p@(b,_) = b `seq` return p
--
-- MB: I'm not sure we're gaining much insight by rewriting all the lifting
-- functions like that.
-- IP: I said the same thing above ("It is also unclear how much it
-- helps."). It's work in progress.
--
-- MB: The type (a1 -> m1 (b1, MStreamF m1 a1 b1)) is just MStreamF m1 a1 b1.
-- IP: I'm looking for a lifting pattern in terms of m m1 a b a1 and b1. By
-- exposing the function, I'm hoping to *eventually see* the pattern. If I hide
-- it in the MStreamF, then it'll always remain hidden.
lifterS :: (Monad m, Monad m1)
        => ((a1 -> m1 (b1, MStreamF m1 a1 b1)) -> a -> m (b, MStreamF m1 a1 b1))
        -> MStreamF m1 a1 b1
        -> MStreamF m  a  b
lifterS f msf = MStreamF $ \a -> do
  (b, msf') <- f (unMStreamF msf) a
  return (b, lifterS f msf')

-- ** Another wrapper idea
transS :: (Monad m1, Monad m2)
       => (a2 -> m1 a1)
       -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, c))
       -> MStreamF m1 a1 b1 -> MStreamF m2 a2 b2
transS transformInput transformOutput msf = MStreamF $ \a2 -> do
    (b2, msf') <- transformOutput a2 $ unMStreamF msf =<< transformInput a2
    return (b2, transS transformInput transformOutput msf')

-- ** A more general lifting mechanism that enables recovery.
transG1 :: (Monad m1, Monad m2)
        => (a2 -> m1 a1)
        -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, c))
        -> MStreamF m1 a1 b1 -> MStreamF m2 a2 b2
transG1 transformInput transformOutput msf =
  transG transformInput transformOutput' msf
    where
      -- transformOutput' :: forall c. a2 -> m1 (b1, c) -> m2 (b2, Maybe c)
      transformOutput' a b = return . second Just =<< transformOutput a b

transG :: (Monad m1, Monad m2)
       => (a2 -> m1 a1)
       -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, Maybe c))
       -> MStreamF m1 a1 b1 -> MStreamF m2 a2 b2
transG transformInput transformOutput msf = go
  where go = MStreamF $ \a2 -> do
               (b2, msf') <- transformOutput a2 $ unMStreamF msf =<< transformInput a2
               case msf' of
                 Just msf'' -> return (b2, transG transformInput transformOutput msf'')
                 Nothing    -> return (b2, go)

-- transGN :: (Monad m1, Monad m2)
--         => (a2 -> m1 a1)
--         -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, [c]))
--         -> MStreamF m1 a1 b1 -> MStreamF m2 a2 b2
-- transGN transformInput transformOutput msf = go
--   where go = MStreamF $ \a2 -> do
--                (b2, msf') <- transformOutput a2 $ unMStreamF msf =<< transformInput a2
--                case msf' of
--                  []      -> return (b2, go)
--                  [msf''] -> return (b2, transGN transformInput transformOutput msf'')
--                  ms      ->

-- ** Alternative Reader wrapping/unwrapping MSF combinators
readerS' :: Monad m => MStreamF m (s, a) b -> MStreamF (ReaderT s m) a b
readerS' = lifterS wrapReaderT

runReaderS'' :: Monad m => MStreamF (ReaderT s m) a b -> MStreamF m (s, a) b
runReaderS'' = transG transformInput transformOutput
  where
    transformInput  (_, a) = return a
    transformOutput (s, _) m1 = do (r, c) <- runReaderT m1 s
                                   return (r, Just c)


runStateS''' :: (Functor m, Monad m) => MStreamF (StateT s m) a b -> MStreamF m (s, a) (s, b)
runStateS''' = transG transformInput transformOutput
  where
    transformInput  (_, a)           = return a
    transformOutput (s, _) msfaction = sym <$> flip runStateT s msfaction
    sym ((b, msf), s)                = ((s, b), Just msf)

runMaybeS'' :: Monad m => MStreamF (MaybeT m) a b -> MStreamF m a (Maybe b)
runMaybeS'' = transG transformInput transformOutput
  where
    transformInput       = return
    transformOutput _ m1 = do r <- runMaybeT m1
                              case r of
                                Nothing     -> return (Nothing, Nothing)
                                Just (b, c) -> return (Just b,  Just c)

{-
readerS'' :: Monad m => MStreamF m (s, a) b -> MStreamF (ReaderT s m) a b
readerS'' = transS transformInput transformOutput
  where
    transformInput :: a -> m (s, a)
    transformInput a = (,) <$> asks <*> pure a
    transformOutput _ = lift
-}

runReaderS' :: Monad m => MStreamF (ReaderT s m) a b -> MStreamF m (s, a) b
runReaderS' = lifterS unwrapReaderT

-- *** Wrapping/unwrapping functions
--
-- IP: Alternative formulation (typechecks just fine):
--
-- FIXME: The foralls may get in the way. They may not be necessary.  MB
-- raised the issue already for similar code in Core.
--
type Wrapper   m1 m2 t1 t2 = forall a b . (t1 a -> m2 b     ) -> (a    -> m1 (t2 b))
type Unwrapper m1 m2 t1 t2 = forall a b . (a    -> m1 (t2 b)) -> (t1 a -> m2 b     )
--
-- Helper type, for when we need some identity * -> * type constructor that
-- does not get in the way.
--
type Id a = a
--
-- And for the Reader, we can now define
type ReaderWrapper   s m = Wrapper   (ReaderT s m) m ((,) s) Id
type ReaderUnwrapper s m = Unwrapper (ReaderT s m) m ((,) s) Id
-- and use the types:
-- wrapReaderT   :: ReaderWrapper s m
-- unwrapReaderT :: ReaderUnwrapper s m

wrapReaderT :: ((s, a) -> m b) -> a -> ReaderT s m b
wrapReaderT g i = ReaderT $ g . flip (,) i

unwrapReaderT :: (a -> ReaderT s m b) -> (s, a) -> m b
unwrapReaderT g i = uncurry (flip runReaderT) $ (second g) i

-- ** Alternative State wrapping/unwrapping MSF combinators
--
-- IPerez: TODO: Is this exactly the same as stateS?
stateS' :: (Functor m, Monad m) => MStreamF m (s, a) (s, b) -> MStreamF (StateT s m) a b
stateS' = lifterS (\g i -> StateT ((resort <$>) . (g . flip (,) i)))
 where resort ((s, b), ct) = ((b, ct), s)

-- stateS' :: Monad m => MStreamF m (s, a) (s, b) -> MStreamF (StateT s m) a b
-- stateS' = lifterS $ \f a -> StateT $ \s -> do
--   ((s', b), msf') <- f (s, a)
--   return ((b, msf'), s')

runStateS' :: (Functor m, Monad m) => MStreamF (StateT s m) a b -> MStreamF m (s, a) (s, b)
runStateS' = lifterS (\g i -> resort <$> (uncurry (flip runStateT) (second g i)))
 where resort = \((b, msf), s) -> ((s, b), msf)


runStateS'' :: (Functor m, Monad m) => MStreamF (StateT s m) a b -> MStreamF m (s, a) (s, b)
runStateS'' = transS transformInput transformOutput
  where
    transformInput  (_, a)           = return a
    transformOutput (s, _) msfaction = sym <$> flip runStateT s msfaction
    sym ((b, msf), s)                = ((s, b), msf)

{-
stateS'' :: Monad m => MStreamF m (s, a) (s, b) -> MStreamF (StateT s m) a b
stateS'' = transS transformInput transformOutput
  where
    transformInput  (_, a) = return a
    transformOutput (s, _) = do
        put s
-}
-- ** Alternative Writer wrapping/unwrapping MSF combinators
--

writerS' :: (Monad m, Monoid s) => MStreamF m a (s, b) -> MStreamF (WriterT s m) a b
writerS' = lifterS wrapMSFWriterT

runWriterS' :: (Monoid s, Functor m, Monad m) => MStreamF (WriterT s m) a b -> MStreamF m a (s, b)
runWriterS' = lifterS unwrapMSFWriterT

writerS'' :: (Monad m, Monoid w) => MStreamF m a (w, b) -> MStreamF (WriterT w m) a b
writerS'' = transS transformInput transformOutput
  where
    transformInput = return
    transformOutput _ msfaction = do
        ((w, b), msf') <- lift msfaction
        tell w
        return (b, msf')

runWriterS'' :: (Monoid s, Functor m, Monad m) => MStreamF (WriterT s m) a b -> MStreamF m a (s, b)
runWriterS'' = transS transformInput transformOutput
  where
    transformInput              = return
    transformOutput _ msfaction = sym <$> runWriterT msfaction
    sym ((b, msf), s)           = ((s, b), msf)

-- *** Wrapping/unwrapping functions
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
  where resort = \((b, msf), s) -> ((s, b), msf)

-- * Reader monad
readerS :: Monad m => MStreamF m (s, a) b -> MStreamF (ReaderT s m) a b
readerS msf = MStreamF $ \a -> do
  (b, msf') <- ReaderT $ \s -> unMStreamF msf (s, a)
  return (b, readerS msf')

runReaderS :: Monad m => MStreamF (ReaderT s m) a b -> MStreamF m (s, a) b
runReaderS msf = MStreamF $ \(s,a) -> do
  (b, msf') <- runReaderT (unMStreamF msf a) s
  return (b, runReaderS msf')

-- ** Auxiliary functions related to ReaderT

-- IP: Is runReaderS_ msf s = arr (\a -> (s,a)) >>> runReaderS msf ?
-- MB: Yes, but possibly more efficient.
runReaderS_ :: Monad m => MStreamF (ReaderT s m) a b -> s -> MStreamF m a b
runReaderS_ msf s = MStreamF $ \a -> do
    (b, msf') <- runReaderT (unMStreamF msf a) s
    return (b, runReaderS_ msf' s)

-- * State monad
stateS :: Monad m => MStreamF m (s, a) (s, b) -> MStreamF (StateT s m) a b
stateS msf = MStreamF $ \a -> StateT $ \s -> do
    ((s', b), msf') <- unMStreamF msf (s, a)
    return ((b, stateS msf'), s')

runStateS :: Monad m => MStreamF (StateT s m) a b -> MStreamF m (s, a) (s, b)
runStateS msf = MStreamF $ \(s, a) -> do
    ((b, msf'), s') <- runStateT (unMStreamF msf a) s
    return ((s', b), runStateS msf')

-- ** Auxiliary functions related to StateT

-- IP: Is runStateS_ msf s = feedback s $ runStateS msf >>> arr (\(s,b) -> ((s,b), s)) ?
runStateS_ :: Monad m => MStreamF (StateT s m) a b -> s -> MStreamF m a (s, b)
runStateS_ msf s = MStreamF $ \a -> do
    ((b, msf'), s') <- runStateT (unMStreamF msf a) s
    return ((s', b), runStateS_ msf' s')

-- IP: Is runStateS__ msf s = feedback s $ runStateS msf >>> arr (\(s,b) -> (b, s)) ?
runStateS__ :: Monad m => MStreamF (StateT s m) a b -> s -> MStreamF m a b
runStateS__ msf s = MStreamF $ \a -> do
    ((b, msf'), s') <- runStateT (unMStreamF msf a) s
    return (b, runStateS__ msf' s')

-- * Writer monad
writerS :: (Monad m, Monoid s) => MStreamF m a (s, b) -> MStreamF (WriterT s m) a b
writerS msf = MStreamF $ \a -> do
    ((s, b), msf') <- lift $ unMStreamF msf a
    tell s
    return (b, writerS msf')

runWriterS :: Monad m => MStreamF (WriterT s m) a b -> MStreamF m a (s, b)
runWriterS msf = MStreamF $ \a -> do
    ((b, msf'), s') <- runWriterT $ unMStreamF msf a
    return ((s', b), runWriterS msf')

-- * RWS (Reader-Writer-State) monad

runRWSS :: (Functor m, Monad m, Monoid w)
        => MStreamF (RWST r w s m) a b
        -> MStreamF m (r, s, a) (w, s, b)
runRWSS = transS transformInput transformOutput
  where
    transformInput  (_, _, a) = return a
    transformOutput (r, s, _) msfaction = sym <$> runRWST msfaction r s
    sym ((b, msf'), s, w) = ((w, s, b), msf')


-- * Maybe monad

exit :: Monad m => MStreamF (MaybeT m) a b
exit = MStreamF $ const $ MaybeT $ return Nothing

exitWhen :: Monad m => (a -> Bool) -> MStreamF (MaybeT m) a a
exitWhen condition = go where
    go = MStreamF $ \a -> MaybeT $
        if condition a
        then return Nothing
        else return $ Just (a, go)

exitIf :: Monad m => MStreamF (MaybeT m) Bool ()
exitIf = MStreamF $ \b -> MaybeT $ return $ if b then Nothing else Just ((), exitIf)

-- Just a is passed along, Nothing causes the whole MStreamF to exit
maybeExit :: Monad m => MStreamF (MaybeT m) (Maybe a) a
maybeExit = MStreamF $ MaybeT . return . fmap (\x -> (x, maybeExit))

mapMaybeS :: Monad m => MStreamF m a b -> MStreamF m (Maybe a) (Maybe b)
mapMaybeS msf = go
  where
    go = MStreamF $ \maybeA -> case maybeA of
                                 Just a -> do
                                     (b, msf') <- unMStreamF msf a
                                     return (Just b, mapMaybeS msf')
                                 Nothing -> return (Nothing, go)

-- mapMaybeS msf == runMaybeS (inMaybeT >>> lift mapMaybeS)

inMaybeT :: Monad m => MStreamF (MaybeT m) (Maybe a) a
inMaybeT = liftMStreamF $ MaybeT . return

{-
maybeS :: Monad m => MStreamF m a (Maybe b) -> MStreamF (MaybeT m) a b
maybeS msf = MStreamF $ \a -> MaybeT $ return $ unMStreamF msf a
-- maybeS msf == lift msf >>> inMaybeT
-}

runMaybeS :: Monad m => MStreamF (MaybeT m) a b -> MStreamF m a (Maybe b)
runMaybeS msf = go
  where
    go = MStreamF $ \a -> do
           bmsf <- runMaybeT $ unMStreamF msf a
           case bmsf of
             Just (b, msf') -> return (Just b, runMaybeS msf')
             Nothing        -> return (Nothing, go)

{-
-- MB: Doesn't typecheck, I don't know why
--
-- IP: Because of the forall in runTS.
--
-- From the action runMaybeT msfaction it does not know that
-- the second element of the pair in 'thing' will be a continuation.
--
-- The first branch of the case works because you are passing the
-- msf' as is.
--
-- In the second one, you are passing msf, which has the specific type
-- MStreamF (MaybeT m) a b.
--
-- Two things you can try (to help you see that this is indeed why GHC is
-- complaining):
--   - Make the second continuation undefined. Then it typechecks.
--   - Use ScopedTypeVariables and a let binding to type msf' in the
--   first branch of the case selector. It'll complain about the type
--   of msf' if you say it's forcibly a MStreamF (MaybeT m) a b.
--

runMaybeS'' :: Monad m => MStreamF (MaybeT m) a b -> MStreamF m a (Maybe b)
runMaybeS'' msf = transS transformInput transformOutput msf
  where
    transformInput  = return
    transformOutput _ msfaction = do
      thing <- runMaybeT msfaction
      case thing of
        Just (b, msf') -> return (Just b, msf')
        Nothing        -> return (Nothing, msf)
-}

untilMaybe :: Monad m => MStreamF m a b -> MStreamF m b Bool -> MStreamF (MaybeT m) a b
untilMaybe msf cond = proc a -> do
    b <- liftMStreamFTrans msf -< a
    c <- liftMStreamFTrans cond -< b
    inMaybeT -< if c then Nothing else Just b

catchMaybe :: Monad m => MStreamF (MaybeT m) a b -> MStreamF m a b -> MStreamF m a b
catchMaybe msf1 msf2 = MStreamF $ \a -> do
    cont <- runMaybeT $ unMStreamF msf1 a
    case cont of
        Just (b, msf1') -> return (b, msf1' `catchMaybe` msf2)
        Nothing         -> unMStreamF msf2 a


-- * Exception monad

{-
catchS' :: Monad m => MStreamF (ExceptT e m) a b -> (e -> m (b, MStreamF m a b)) -> MStreamF m a b
catchS' msf f = MStreamF $ \a -> (unMStreamF msf a) f `catchFinal` f
-}
catchS :: Monad m => MStreamF (ExceptT e m) a b -> (e -> MStreamF m a b) -> MStreamF m a b
catchS msf f = MStreamF $ \a -> do
    cont <- runExceptT $ unMStreamF msf a
    case cont of
        Left e          -> unMStreamF (f e) a
        Right (b, msf') -> return (b, msf' `catchS` f)

exceptS :: Monad m => MStreamF (ExceptT e m) a b -> MStreamF m a (Either e b)
exceptS msf = go
 where
   go = MStreamF $ \a -> do
          cont <- runExceptT $ unMStreamF msf a
          case cont of
            Left e          -> return (Left e,  go)
            Right (b, msf') -> return (Right b, exceptS msf')

-- catchFinal :: Monad m => ExceptT e m a -> (e -> m a) -> m a
-- catchFinal action f = do
--     ea <- runExceptT action
--     case ea of
--         Left  e -> f e
--         Right a -> return a


throwOnCond :: Monad m => (a -> Bool) -> e -> MStreamF (ExceptT e m) a a
throwOnCond cond e = proc a -> if cond a
    then liftMStreamF throwE -< e
    else returnA -< a

throwOnCondM :: Monad m => (a -> m Bool) -> e -> MStreamF (ExceptT e m) a a
throwOnCondM cond e = proc a -> do
    b <- liftMStreamF (lift . cond) -< a
    if b
    then liftMStreamF throwE -< e
    else returnA -< a


throwOn :: Monad m => e -> MStreamF (ExceptT e m) Bool ()
throwOn e = proc b -> throwOn' -< (b, e)

throwOn' :: Monad m => MStreamF (ExceptT e m) (Bool, e) ()
throwOn' = proc (b, e) -> if b
    then liftMStreamF throwE -< e
    else returnA -< ()

-- Similar to delayed switching. Looses a b in case of exception
untilE :: Monad m => MStreamF m a b -> MStreamF m b (Maybe e)
       -> MStreamF (ExceptT e m) a b
untilE msf msfe = proc a -> do
    b <- liftMStreamFTrans msf -< a
    me <- liftMStreamFTrans msfe -< b
    inExceptT -< (ExceptT . return) (maybe (Right b) Left me)

throwMaybe :: Monad m => MStreamF (ExceptT e m) (Maybe e) (Maybe a)
throwMaybe = mapMaybeS $ liftMStreamF throwE

throwS :: Monad m => MStreamF (ExceptT e m) e a
throwS = liftMStreamF throwE

inExceptT :: Monad m => MStreamF (ExceptT e m) (ExceptT e m a) a
inExceptT = liftMStreamF id -- extracts value from monadic action

-- * List monad

-- Name alternative (in the article): collect
widthFirst :: (Functor m, Monad m) => MStreamF (ListT m) a b -> MStreamF m a [b]
widthFirst msf = widthFirst' [msf] where
    widthFirst' msfs = MStreamF $ \a -> do
        (bs, msfs') <- unzip . concat <$> (sequence $ map (runListT . flip unMStreamF a) msfs)
        return (bs, widthFirst' msfs')


-- Name alternatives: "choose", "parallely" (problematic because it's not multicore)
sequenceS :: Monad m => [MStreamF m a b] -> MStreamF (ListT m) a b
sequenceS msfs = MStreamF $ \a -> ListT $ sequence $ apply a <$> msfs
  where
    apply a msf = do
        (b, msf') <- unMStreamF msf a
        return (b, sequenceS [msf'])
-- sequenceS = foldl (<+>) arrowzero . map liftMStreamFTrans
