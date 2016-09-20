{-# LANGUAGE Arrows              #-}
{-# LANGUAGE Rank2Types          #-}
module Control.Monad.Trans.MSF.Maybe
  ( module Control.Monad.Trans.MSF.Maybe
  , module Control.Monad.Trans.Maybe
  ) where

-- External
import Control.Monad.Trans.Maybe
  hiding (liftCallCC, liftCatch, liftListen, liftPass) -- Avoid conflicting exports

-- Internal
import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction


runMaybeS'' :: Monad m => MSF (MaybeT m) a b -> MSF m a (Maybe b)
runMaybeS'' = transG transformInput transformOutput
  where
    transformInput       = return
    transformOutput _ m1 = do r <- runMaybeT m1
                              case r of
                                Nothing     -> return (Nothing, Nothing)
                                Just (b, c) -> return (Just b,  Just c)


-- * Throwing Nothing as an exception ("exiting")

exit :: Monad m => MSF (MaybeT m) a b
exit = MSF $ const $ MaybeT $ return Nothing

exitWhen :: Monad m => (a -> Bool) -> MSF (MaybeT m) a a
exitWhen condition = go where
    go = MSF $ \a -> MaybeT $
        if condition a
        then return Nothing
        else return $ Just (a, go)

exitIf :: Monad m => MSF (MaybeT m) Bool ()
exitIf = MSF $ \b -> MaybeT $ return $ if b then Nothing else Just ((), exitIf)

-- Just a is passed along, Nothing causes the whole MSF to exit
maybeExit :: Monad m => MSF (MaybeT m) (Maybe a) a
maybeExit = MSF $ MaybeT . return . fmap (\x -> (x, maybeExit))

inMaybeT :: Monad m => MSF (MaybeT m) (Maybe a) a
inMaybeT = liftMSF $ MaybeT . return

-- * Catching Maybe exceptions

untilMaybe :: Monad m => MSF m a b -> MSF m b Bool -> MSF (MaybeT m) a b
untilMaybe msf cond = proc a -> do
  b <- liftMSFTrans msf -< a
  c <- liftMSFTrans cond -< b
  inMaybeT -< if c then Nothing else Just b

catchMaybe :: Monad m => MSF (MaybeT m) a b -> MSF m a b -> MSF m a b
catchMaybe msf1 msf2 = MSF $ \a -> do
  cont <- runMaybeT $ unMSF msf1 a
  case cont of
    Just (b, msf1') -> return (b, msf1' `catchMaybe` msf2)
    Nothing         -> unMSF msf2 a



mapMaybeS :: Monad m => MSF m a b -> MSF m (Maybe a) (Maybe b)
mapMaybeS msf = go
  where
    go = MSF $ \maybeA -> case maybeA of
                                 Just a -> do
                                     (b, msf') <- unMSF msf a
                                     return (Just b, mapMaybeS msf')
                                 Nothing -> return (Nothing, go)

-- mapMaybeS msf == runMaybeS (inMaybeT >>> lift mapMaybeS)

{-
maybeS :: Monad m => MSF m a (Maybe b) -> MSF (MaybeT m) a b
maybeS msf = MSF $ \a -> MaybeT $ return $ unMSF msf a
-- maybeS msf == lift msf >>> inMaybeT
-}

runMaybeS :: Monad m => MSF (MaybeT m) a b -> MSF m a (Maybe b)
runMaybeS msf = go
  where
    go = MSF $ \a -> do
           bmsf <- runMaybeT $ unMSF msf a
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
-- MSF (MaybeT m) a b.
--
-- Two things you can try (to help you see that this is indeed why GHC is
-- complaining):
--   - Make the second continuation undefined. Then it typechecks.
--   - Use ScopedTypeVariables and a let binding to type msf' in the
--   first branch of the case selector. It'll complain about the type
--   of msf' if you say it's forcibly a MSF (MaybeT m) a b.
--

runMaybeS'' :: Monad m => MSF (MaybeT m) a b -> MSF m a (Maybe b)
runMaybeS'' msf = transS transformInput transformOutput msf
  where
    transformInput  = return
    transformOutput _ msfaction = do
      thing <- runMaybeT msfaction
      case thing of
        Just (b, msf') -> return (Just b, msf')
        Nothing        -> return (Nothing, msf)
-}
