{-# LANGUAGE Arrows              #-}
{-# LANGUAGE Rank2Types          #-}
module Control.Monad.Trans.MSF.Except
  ( module Control.Monad.Trans.MSF.Except
  , module Control.Monad.Trans.Except
  ) where

-- External

import           Control.Applicative
import qualified Control.Category           as Category
import           Control.Monad              (liftM, ap)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except hiding (liftCallCC, liftListen, liftPass) -- Avoid conflicting exports
import           Control.Monad.Trans.Maybe

-- Internal
-- import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction

-- * Throwing exceptions

-- | Throw the exception 'e' whenever the function evaluates to 'True'.
throwOnCond :: Monad m => (a -> Bool) -> e -> MSF (ExceptT e m) a a
throwOnCond cond e = proc a -> if cond a
  then throwS  -< e
  else returnA -< a

-- | Variant of 'throwOnCond' for Kleisli arrows.
-- | Throws the exception when the input is 'True'.
throwOnCondM :: Monad m => (a -> m Bool) -> e -> MSF (ExceptT e m) a a
throwOnCondM cond e = proc a -> do
  b <- arrM (lift . cond) -< a
  if b
    then throwS  -< e
    else returnA -< a

-- | Throw the exception when the input is 'True'.
throwOn :: Monad m => e -> MSF (ExceptT e m) Bool ()
throwOn e = proc b -> throwOn' -< (b, e)

-- | Variant of 'throwOn', where the exception may change every tick.
throwOn' :: Monad m => MSF (ExceptT e m) (Bool, e) ()
throwOn' = proc (b, e) -> if b
  then throwS  -< e
  else returnA -< ()

-- | When the input is 'Just e', throw the exception 'e'.
--   (Does not output any actual data.)
throwMaybe :: Monad m => MSF (ExceptT e m) (Maybe e) (Maybe a)
throwMaybe = mapMaybeS throwS

-- | Immediately throw the incoming exception.
throwS :: Monad m => MSF (ExceptT e m) e a
throwS = arrM throwE

-- | Immediately throw the given exception.
throw :: Monad m => e -> MSF (ExceptT e m) a b
throw = arrM_ . throwE

-- | Do not throw an exception.
pass :: Monad m => MSF (ExceptT e m) a a
pass = Category.id

-- | Whenever 'Nothing' is thrown, throw '()' instead.
maybeToExceptS :: Monad m => MSF (MaybeT m) a b -> MSF (ExceptT () m) a b
maybeToExceptS = liftMSFPurer (ExceptT . (maybe (Left ()) Right <$>) . runMaybeT)

-- * Catching exceptions

{-
catchS' :: Monad m => MSF (ExceptT e m) a b -> (e -> m (b, MSF m a b)) -> MSF m a b
catchS' msf f = MSF $ \a -> (unMSF msf a) f `catchFinal` f
-}

-- catchFinal :: Monad m => ExceptT e m a -> (e -> m a) -> m a
-- catchFinal action f = do
--     ea <- runExceptT action
--     case ea of
--         Left  e -> f e
--         Right a -> return a

-- | Catch an exception in an 'MSF'. As soon as an exception occurs,
--   the current continuation is replaced by a new 'MSF', the exception handler,
--   based on the exception value.
--   For exception catching where the handler can throw further exceptions,
--   see 'MSFExcept' further below.
catchS :: Monad m => MSF (ExceptT e m) a b -> (e -> MSF m a b) -> MSF m a b
catchS msf f = MSF $ \a -> do
  cont <- runExceptT $ unMSF msf a
  case cont of
    Left e          -> unMSF (f e) a
    Right (b, msf') -> return (b, msf' `catchS` f)

-- | Similar to Yampa's delayed switching. Looses a 'b' in case of an exception.
untilE :: Monad m => MSF m a b -> MSF m b (Maybe e)
       -> MSF (ExceptT e m) a b
untilE msf msfe = proc a -> do
  b  <- liftMSFTrans msf  -< a
  me <- liftMSFTrans msfe -< b
  inExceptT -< ExceptT $ return $ maybe (Right b) Left me

-- | Escape an 'ExceptT' layer by outputting the exception whenever it occurs.
--   If an exception occurs, the current 'MSF' continuation is tested again
--   on the next input.
exceptS :: Monad m => MSF (ExceptT e m) a b -> MSF m a (Either e b)
exceptS msf = go
 where
   go = MSF $ \a -> do
          cont <- runExceptT $ unMSF msf a
          case cont of
            Left e          -> return (Left e,  go)
            Right (b, msf') -> return (Right b, exceptS msf')

-- | Embed an 'ExceptT' value inside the 'MSF'.
--   Whenever the input value is an ordinary value,
--   it is passed on. If it is an exception, it is raised.
inExceptT :: Monad m => MSF (ExceptT e m) (ExceptT e m a) a
inExceptT = arrM id

-- | In case an exception occurs in the first argument,
--   replace the exception by the second component of the tuple.
tagged :: Monad m => MSF (ExceptT e1 m) a b -> MSF (ExceptT e2 m) (a, e2) b
tagged msf = MSF $ \(a, e2) -> ExceptT $ do
  cont <- runExceptT $ unMSF msf a
  case cont of
    Left e1 -> return $ Left e2
    Right (b, msf') -> return $ Right (b, tagged msf')


-- * Monad interface for Exception MSFs

-- | 'MSF's with an 'ExceptT' transformer layer
--   are in fact monads /in the exception type/.
--
--   * 'return' corresponds to throwing an exception immediately.
--   * '(>>=)' is exception handling:
--     The first value throws an exception,
--     while the Kleisli arrow handles the exception
--     and produces a new signal function,
--     which can throw exceptions in a different type.
newtype MSFExcept m a b e = MSFExcept { runMSFExcept :: MSF (ExceptT e m) a b }

-- | An alias for the |MSFExcept| constructor,
-- used to enter the |MSFExcept| monad context.
-- Execute an 'MSF' in 'ExceptT' until it raises an exception.
try :: MSF (ExceptT e m) a b -> MSFExcept m a b e
try = MSFExcept

-- | Immediately throw the current input as an exception.
currentInput :: Monad m => MSFExcept m e b e
currentInput = try throwS

instance Monad m => Functor (MSFExcept m a b) where
  fmap = liftM

instance Monad m => Applicative (MSFExcept m a b) where
  pure = MSFExcept . throw
  (<*>) = ap

instance Monad m => Monad (MSFExcept m a b) where
  MSFExcept msf >>= f = MSFExcept $ MSF $ \a -> do
    cont <- lift $ runExceptT $ unMSF msf a
    case cont of
      Left e          -> unMSF (runMSFExcept $ f e) a
      Right (b, msf') -> return (b, runMSFExcept $ try msf' >>= f)

-- | The empty type. As an exception type, it encodes "no exception possible".
data Empty

-- | If no exception can occur, the 'MSF' can be executed without the 'ExceptT' layer.
safely :: Monad m => MSFExcept m a b Empty -> MSF m a b
safely (MSFExcept msf) = safely' msf
  where
    safely' msf = MSF $ \a -> do
      Right (b, msf') <- runExceptT $ unMSF msf a
      return (b, safely' msf')

-- | An 'MSF' without an 'ExceptT' layer never throws an exception,
--   and can thus have an arbitrary exception type.
safe :: Monad m => MSF m a b -> MSFExcept m a b e
safe = try . liftMSFTrans

-- | Inside the 'MSFExcept' monad, execute an action of the wrapped monad.
--   This passes the last input value to the action,
--   but doesn't advance a tick.
once :: Monad m => (a -> m e) -> MSFExcept m a b e
once f = try $ arrM (lift . f) >>> throwS

-- | Variant of 'once' without input.
once_ :: Monad m => m e -> MSFExcept m a b e
once_ = once . const

-- | Advances a single tick with the given Kleisli arrow,
--   and then throws an exception.
step :: Monad m => (a -> m (b, e)) -> MSFExcept m a b e
step f = try $ proc a -> do
  n      <- count           -< ()
  (b, e) <- arrM (lift . f) -< a
  _      <- throwOn'        -< (n > (1 :: Int), e)
  returnA                   -< b
