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
import Control.Monad.Trans.Maybe

-- Internal
-- import Control.Monad.Trans.MSF.GenLift
import Data.MonadicStreamFunction

-- * Throwing exceptions

throwOnCond :: Monad m => (a -> Bool) -> e -> MSF (ExceptT e m) a a
throwOnCond cond e = proc a -> if cond a
    then arrM throwE -< e
    else returnA -< a

throwOnCondM :: Monad m => (a -> m Bool) -> e -> MSF (ExceptT e m) a a
throwOnCondM cond e = proc a -> do
    b <- arrM (lift . cond) -< a
    if b
      then arrM throwE -< e
      else returnA -< a

throwOn :: Monad m => e -> MSF (ExceptT e m) Bool ()
throwOn e = proc b -> throwOn' -< (b, e)

throwOn' :: Monad m => MSF (ExceptT e m) (Bool, e) ()
throwOn' = proc (b, e) -> if b
    then arrM throwE -< e
    else returnA -< ()

throwMaybe :: Monad m => MSF (ExceptT e m) (Maybe e) (Maybe a)
throwMaybe = mapMaybeS $ arrM throwE

throwS :: Monad m => MSF (ExceptT e m) e a
throwS = arrM throwE

throw :: Monad m => e -> MSF (ExceptT e m) a b
throw = arrM_ . throwE

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
catchS :: Monad m => MSF (ExceptT e m) a b -> (e -> MSF m a b) -> MSF m a b
catchS msf f = MSF $ \a -> do
    cont <- runExceptT $ unMSF msf a
    case cont of
        Left e          -> unMSF (f e) a
        Right (b, msf') -> return (b, msf' `catchS` f)

-- catchFinal :: Monad m => ExceptT e m a -> (e -> m a) -> m a
-- catchFinal action f = do
--     ea <- runExceptT action
--     case ea of
--         Left  e -> f e
--         Right a -> return a

-- Similar to delayed switching. Looses a b in case of exception
untilE :: Monad m => MSF m a b -> MSF m b (Maybe e)
       -> MSF (ExceptT e m) a b
untilE msf msfe = proc a -> do
    b <- liftMSFTrans msf -< a
    me <- liftMSFTrans msfe -< b
    inExceptT -< (ExceptT . return) (maybe (Right b) Left me)

exceptS :: Monad m => MSF (ExceptT e m) a b -> MSF m a (Either e b)
exceptS msf = go
 where
   go = MSF $ \a -> do
          cont <- runExceptT $ unMSF msf a
          case cont of
            Left e          -> return (Left e,  go)
            Right (b, msf') -> return (Right b, exceptS msf')

inExceptT :: Monad m => MSF (ExceptT e m) (ExceptT e m a) a
inExceptT = arrM id -- extracts value from monadic action

{-
tagged :: MSF (ExceptT e m) a b -> MSF (ExceptT t m) (a, t) b
tagged msf = MSF $ \(a, t) -> ExceptT $ do
  cont <- runExceptT $ unMSF msf a
  case cont of
    Left  e     -> _ return t
    Right bmsf' -> _ return bmsf'
    -}

-- * Monad interface for Exception MSFs

newtype MSFExcept m a b e = MSFExcept { runMSFExcept :: MSF (ExceptT e m) a b }

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

data Empty

safely :: Monad m => MSFExcept m a b Empty -> MSF m a b
safely (MSFExcept msf) = safely' msf
  where
    safely' msf = MSF $ \a -> do
      Right (b, msf') <- runExceptT $ unMSF msf a
      return (b, safely' msf')

safe :: Monad m => MSF m a b -> MSFExcept m a b e
safe = try . liftMSFTrans

once :: Monad m => (a -> m e) -> MSFExcept m a b e
once f = try $ arrM (lift . f) >>> throwS

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

tagged :: Monad m => MSF (ExceptT e1 m) a b -> MSF (ExceptT e2 m) (a, e2) b
tagged msf = MSF $ \(a, e2) -> ExceptT $ do
  cont <- runExceptT $ unMSF msf a
  case cont of
    Left  _e1       -> return $ Left e2
    Right (b, msf') -> return $ Right (b, tagged msf')
