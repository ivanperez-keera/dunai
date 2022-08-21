{-# LANGUAGE Arrows     #-}
{-# LANGUAGE CPP        #-}
{-# LANGUAGE Rank2Types #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- 'MSF's in the 'ExceptT' monad are monadic stream functions that can throw
-- exceptions, i.e. return an exception value instead of a continuation. This
-- module gives ways to throw exceptions in various ways, and to handle them
-- through a monadic interface.
module Control.Monad.Trans.MSF.Except
    ( module Control.Monad.Trans.MSF.Except
    , module Control.Monad.Trans.Except
    )
  where

-- External imports
#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        (Applicative (..), (<$>))
#endif

import           Control.Arrow              (arr, returnA, (<<<), (>>>))
import qualified Control.Category           as Category
import           Control.Monad              (ap, liftM)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except hiding (liftCallCC, liftListen,
                                             liftPass)
import           Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)
import           Data.Void                  (Void)

-- Internal imports
import Data.MonadicStreamFunction              (arrM, constM, count, feedback,
                                                liftTransS, mapMaybeS, morphS,
                                                reactimate)
import Data.MonadicStreamFunction.InternalCore (MSF (MSF, unMSF))

-- External, necessary for older base versions
#if !MIN_VERSION_base(4,10,0)
fromLeft  :: a -> Either a b -> a
fromLeft  _ (Left  a) = a
fromLeft  a (Right _) = a
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b (Left  _) = b
#else
import           Data.Either                (fromLeft, fromRight)
#endif

-- * Throwing exceptions

-- | Throw the exception 'e' whenever the function evaluates to 'True'.
throwOnCond :: Monad m => (a -> Bool) -> e -> MSF (ExceptT e m) a a
throwOnCond cond e = proc a -> if cond a
  then throwS  -< e
  else returnA -< a

-- | Throws the exception when the input is 'True'. Variant of 'throwOnCond'
-- for Kleisli arrows.
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

-- | When the input is @Just e@, throw the exception @e@. (Does not output any
-- actual data.)
throwMaybe :: Monad m => MSF (ExceptT e m) (Maybe e) (Maybe a)
throwMaybe = mapMaybeS throwS

-- | Immediately throw the incoming exception.
throwS :: Monad m => MSF (ExceptT e m) e a
throwS = arrM throwE

-- | Immediately throw the given exception.
throw :: Monad m => e -> MSF (ExceptT e m) a b
throw = constM . throwE

-- | Do not throw an exception.
pass :: Monad m => MSF (ExceptT e m) a a
pass = Category.id

-- | Converts an 'MSF' in 'MaybeT' to an 'MSF' in 'ExceptT'. Whenever
-- 'Nothing' is thrown, throw @()@ instead.
maybeToExceptS :: (Functor m, Monad m)
               => MSF (MaybeT m) a b -> MSF (ExceptT () m) a b
maybeToExceptS = morphS (ExceptT . (maybe (Left ()) Right <$>) . runMaybeT)

-- * Catching exceptions

-- | Catch an exception in an 'MSF'. As soon as an exception occurs, the
-- current continuation is replaced by a new 'MSF', the exception handler,
-- based on the exception value. For exception catching where the handler can
-- throw further exceptions, see 'MSFExcept' further below.
catchS :: Monad m => MSF (ExceptT e m) a b -> (e -> MSF m a b) -> MSF m a b
catchS msf f = safely $ do
  e <- try msf
  safe $ f e

-- | Similar to Yampa's delayed switching. Loses a @b@ in case of an exception.
untilE :: Monad m
       => MSF m a b
       -> MSF m b (Maybe e)
       -> MSF (ExceptT e m) a b
untilE msf msfe = proc a -> do
  b  <- liftTransS msf  -< a
  me <- liftTransS msfe -< b
  inExceptT -< ExceptT $ return $ maybe (Right b) Left me

-- | Escape an 'ExceptT' layer by outputting the exception whenever it occurs.
-- If an exception occurs, the current 'MSF' continuation is tested again on
-- the next input.
exceptS :: (Functor m, Monad m) => MSF (ExceptT e m) a b -> MSF m a (Either e b)
exceptS = transG return $ const $ fmap f . runExceptT
  where
    f (Left e)       = (Left e , Nothing)
    f (Right (b, c)) = (Right b, Just c )

-- | Embed an 'ExceptT' value inside the 'MSF'. Whenever the input value is an
-- ordinary value, it is passed on. If it is an exception, it is raised.
inExceptT :: Monad m => MSF (ExceptT e m) (ExceptT e m a) a
inExceptT = arrM id

-- | In case an exception occurs in the first argument, replace the exception
-- by the second component of the tuple.
tagged :: Monad m => MSF (ExceptT e1 m) a b -> MSF (ExceptT e2 m) (a, e2) b
tagged msf = runMSFExcept $ do
  _       <- try $ msf <<< arr fst
  (_, e2) <- currentInput
  return e2

-- * Monad interface for Exception MSFs

-- | 'MSF's with an 'ExceptT' transformer layer are in fact monads /in the
-- exception type/.
--
--   * 'return' corresponds to throwing an exception immediately.
--   * '>>=' is exception handling: The first value throws an exception, while
--     the Kleisli arrow handles the exception and produces a new signal
--     function, which can throw exceptions in a different type.
--   * @m@: The monad that the 'MSF' may take side effects in.
--   * @a@: The input type
--   * @b@: The output type
--   * @e@: The type of exceptions that can be thrown
newtype MSFExcept m a b e = MSFExcept { runMSFExcept :: MSF (ExceptT e m) a b }

-- | Execute an 'MSF' in 'ExceptT' until it raises an exception.
--
-- An alias for the 'MSFExcept' constructor, used to enter the 'MSFExcept'
-- monad context.
try :: MSF (ExceptT e m) a b -> MSFExcept m a b e
try = MSFExcept

-- | Immediately throw the current input as an exception.
currentInput :: Monad m => MSFExcept m e b e
currentInput = try throwS

-- | Functor instance for MSFs on the 'Either' monad. Fmapping is the same as
-- applying a transformation to the 'Left' values.
instance Monad m => Functor (MSFExcept m a b) where
  fmap = liftM

-- | Applicative instance for MSFs on the 'Either' monad. The function 'pure'
-- throws an exception.
instance Monad m => Applicative (MSFExcept m a b) where
  pure = MSFExcept . throw
  (<*>) = ap

-- | Monad instance for 'MSFExcept'. Bind uses the exception as the 'return'
-- value in the monad.
instance Monad m => Monad (MSFExcept m a b) where
  return = pure
  MSFExcept msf >>= f = MSFExcept $ handleExceptT msf $ runMSFExcept . f

-- | Execute an MSF and, if it throws an exception, recover by switching to a
-- second MSF.
handleExceptT :: Monad m
              => MSF (ExceptT e1 m) a b
              -> (e1 -> MSF (ExceptT e2 m) a b)
              -> MSF (ExceptT e2 m) a b
handleExceptT msf f = flip handleGen msf $ \a mbcont -> do
  ebcont <- lift $ runExceptT mbcont
  case ebcont of
    Left e          -> unMSF (f e) a
    Right (b, msf') -> return (b, handleExceptT msf' f)

-- | If no exception can occur, the 'MSF' can be executed without the 'ExceptT'
-- layer.
safely :: Monad m => MSFExcept m a b Void -> MSF m a b
safely (MSFExcept msf) = morphS fromExcept msf
  where
    -- We can assume that the pattern @Left e@ will not occur, since @e@ would
    -- have to be of type @Void@.
    fromExcept ma = do
      rightMa <- runExceptT ma
      return $ fromRight (error "safely: Received `Left`") rightMa

-- | An 'MSF' without an 'ExceptT' layer never throws an exception, and can
-- thus have an arbitrary exception type.
safe :: Monad m => MSF m a b -> MSFExcept m a b e
safe = try . liftTransS

-- | Inside the 'MSFExcept' monad, execute an action of the wrapped monad.
-- This passes the last input value to the action, but doesn't advance a tick.
once :: Monad m => (a -> m e) -> MSFExcept m a b e
once f = try $ arrM (lift . f) >>> throwS

-- | Variant of 'once' without input.
once_ :: Monad m => m e -> MSFExcept m a b e
once_ = once . const

-- | Advances a single tick with the given Kleisli arrow, and then throws an
-- exception.
step :: Monad m => (a -> m (b, e)) -> MSFExcept m a b e
step f = try $ proc a -> do
  n      <- count           -< ()
  (b, e) <- arrM (lift . f) -< a
  _      <- throwOn'        -< (n > (1 :: Int), e)
  returnA                   -< b

-- | Advances a single tick outputting the value, and then throws '()'.
step_ :: Monad m => b -> MSFExcept m a b ()
step_ b = step $ const $ return (b, ())

-- | Converts a list to an 'MSFExcept', which outputs an element of the list at
-- each step, throwing '()' when the list ends.
listToMSFExcept :: Monad m => [b] -> MSFExcept m a b ()
listToMSFExcept = mapM_ step_

-- * Utilities definable in terms of 'MSFExcept'

-- | Extract an 'MSF' from a monadic action.
--
-- Runs a monadic action that produces an 'MSF' on the first iteration/step,
-- and uses that 'MSF' as the main signal function for all inputs (including
-- the first one).
performOnFirstSample :: Monad m => m (MSF m a b) -> MSF m a b
performOnFirstSample sfaction = safely $ do
  msf <- once_ sfaction
  safe msf

-- | Reactimates an 'MSFExcept' until it throws an exception.
reactimateExcept :: Monad m => MSFExcept m () () e -> m e
reactimateExcept msfe = do
  leftMe <- runExceptT $ reactimate $ runMSFExcept msfe
  return $ fromLeft (error "reactimateExcept: Received `Right`") leftMe

-- | Reactimates an 'MSF' until it returns 'True'.
reactimateB :: Monad m => MSF m () Bool -> m ()
reactimateB sf = reactimateExcept $ try $ liftTransS sf >>> throwOn ()

-- | Run first MSF until the second value in the output tuple is @Just c@ (for
-- some @c@), then start the second MSF.
--
-- Analog to Yampa's
-- [@switch@](https://hackage.haskell.org/package/Yampa/docs/FRP-Yampa-Switches.html#v:switch),
-- with 'Maybe' instead of @Event@.
switch :: Monad m => MSF m a (b, Maybe c) -> (c -> MSF m a b) -> MSF m a b
switch sf f = catchS ef f
  where
    -- Run sf, throwing an exception if there is a no-Nothing value in the
    -- second element of the pair, and returning the first element otherwise.
    ef = proc a -> do
           (b, me)  <- liftTransS sf  -< a
           throwMaybe                 -< me
           returnA                    -< b

-- | Run first MSF until the second value in the output tuple is @Just c@ (for
-- some @c@), then start the second MSF.
--
-- Analog to Yampa's
-- [@dswitch@](https://hackage.haskell.org/package/Yampa/docs/FRP-Yampa-Switches.html#v:dSwitch),
-- with 'Maybe' instead of @Event@.
dSwitch :: Monad m => MSF m a (b, Maybe c) -> (c -> MSF m a b) -> MSF m a b
dSwitch sf f = catchS ef f
  where
    ef = feedback Nothing $ proc (a, me) -> do
           throwMaybe    -< me
           liftTransS sf -< a

-- | More general lifting combinator that enables recovery. Note that, unlike a
-- polymorphic lifting function @forall a . m a -> m1 a@, this auxiliary
-- function needs to be a bit more structured, and produces a Maybe value. The
-- previous 'MSF' is used if a new one is not produced.
transG :: (Monad m1, Monad m2)
       => (a2 -> m1 a1)
       -> (forall c. a2 -> m1 (b1, c) -> m2 (b2, Maybe c))
       -> MSF m1 a1 b1
       -> MSF m2 a2 b2
transG transformInput transformOutput msf = go
  where
    go = MSF $ \a2 -> do
           (b2, msf') <- transformOutput a2 $ unMSF msf =<< transformInput a2
           case msf' of
             Just msf'' ->
               return (b2, transG transformInput transformOutput msf'')
             Nothing ->
               return (b2, go)

-- | Use a generic handler to handle exceptions in MSF processing actions.
handleGen :: (a -> m1 (b1, MSF m1 a b1) -> m2 (b2, MSF m2 a b2))
          -> MSF m1 a b1
          -> MSF m2 a b2
handleGen handler msf = MSF $ \a -> handler a (unMSF msf a)
