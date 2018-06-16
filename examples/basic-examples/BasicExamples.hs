import Control.Monad.Trans.MSF
import Data.MonadicStreamFunction


import Text.Read (readMaybe)


add :: (Num n, Monad m) => MSF m (n, n) n
add = arr (\(x, y) -> x + y)

testSerial :: MSF IO () ()
testSerial =   liftS (\_ -> getLine)
           >>> (arr id &&& arr reverse)
           >>> liftS print

main :: IO ()
main = reactimate testSerial

summator :: (Num n, Monad m) => MSF m n n
summator = feedback 0 (arr add2)
  where add2 (n, acc) = let n' = n + acc in (n', n')

counter :: (Num n, Monad m) => MSF m () n
counter = arr (const 1) >>> summator


-- | Will sum all integers entered. If a string is encountered that doesn't
--   parse to an integer, an exception is thrown and caught, and the game
--   starts afresh.
testMaybe :: MSF IO () ()
testMaybe = sumGetLines `catchMaybe` testMaybe
  where
    sumGetLines :: MSF (MaybeT IO) () ()
    sumGetLines =   liftS (const getLine)
                >>> arr (readMaybe :: String -> Maybe Integer)
                >>> (maybeExit :: MSF (MaybeT IO) (Maybe Integer) Integer)
                >>> summator
                >>> liftS print
