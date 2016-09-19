module BasicExamples where

import Data.MonadicStreamFunction


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
