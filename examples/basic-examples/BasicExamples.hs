module BasicExamples where

import Data.MonadicStreamFunction

type MSF = MStreamF

summator :: (Num n, Monad m) => MSF m n n
summator = feedback 0 (arr add2)
  where add2 (n, acc) = let n' = n + acc in (n', n')

counter :: (Num n, Monad m) => MSF m () n
counter = arr (const 1) >>> summator
