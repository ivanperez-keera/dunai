-- | This module contains operations on monadic streams that are asynchronous,
--   i.e. that change the speed at which data enters or leaves the 'MSF'.

module Data.MonadicStreamFunction.Async where

-- Internal
import Data.MonadicStreamFunction.Core
import Data.MonadicStreamFunction.InternalCore
import Data.MonadicStreamFunction.Util (MStream)

{- |
Concatenates a monadic stream of lists to a monadic stream.
The stream of lists will be called exactly when new data is needed.

Example:

>>> let intstream = constS $ putStrLn "Enter a list of Ints:" >> readLn :: MStream IO [Int]
>>> reactimate $ concatS intstream >>> arrM print
Enter a list of Ints:
[1,2,33]
1
2
33
Enter a list of Ints:
[]
Enter a list of Ints:
[]
Enter a list of Ints:
[1,2]
1
2
Enter a list of Ints:
...

Beware that @concatS msf@ becomes unproductive when @msf@ starts outputting empty lists forever.
This is ok:

>>> let boolToList b = if b then ["Yes"] else []
>>> let everyOddEmpty = count >>> arr (even >>> boolToList)
>>> reactimate $ concatS everyOddEmpty >>> arrM print
"Yes"
"Yes"
"Yes"
"Yes"
"Yes"
...

But this will be caught in a loop:

>>> let after3Empty = count >>> arr ((<= 3) >>> boolToList)
>>> reactimate $ concatS after3Empty  >>> arrM print
"Yes"
"Yes"
"Yes"
^CInterrupted.
-}
concatS :: Monad m => MStream m [b] -> MStream m b
concatS msf = MSF $ \_ -> tick msf []
  where
    tick msf' (b:bs) = return (b, MSF $ \_ -> tick msf' bs)
    tick msf' []     = do
      (bs, msf'') <- unMSF msf' ()
      tick msf'' bs
-- TODO Maybe this can be written more nicely with exceptions?
-- Similarly takeS :: Int -> MSF m a b -> MSFExcept m a b () throws an exception after n ticks
-- Or with merge?
