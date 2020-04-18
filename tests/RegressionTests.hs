module Main where

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Writer.Lazy

-- dunai
import Control.Monad.Trans.MSF.Maybe
import Data.MonadicStreamFunction

-- tasty
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "listToMaybeS"
    [ testCase "First emits all list elements before throwing exception" $
        "Hello" @?= (execWriter $ runMaybeT $ flip embed [(1 :: Integer)..] $ listToMaybeS ["H", "el", "lo"] >>> arrM (tell >>> lift))
    ]
