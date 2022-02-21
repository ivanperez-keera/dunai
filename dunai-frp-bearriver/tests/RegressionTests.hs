module Main where

-- transformers
import Control.Monad.Trans.State.Lazy

-- bearriver
import FRP.Yampa

-- tasty
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup " Reactimate"
    [ testCase "reactimate where senseRest produces Nothing on first tick" $
        run senseI senseRest_0 actuate identity @?= [0, 0]
    , testCase "reactimate where senseRest produces Just a on first tick" $
        run senseI senseRest_1 actuate identity @?= [0, 99]
    ] 
    where
        run senseI' senseRest' actuate' sf = reverse (snd (flip execState (0,[]) $ reactimate senseI' senseRest' actuate' sf))

senseI :: Monad m => m Int
senseI = return 0

actuate :: Bool -> Int -> State (Int , [Int]) Bool
actuate _ x = do
  (v, xs) <- get
  put (v+1, x : xs)
  return (v >= 1)

senseRest_0 :: Monad m => Bool -> m (DTime, Maybe Int)
senseRest_0 _ = return (0.1, Nothing)

senseRest_1 :: Monad m => Bool -> m (DTime, Maybe Int)
senseRest_1 _ = return (0.1, Just 99)


