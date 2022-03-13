{-# LANGUAGE PackageImports #-}
import Data.IORef
import "bearriver" FRP.Yampa
-- import "Yampa" FRP.Yampa

main = do
  ref <- newIORef 0.0
  reactimate senseI (senseRest ref) (actuate ref) sf

senseI = do
  putStrLn "SenseI"
  return 45

senseRest ref _ = do
  v <- readIORef ref
  putStrLn "SenseRest"
  return (0.1, Just v)

actuate ref _ x = do
  v <- readIORef ref
  putStrLn $ "actuate " ++ show x
  writeIORef ref (v+1)
  return (v >= 3)

sf :: SF Double Double
sf = identity
