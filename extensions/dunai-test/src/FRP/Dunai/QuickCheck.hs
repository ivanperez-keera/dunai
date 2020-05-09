{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Dunai.QuickCheck where

-- Examples accompanying the ICFP 2017 paper.
--
-- Changes with respect to the paper:
--
-- - The signature of ballTrulyFalling' in the paper was SF () Double. It's
--   been changed to the intended meaning: TPred ()

-- - The function uniDistStreamMaxDT had the wrong type and the name on the
--   paper was: uniDistStream. This has been fixed.

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), pure)
#endif

import Data.Random.Normal
import Data.MonadicStreamFunction
import FRP.Dunai.Stream
import Test.QuickCheck
import Test.QuickCheck.Gen

-- * Random stream generation

-- ** Parameters used to generate random input streams

data Distribution = DistConstant
                  | DistNormal (DTime, DTime)
                  | DistRandom

type Range = (Maybe DTime, Maybe DTime)

type Length = Maybe (Either Int DTime)

-- ** Time delta generation

-- | Generate a random delta according to some required specifications.
generateDeltas :: Distribution -> Range -> Length -> Gen DTime
generateDeltas DistConstant            (mn, mx) len = generateDelta mn mx
generateDeltas DistRandom              (mn, mx) len = generateDelta mn mx
generateDeltas (DistNormal (avg, dev)) (mn, mx) len =
  generateDSNormal avg dev mn mx

-- | Generate one random delta, possibly within a range.
generateDelta :: Maybe DTime -> Maybe DTime -> Gen DTime
generateDelta (Just x)  (Just y)  = choose (x, y)
generateDelta (Just x)  Nothing   = (x+) . getPositive <$> arbitrary
generateDelta Nothing   (Just y)  = choose (2.2251e-308, y)
generateDelta Nothing   Nothing   = getPositive <$> arbitrary

-- | Generate a random delta following a normal distribution,
--   and possibly within a given range.
generateDSNormal :: DTime -> DTime -> Maybe DTime -> Maybe DTime -> Gen DTime
generateDSNormal avg stddev m n = suchThat gen (\x -> mx x && mn x)
  where
    gen = MkGen (\r _ -> let (x,_) = normal' (avg, stddev) r in x)
    mn  = maybe (const True) (<=) m
    mx  = maybe (const True) (>=) n

-- | Generate random samples up until a max time.
timeStampsUntil :: DTime -> Gen [DTime]
timeStampsUntil = timeStampsUntilWith arbitrary

-- | Generate random samples up until a max time, with a given time delta
--   generation function.
timeStampsUntilWith :: Gen DTime -> DTime -> Gen [DTime]
timeStampsUntilWith arb = timeStampsUntilWith' arb []
  where
    -- Generate random samples up until a max time, with a given time delta
    -- generation function, and an initial suffix of time deltas.
    timeStampsUntilWith' :: Gen DTime -> [DTime] -> DTime -> Gen [DTime]
    timeStampsUntilWith' arb acc ds
      | ds < 0    = return acc
      | otherwise = do d <- arb
                       let acc' = acc `seq` (d:acc)
                       acc' `seq` timeStampsUntilWith' arb acc' (ds - d)

-- ** Random stream generation

-- | Generate random stream.
generateStream :: Arbitrary a
               => Distribution
               -> Range
               -> Length
               -> Gen (SignalSampleStream a)
generateStream = generateStreamWith (\_ _ -> arbitrary)

-- | Generate random stream, parameterized by the value generator.
generateStreamWith :: (Int -> DTime -> Gen a)
                   -> Distribution
                   -> Range
                   -> Length
                   -> Gen (SignalSampleStream a)

generateStreamWith arb DistConstant range  len     =
  generateConstantStream arb =<< generateStreamLenDT range len

generateStreamWith arb DistRandom   (m, n) Nothing = do
  l <- arbitrary
  x <- arb 0 0
  ds <- vectorOfWith l (\_ -> generateDelta m n)
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb DistRandom (m, n) (Just (Left l)) = do
  x <- arb 0 0
  ds <- vectorOfWith l (\_ -> generateDelta m n)
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb DistRandom (m, n) (Just (Right maxds)) = do
  ds <- timeStampsUntilWith (generateDelta m n) maxds
  let l = length ds
  x  <- arb 0 0
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb (DistNormal (avg, stddev)) (m, n) Nothing = do
  l <- arbitrary
  x <- arb 0 0
  ds <- vectorOfWith l (\_ -> generateDSNormal avg stddev m n)
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb (DistNormal (avg, stddev)) (m, n) (Just (Left l)) = do
  x <- arb 0 0
  ds <- vectorOfWith l (\_ -> generateDSNormal avg stddev m n)
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

generateStreamWith arb (DistNormal (avg, stddev)) (m, n) (Just (Right maxds)) = do
  ds <- timeStampsUntilWith (generateDSNormal avg stddev m n) maxds
  let l = length ds
  x <- arb 0 0
  let f n = arb n (ds!!(n-1))
  xs <- vectorOfWith l f
  return $ groupDeltas (x:xs) ds

-- | Generate arbitrary stream with fixed length and constant delta.
generateConstantStream :: (Int -> DTime -> Gen a)
                       -> (DTime, Int)
                       -> Gen (SignalSampleStream a)
generateConstantStream arb (x, length) = do
  ys <- vectorOfWith length (`arb` x)
  let ds = repeat x
  return $ groupDeltas ys ds

-- | Generate arbitrary stream
generateStreamLenDT :: (Maybe DTime, Maybe DTime)
                    -> Maybe (Either Int DTime)
                    -> Gen (DTime, Int)
generateStreamLenDT range len = do
  x <- uncurry generateDelta range
  l <- case len of
         Nothing         -> getPositive <$> arbitrary
         Just (Left l)   -> pure l
         Just (Right ds) -> pure (floor (ds / x))
  return (x, l)

-- generateStreamLenDT (Just x,  Just y)  (Just (Left l))   = (,) <$> choose (x, y)        <*> pure l
-- generateStreamLenDT (Just x,  Nothing) (Just (Left l))   = (,) <$> ((x+) <$> arbitrary) <*> pure l
-- generateStreamLenDT (Nothing, Just y)  (Just (Left l))   = (,) <$> choose (0, y)        <*> pure l
-- generateStreamLenDT (Just x,  _)       (Just (Right ts)) = (,) <$> pure x               <*> pure (floor (ts / x))
-- generateStreamLenDT (Just x,  _)       Nothing           = (,) <$> pure x               <*> arbitrary
-- generateStreamLenDT (Nothing, Nothing) Nothing           = (,) <$> arbitrary            <*> arbitrary
-- generateStreamLenDT (Nothing, Nothing) (Just (Left l))   = (,) <$> arbitrary            <*> pure l
-- generateStreamLenDT (Nothing, Nothing) (Just (Right ds)) = f2  <$> arbitrary
--   where
--     f2 l = (ds / fromIntegral l, l)

-- ** Helpers for common cases

-- | Generate a stream of values with uniformly distributed time deltas.
uniDistStream :: Arbitrary a => Gen (SignalSampleStream a)
uniDistStream = generateStream DistRandom (Nothing, Nothing) Nothing

-- | Generate a stream of values with uniformly distributed time deltas, with a
-- max DT.
uniDistStreamMaxDT :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
uniDistStreamMaxDT maxDT =
  generateStream DistRandom (Nothing, Just maxDT) Nothing

-- | Generate a stream of values with a fixed time delta.
fixedDelayStream :: Arbitrary a => DTime -> Gen (SignalSampleStream a)
fixedDelayStream dt = generateStream DistConstant (Just dt, Just dt) Nothing

-- | Generate a stream of values with a fixed time delta.
fixedDelayStreamWith :: Arbitrary a
                     => (DTime -> a)
                     -> DTime
                     -> Gen (SignalSampleStream a)
fixedDelayStreamWith f dt =
    generateStreamWith f' DistConstant (Just dt, Just dt) Nothing
  where
    f' n t = return $ f (fromIntegral n * t)

-- * Extended quickcheck generator

-- | Generates a list of the given length.
vectorOfWith :: Int -> (Int -> Gen a) -> Gen [a]
vectorOfWith k genF = sequence [ genF i | i <- [1..k] ]
