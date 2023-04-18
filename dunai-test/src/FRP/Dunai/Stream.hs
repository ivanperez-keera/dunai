-- |
-- Copyright  : (c) Ivan Perez, 2017-2023
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Streams and stream manipulation API.
--
-- The evaluation of Dunai MSFs, especially for testing purposes, needs the
-- generation of suitable input streams.
--
-- While some streams can be generated randomly using QuickCheck, it is
-- sometimes useful to be able to preprend or adapt an input stream. It is also
-- useful to debug programs when you have recorded input streams using Haskell
-- Titan.
--
-- This module defines types for input streams, as well as an API to create,
-- examine and combine streams. It also provides evaluation functions that are
-- needed to apply an MSF to a stream and obtain an output stream and a
-- continuation MSF.
module FRP.Dunai.Stream where

-- External imports
import Control.Monad.Trans.MSF.Reader          (ReaderT, readerS, runReaderS)
import Data.MonadicStreamFunction              (MSF)
import Data.MonadicStreamFunction.InternalCore (unMSF)

-- * Types

-- | A stream of samples, with their sampling times.
type SignalSampleStream a = SampleStream (DTime, a)

-- | A stream of samples, with no sampling time.
type SampleStream a = [a]

-- | DTime is the time type for lengths of sample intervals. Conceptually,
-- DTime = R+ = { x in R | x > 0 }.
type DTime    = Double


-- ** Creation

-- | Group a series of samples with a series of time deltas.
--
-- The first sample will have no delta. Unused samples and deltas will be
-- dropped.
groupDeltas :: [a] -> [DTime] -> SignalSampleStream a
groupDeltas xs ds = zip (0:ds) xs

-- * Obtain samples

-- | Turn a stream with sampling times into a list of values.
samples :: SignalSampleStream a -> [a]
samples = map snd

-- | Return the first sample in a signal sample stream.
firstSample :: SignalSampleStream a -> a
firstSample = head . samples

-- | Return the last sample in a signal sample stream.
lastSample :: SignalSampleStream a -> a
lastSample = last . samples

-- * Stream manipulation

-- ** Merging

-- | Merge two streams, using an auxilary function to merge samples that fall
-- at the exact same sampling time.
sMerge :: (a -> a -> a) -> SignalSampleStream a -> SignalSampleStream a -> SignalSampleStream a
sMerge _ []              xs2             = xs2
sMerge _ xs1             []              = xs1
sMerge f ((dt1, x1):xs1) ((dt2, x2):xs2)
  | dt1 == dt2 = (dt1, f x1 x2) : sMerge f xs1 xs2
  | dt1 <  dt2 = (dt1, x1) : sMerge f xs1 ((dt2-dt1, x2):xs2)
  | otherwise  = (dt2, x2) : sMerge f ((dt1-dt2, x1):xs1) xs2

-- ** Concatenating

-- | Concatenate two sample streams, separating them by a given time delta.
sConcat :: SignalSampleStream a -> SignalSampleStream a -> SignalSampleStream a
sConcat xs1 xs2 = xs1 ++ xs2

-- ** Refining

-- | Refine a signal sample stream by establishing the maximum time delta.
--
-- If two samples are separated by a time delta bigger than the given max DT,
-- the former is replicated as many times as necessary.
sRefine :: DTime -> a -> SignalSampleStream a -> SignalSampleStream a
sRefine _     _ [] = []
sRefine maxDT a0 ((dt, a):as)
  | dt > maxDT = (maxDT, a0) : sRefine maxDT a0 ((dt - maxDT, a):as)
  | otherwise  = (dt, a) : sRefine maxDT a as

-- | Refine a stream by establishing the maximum time delta.
--
-- If two samples are separated by a time delta bigger than the given max DT,
-- the auxiliary interpolation function is used to determine the intermendiate
-- sample.
refineWith :: (a -> a -> a) -> DTime -> a -> SignalSampleStream a -> SignalSampleStream a
refineWith _           _     _  [] = []
refineWith interpolate maxDT a0 ((dt, a):as)
  | dt > maxDT = let a' = interpolate a0 a
                 in (maxDT, interpolate a0 a) : refineWith interpolate maxDT a' ((dt - maxDT, a):as)
  | otherwise  = (dt, a) : refineWith interpolate maxDT a as

-- ** Clipping (dropping samples)

-- | Clip a signal sample stream at a given number of samples.
sClipAfterFrame :: Int -> SignalSampleStream a -> SignalSampleStream a
sClipAfterFrame = take

-- | Clip a signal sample stream after a certain (non-zero) time.
sClipAfterTime :: DTime -> SignalSampleStream a -> SignalSampleStream a
sClipAfterTime _  [] = []
sClipAfterTime dt ((dt',x):xs)
  | dt < dt'  = []
  | otherwise = (dt', x) : sClipAfterTime (dt - dt') xs

-- | Drop the first n samples of a signal sample stream. The time deltas are
-- not re-calculated.
sClipBeforeFrame :: Int -> SignalSampleStream a -> SignalSampleStream a
sClipBeforeFrame 0 xs@(_:_) = xs
sClipBeforeFrame _ xs@[_]   = xs
sClipBeforeFrame n xs       = sClipBeforeFrame (n-1) xs

-- | Drop the first samples of a signal sample stream up to a given time. The
-- time deltas are not re-calculated to match the original stream.
sClipBeforeTime  :: DTime -> SignalSampleStream a -> SignalSampleStream a
sClipBeforeTime dt xs
  | dt <= 0   = xs
  | otherwise = case xs of
                  [_]              -> xs
                  (_:(dt',x'):xs') -> if | dt < dt'  -> ((dt'- dt, x'):xs')
                                         | otherwise -> sClipBeforeTime (dt - dt') ((0,x'):xs')


-- | Evaluate an SF with a 'SignalSampleStream', obtaining an output stream and
-- a continuation.
--
-- You should never use this for actual execution in your applications, only
-- for testing.
evalSF :: Monad m
       => MSF (ReaderT DTime m) a b
       -> SignalSampleStream a
       -> m (SampleStream b, MSF (ReaderT DTime m) a b)
evalSF fsf as = do
  let msf'' = runReaderS fsf
  (ss, msf') <- evalMSF msf'' as
  return (ss, readerS msf')


-- | Evaluate an MSF with a 'SampleStream', obtaining an output stream and a
-- continuation.
--
-- You should never use this for actual execution in your applications, only
-- for testing.
evalMSF :: Monad m
        => MSF m a b
       -> SampleStream a
       -> m (SampleStream b, MSF m a b)
evalMSF fsf [] = return ([], fsf)
evalMSF fsf (a:as) = do
  (b, fsf')   <- unMSF fsf a
  (bs, fsf'') <- evalMSF fsf' as
  let outputStrm  = b : bs
  return (outputStrm, fsf'')
