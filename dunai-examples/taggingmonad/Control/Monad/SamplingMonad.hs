{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- A monad to capture when the next sample should be taken in an interactive
-- system implemented using MSFs, to prevent missing collisions or other
-- events.
module Control.Monad.SamplingMonad where

import Control.Monad.TaggingMonad
import Data.Monoid
import Data.Maybe.Util

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup as Sem
#endif

type SamplingMonad t a = TaggingMonad (NextSample t) a

data NextSample a = Ord a => NextSample { unNext :: Maybe a }

#if MIN_VERSION_base(4,9,0)
instance Ord a => Semigroup (NextSample a) where
  (NextSample x) <> (NextSample y) = NextSample $ mergeMaybe min x y
#endif

instance Ord a => Monoid (NextSample a) where
  mempty = NextSample Nothing

#if !(MIN_VERSION_base(4,9,0))
  mappend (NextSample x) (NextSample y) = NextSample $ mergeMaybe min x y
#elif !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (Sem.<>)
#endif
