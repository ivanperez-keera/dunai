{-# LANGUAGE ExistentialQuantification #-}
module Control.Monad.SamplingMonad where

import Control.Monad.TaggingMonad
import Data.Monoid
import Data.Maybe.Util

type SamplingMonad t a = TaggingMonad (NextSample t) a

data NextSample a = Ord a => NextSample { unNext :: Maybe a }

instance Ord a => Monoid (NextSample a) where
 mempty = NextSample Nothing
 mappend (NextSample x) (NextSample y) = NextSample $ mergeMaybe min x y
