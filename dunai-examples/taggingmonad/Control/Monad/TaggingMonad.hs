-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Tag values with additional information.
module Control.Monad.TaggingMonad where

import Control.Applicative
import Data.Monoid

data TaggingMonad t a = TaggingMonad
  { tag   :: t
  , value :: a
  }

instance Functor (TaggingMonad t) where
  fmap f (TaggingMonad t v) = TaggingMonad t (f v)

instance Monoid t => Applicative (TaggingMonad t) where
  pure f = TaggingMonad mempty f
  (TaggingMonad t f) <*> (TaggingMonad t' x) =
    (TaggingMonad (mappend t t') (f x))

instance Monoid t => Monad (TaggingMonad t) where
  return x = TaggingMonad mempty x
  (TaggingMonad t x) >>= f =
    let TaggingMonad t' x' = f x
    in TaggingMonad (mappend t t') x'
