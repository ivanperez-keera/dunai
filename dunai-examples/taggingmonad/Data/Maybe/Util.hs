-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Auxiliary module with utility functions related to Data.Maybe.
module Data.Maybe.Util where

mergeMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeMaybe f Nothing x         = x
mergeMaybe f x       Nothing   = x
mergeMaybe f (Just x) (Just y) = Just $ f x y
