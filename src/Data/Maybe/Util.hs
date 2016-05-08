module Data.Maybe.Util where

mergeMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
mergeMaybe f Nothing x         = x
mergeMaybe f x       Nothing   = x
mergeMaybe f (Just x) (Just y) = Just $ f x y
