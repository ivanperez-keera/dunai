module Data.Tuple.Util where

assocR :: ((a, b), c) -> (a, (b, c))
assocR ((a, b), c) = (a, (b, c))

assocL :: (a, (b, c)) -> ((a, b), c)
assocL (a, (b, c)) = ((a, b), c)
