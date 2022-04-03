-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
module Data.MonadicStreamFunction.Instances.Ultrametric where

import Data.MonadicStreamFunction.Core

data Nat = Zero | Succ Nat

cauchy :: Real r => (Nat -> r) -> r
cauchy = undefined

class Ultrametric a where
    dist :: Real b => a -> a -> b
    sup :: Real b => (a -> b) -> b
    -- Because deriving this from Cauchy sequences would be quite unwieldy.
    -- Scary. This is the continuation type.

instance (Ultrametric a, Ultrametric b) => Ultrametric (a, b) where
    dist (a1, b1) (a2, b2) = max (dist a1 a2) (dist b1 b2)

instance (Ultrametric a, Ultrametric b) => Ultrametric (Either a b) where
    dist (Left a1) (Left a2) = dist a1 a2
    dist (Right b1) (Right b2) = dist b1 b2
    dist _ _ = 1

instance (Ultrametric a, Ultrametric b) => Ultrametric (a -> b) where
    dist f1 f2 = sup $ \a -> dist (f1 a) (f2 a)

newtype Half a = Half a deriving Functor

unzipHalf :: Half (a, b) -> (Half a, Half b)

instance Ultrametric a => Ultrametric (Half a) where
    dist (Half a1) (Half a2) = dist a1 a2 / 2

delay :: a -> Half a
delay = Half

instance Ultrametric a => Ultrametric (MStream Half a) where
    dist as1 as2 = dist (unMStreamF as1 ()) (unMStreamF as2 ())
