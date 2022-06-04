-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- Utility functions to work with 'Arrow's.
module Control.Arrow.Util where

-- External imports
import Control.Arrow (Arrow, arr, (&&&), (>>^), (^<<))

-- | Constantly produce the same output.
constantly :: Arrow a => b -> a c b
constantly = arr . const
{-# INLINE constantly #-}

-- * Apply functions at the end

-- | Alternative name for '^<<'.
elementwise :: Arrow a => (c -> d) -> a b c -> a b d
elementwise = (^<<)

-- | Apply a curried function with two arguments to the outputs of two arrows.
elementwise2 :: Arrow a => (c -> d -> e) -> a b c -> a b d -> a b e
elementwise2 op a1 a2 = (a1 &&& a2) >>^ uncurry op
