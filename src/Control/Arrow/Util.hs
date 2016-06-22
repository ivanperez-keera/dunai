module Control.Arrow.Util where

import Control.Arrow

-- Hah! I shall implement this for TimelessSFs and SFs at the same time!
constantly :: Arrow a => b -> a c b
constantly = arr . const
{-# INLINE constantly #-}

-- More strongly bound arrow combinators
infixr 4 <-<
(<-<) :: Arrow a => a c d -> a b c -> a b d
(<-<) = (<<<)
{-# INLINE (<-<) #-}

infixr 4 >->
(>->) :: Arrow a => a b c -> a c d -> a b d
(>->) = (>>>)
{-# INLINE (>->) #-}

-- import Control.Category (id)
-- import Prelude hiding (id)

-- (&&&!) :: Arrow a => a b c -> a b () -> a b c
-- a1 &&&! a2 = (a1 &&& a2) >>> arr fst

-- sink :: Arrow a => a b c -> a c () -> a b c
-- a1 `sink` a2 = a1 >>> (id &&& a2) >>> arr fst
