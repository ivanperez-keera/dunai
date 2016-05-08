{-# LANGUAGE TypeFamilies           #-}
module Data.MonadicStreamFunction.Instances where

-- External
import Control.Arrow

-- Internal
import Control.Arrow.Util
import Data.MonadicStreamFunction.Core

-- Numerical operations are defined elementwise on the output
elementwise :: Monad m => (b -> c) -> MStreamF m a b -> MStreamF m a c
elementwise f msf = msf >>> arr f

elementwise2 :: Monad m => (b -> c -> d) -> MStreamF m a b -> MStreamF m a c -> MStreamF m a d
elementwise2 op msf1 msf2 = msf1 &&& msf2 >>> arr (uncurry op)
