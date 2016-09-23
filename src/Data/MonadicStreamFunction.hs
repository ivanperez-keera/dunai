-- | Monadic Stream Functions are synchronized stream functions
-- with side effects.
module Data.MonadicStreamFunction
  ( module Control.Arrow
  , module X
  )
 where

-- External

import Control.Arrow

-- Internal

import Data.MonadicStreamFunction.Core        as X
import Data.MonadicStreamFunction.Util        as X

-- Internal (Instances)

import Data.MonadicStreamFunction.ArrowChoice ()
import Data.MonadicStreamFunction.ArrowLoop   ()
import Data.MonadicStreamFunction.ArrowPlus   ()
