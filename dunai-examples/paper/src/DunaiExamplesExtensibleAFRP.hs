{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright  : (c) Ivan Perez and Manuel Baerenz, 2016
-- License    : BSD3
-- Maintainer : ivan.perez@keera.co.uk
--
-- An example of adding time in a reader monad to an MSF to implement arrowized
-- FRP a-la Yampa.
module DunaiExamplesExtensibleAFRP where

import Data.Functor.Identity
import Control.Monad.Trans.MSF.Reader
import Data.MonadicStreamFunction              as MSF
import Data.VectorSpace

type SF a b = MSF ClockInfo a b
type ClockInfo = Reader DTime
type DTime = Double

integral :: VectorSpace a Double => SF a a
integral = eulerSteps >>> sumFrom zeroVector
  where
    eulerSteps = arrM $ \x -> asks (*^x)

reactimate :: forall a b . IO (DTime, a) -> (b -> IO ()) -> SF a b -> IO ()
reactimate sense actuate sf =
    MSF.reactimate $ senseSF >>> sfIO >>> actuateSF

  where

    sfIO :: MSF IO (DTime, a) b
    sfIO = morphS (return . runIdentity) (runReaderS sf)

    senseSF :: MSF  IO () (DTime, a)
    senseSF = arrM (\() -> sense)

    actuateSF :: MSF IO b ()
    actuateSF = arrM actuate
