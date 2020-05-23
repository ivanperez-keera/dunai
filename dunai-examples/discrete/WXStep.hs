-- Part of this code is taken and adapted from:
-- https://wiki.haskell.org/WxHaskell/Quick_start#Hello_world_in_wxHaskell
module Main where

import Prelude hiding ((.))
import Control.Category
import Control.Monad
import Data.IORef
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore
import Graphics.UI.WX

main :: IO ()
main = start $ do
  f      <- frame      []
  lenLbl <- staticText f [ text := "0" ]
  step   <- button     f [ text := "Up" ]
  quit   <- button     f [ text := "Quit", on command := close f ]

  counter <- newIORef 0

  -- Reactive network
  let labelMSF  = labelTextSk lenLbl . arr show . ioRefSg counter
      buttonMSF = ioRefSg counter >>> arr (+1) >>> ioRefSk counter

  -- NOTE: The order here is *very* important. If you write
  -- labelMSF &&& buttonMSF, then they will be desynced
  -- (meaning that, at the end of one simulation step, the label
  -- will not show the contents of the IORef).
  let appMSF = (buttonMSF &&& labelMSF) >>> arr (const ())

  hndlr <- pushReactimate_ appMSF
  set step [ on command := hndlr ]

  set f [layout := margin 10 (column 5 [ floatCentre (widget lenLbl)
                                       , floatCentre (widget step)
                                       , floatCentre (widget quit)
                                       ] )]


-- * Auxiliary definitions

-- ** Adhoc Dunai-WX backend
textEntryTextSg :: TextCtrl a -> MStream IO String
textEntryTextSg entry = constM (get entry text)

labelTextSk :: StaticText a -> MSink IO String
labelTextSk lbl = arrM $ setJust lbl text
  -- (\t -> set lbl [ text := t ])

ioRefSg :: IORef a -> MStream IO a
ioRefSg ioref = constM (readIORef ioref)

ioRefSk :: IORef a -> MSink IO a
ioRefSk ioref = arrM (writeIORef ioref)

-- ** MSF-related definitions and extensions

-- | Run an MSF on an input sample step by step, using an IORef to store the
-- continuation.
pushReactimate :: MSF IO a b -> IO (a -> IO b)
pushReactimate msf = do
  msfRef <- newIORef msf
  return $ \a -> do
              msf' <- readIORef msfRef
              (b, msf'') <- unMSF msf' a
              writeIORef msfRef msf''
              return b

-- | Run one step of an MSF on () streams, internally storing the
-- continuation.
pushReactimate_ :: MSF IO () () -> IO (IO ())
pushReactimate_ msf = do
  f <- pushReactimate msf
  return (void (f ()))

-- ** Auxiliary WX functions
setJust :: widget -> Attr widget attr -> attr -> IO ()
setJust c p v = set c [ p := v ]
