{-# LANGUAGE Arrows #-}
-- Part of this code is taken and adapted from:
-- https://wiki.haskell.org/WxHaskell/Quick_start#Hello_world_in_wxHaskell
--
-- NOTE: Currently, there exists a problem with implementing push-based
-- evaluation.
--
-- Ideas: using some monad or another arrow-based mechanism to remember
-- values and not push unless its necessary. Maybe using arrow transformers
-- instead of monad transformers, like Henrik suggested?
module Main where

import Prelude hiding ((.))
import Control.Category
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.MonadicStreamFunction hiding (trace)
import Data.MonadicStreamFunction.InternalCore
import Debug.Trace
import Graphics.UI.WX

main :: IO ()
main = start hello

hello :: IO ()
hello = do
  f      <- frame      []
  lenLbl <- staticText f [ text := "0" ]
  entry1 <- textEntry  f []
  entry2 <- textEntry  f []
  quit   <- button     f [ text := "Quit", on command := close f ]

  reactiveWXFieldRW entry1 text =:= (liftRW2 (reverse, reverse) (reactiveWXFieldRW entry2 text))
  reactiveWXFieldRO entry1 text =:> (arr (show.length) >>> reactiveWXFieldWO lenLbl text)

  set f [layout := margin 10 (column 5 [ floatCentre (widget lenLbl)
                                       , floatCentre (widget entry1)
                                       , floatCentre (widget entry2)
                                       , floatCentre (widget quit)
                                       ] )]


-- * Auxiliary definitions

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

-- ** Keera Hails

type ReactiveValueRO m a = (MStream m a, m () -> m ())
type ReactiveValueWO m a = MSink   m a
type ReactiveValueRW m a = (MStream m a, MSink m a, m () -> m ())

liftRW2 :: Monad m => (a -> b, b -> a) -> ReactiveValueRW m a -> ReactiveValueRW m b
liftRW2 (f, f') (sg, sk, h) = (sg >>> arr f, arr f' >>> sk, h)

(=:=) :: (Show a, Eq a) => ReactiveValueRW IO a -> ReactiveValueRW IO a -> IO ()
(sg1,sk1,h1) =:= (sg2, sk2, h2) = do
  (sg1,h1) =:> sk2
  (sg2,h2) =:> sk1

(=:>) :: (Show a, Eq a) => ReactiveValueRO IO a -> ReactiveValueWO IO a -> IO ()
(sg, h) =:> sk = h =<< pushReactimate_ (sg >>> sk)

-- ** Auxiliary WX functions
setProp :: widget -> Attr widget attr -> attr -> IO ()
setProp c p v = set c [ p := v ]

-- ** Keera Hails - WX bridge on top of Dunai
reactiveWXFieldRO :: Updating widget => widget -> Attr widget attr -> ReactiveValueRO IO attr
reactiveWXFieldRO widget attr =
  ( constM (get widget attr)
  , \m -> set widget [ on update :~ (\m1 -> m1 >> m) ]
  )

reactiveWXFieldWO :: Eq attr => widget -> Attr widget attr -> ReactiveValueWO IO attr
reactiveWXFieldWO widget attr = arrM $ \v -> do
  o <- get widget attr
  if v == o
    then return ()
    else setProp widget attr v

reactiveWXFieldRW :: (Updating widget, Eq attr) => widget -> Attr widget attr -> ReactiveValueRW IO attr
reactiveWXFieldRW widget attr = (sg, sk, h)
 where (sg, h) = reactiveWXFieldRO widget attr
       sk      = reactiveWXFieldWO widget attr

pushify :: (Monad m, Eq a, Show a, Show b) => MSF m a b -> MSF m a b
pushify msf = feedback Nothing (pushify' msf)

pushify' :: (Eq a, Monad m, Show a, Show b)
         => MSF m a b -> MSF m (a, Maybe (a, b)) (b, Maybe (a, b))
pushify' msf = proc (a, mov) -> do
  nv <- if Just a == (trace (show (a, mov)) $ fmap fst mov)
          then returnA -< snd (fromJust mov)
          else msf     -< a
  returnA -< (nv, Just (a, nv))

constant :: Monad m => b -> MSF m a b
constant = arr . const

voidA :: Arrow a => a b c -> a b ()
voidA a = a >>> arr (const ())
