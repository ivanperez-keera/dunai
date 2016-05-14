{-# LANGUAGE GeneralizedNewtypeDeriving #-}

sf = bouncingBall (100.0 :: Float) 0.0

bouncingBall p0 v0 =
  switch (proc (_) -> do
            (p,v)  <- fallingBall p0 v0  -< ()
            bounce <- edge               -< (p <= 0 && v < 0)
            returnA -< ((p,v), bounce `tag` (p,v))
         )
         (\(p,v) -> bouncingBall p (-v))

fallingBall p0 v0 = proc () -> do
  v <- (v0 +) ^<< integral -< (-99.8)
  p <- (p0 +) ^<< integral -< v
  returnA -< (p, v)

-- Minimal stuff we need from Yampa
type SF = MStreamF DTMonad

edge :: MStreamF m Bool (Maybe ())
edge = edgeFrom True

edgeBy :: (a -> a -> Maybe b) -> a -> MStreamF m a (Maybe b)
edgeBy isEdge a_prev = MStreamF $ \a ->
  return (isEdge a_prev a, edgeBy isEdge a)

edgeFrom :: Bool -> MStreamF m Bool (Maybe ())
edgeFrom prev = MStreamF $ \a -> do
  let res = if prev || not a then Nothing else Just ()
      ct  = edgeFrom a
   return (res, ct)

tag :: Maybe v -> v -> Maybe v
tag m v = fmap (const v) m

integral :: VectorSpace a s => SF a a
integral = integralFrom zeroVector

integralFrom :: VectorSpace a s => a -> MStreamF a a
integralFrom n0 = MStreamF $ \n -> do
  dt <- ask
  let acc = n0 ^+^ realToFrac dt *^ n
  acc `seq` return (acc, integralFrom acc)

type DTime = Double

newtype DTMonad a = DTMonad { runDT :: (Maybe DTime, a) }
 deriving Functor

ask :: DTMonad a -> DTime
ask = fromMaybe 0 . fst . runDT

instance Applicative DTMonad where
  pure x = DTMonad (Nothing, x)

  (DTMonad (mdt1, f)) <*> (DTMonad (mdt2, v)) = DTMonad (mdt, f v)
    where mdt = case (mdt1, mdt2) of
                  (Just dt1, Just dt2) = Just $ min dt1 dt2
                  (Nothing,  Just dt2) = Just dt2
                  (Just dt1, Nothing)  = Just dt1
                  _                    = Nothing

instance Monad DTMonad where
  (DTMonad (mdt1, v1)) >>= f =
     let DTMonad (mdt2, v2) = f v1
         mdt = case (mdt1, mdt2) of
                  (Just dt1, Just dt2) = Just $ min dt1 dt2
                  (Nothing,  Just dt2) = Just dt2
                  (Just dt1, Nothing)  = Just dt1
                  _                    = Nothing
     in DTMonad (mdt, v2)
