{-# LANGUAGE Arrows              #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import Data.Functor.Identity
import Data.Maybe
import Control.Monad.Trans.MSF.Reader
import Data.IORef
import           Data.MonadicStreamFunction.InternalCore
import "bearriver" FRP.Yampa hiding (dpSwitchB)
-- import "Yampa" FRP.Yampa
-- import "Yampa" FRP.Yampa.InternalCore

type TestSF = SF Int Int

main :: IO ()
main = print test_dpSwitchB

test_dpSwitchB :: [[Int]]
test_dpSwitchB = oss
  where
    steps = 1
    dts = map (\i -> (1.0, Just (i+1))) [0..steps]
    sfs = map sf [0..10]

    oss = embed' (nextStep sfs) (0, dts)

embed' :: forall a b . SF a b -> (a, [(DTime, Maybe a)]) -> [b]
embed' sf times = runIdentity sfE
  where
    sfE :: Identity [b]
    sfE = embed (runReaderS sf) (signalize times)

signalize :: (a, [(DTime, Maybe a)]) -> [(Time, a)]
signalize (a, as) = (0, a) : signalize' a as
  where
    signalize' :: a -> [(DTime, Maybe a)] -> [(DTime, a)]
    signalize' a []                = []
    signalize' a ((t, Nothing):as) = (t, a) : signalize' a as
    signalize' _ ((t, Just a):as)  = (t, a) : signalize' a as

sf :: Int -> TestSF
sf i0 = arr (i0+)

nextStep :: [TestSF] -> SF Int [Int]
nextStep sfs =
  dpSwitchB
    sfs
    (constant (Event ()) >>> notYet)
    cont
  where
    cont sfs _ = nextStep sfs

dpSwitchB :: [SF a b] -> SF (a, [b]) (Event c) -> ([SF a b] -> c -> SF a [b])
          -> SF a [b]
dpSwitchB sfs sfF sfCs = MSF $ \a -> do
  res <- mapM (`unMSF` a) sfs
  let bs   = fmap fst res
      sfs' = fmap snd res
  (e,sfF') <- unMSF sfF (a, bs)
  ct <- case e of
             Event c -> snd <$> unMSF (sfCs sfs c) a
             NoEvent -> return (dpSwitchB sfs' sfF' sfCs)
  return (bs, ct)

-- nextStep :: [TestSF] -> SF Int [Int]
-- nextStep sfs = SF {sfTF = tf0}
--   where
--       tf0 a0 = (continuation, cs0)
--         where
--            cs0   = fmap snd sfcs0
--            bsfs0 = broadcast a0 sfs
--            sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
--
--            continuation = fst (tf0 a0)
--
--       dpSwitchAux = error "I came this way"


-- -- !!! Could be optimized on the event source being SFArr, SFArrE, SFArrEE.
-- --
-- dpSwitchB :: [SF a b]
--                -- ^ Initial collection of signal functions.
--           -> SF (a, [b]) (Event c)
--                -- ^ Signal function that observes the external input signal and
--                -- the output signals from the collection in order to produce a
--                -- switching event.
--           -> ([SF a b] -> c -> SF a [b])
--                -- ^ The fourth argument is a function that is invoked when the
--                -- switching event occurs, yielding a new signal function to
--                -- switch into based on the collection of signal functions
--                -- previously running and the value carried by the switching
--                -- event. This allows the collection to be updated and then
--                -- switched back in, typically by employing 'dpSwitch' again.
--           -> SF a [b]
-- dpSwitchB sfs0 sfe0 k = SF {sfTF = tf0}
--     where
--         tf0 a0 =
--             let bsfs0 = broadcast a0 sfs0
--                 sfcs0 = fmap (\(b0, sf0) -> (sfTF sf0) b0) bsfs0
--                 cs0   = fmap snd sfcs0
--
--                 continuation = case (sfTF sfe0) (a0, cs0) of
--                   (sfe, NoEvent)  -> dpSwitchAux (fmap fst sfcs0) sfe
--                   (_,   Event d0) -> fst (sfTF (k sfs0 d0) a0)
--             in
--                 (continuation, cs0)
--
--         dpSwitchAux = error "I came this way"
--         -- dpSwitchAux sfs (SFArr _ (FDC NoEvent)) = parAux broadcast sfs
--         -- dpSwitchAux sfs sfe = SF' tf -- False
--         --     where
--         --         tf dt a =
--         --             let bsfs  = broadcast a sfs
--         --                 sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
--         --                 cs    = fmap snd sfcs'
--         --             in
--         --                 (case (sfTF' sfe) dt (a, cs) of
--         --                      (sfe', NoEvent) -> dpSwitchAux (fmap fst sfcs') sfe'
--         --                      (_,    Event d) -> fst (sfTF (k (freezeCol sfs dt) d) a), cs)
--
-- -- | Tuple a value up with every element of a collection of signal
-- -- functions.
-- broadcast :: a -> [sf] -> [(a, sf)]
-- broadcast a = fmap (\sf -> (a, sf))
--
-- -- -- Internal definition. Also used in parallel switchers.
-- -- parAux :: Functor col =>
-- --     (forall sf . (a -> col sf -> col (b, sf)))
-- --     -> col (SF' b c)
-- --     -> SF' a (col c)
-- -- parAux rf sfs = SF' tf -- True
-- --     where
-- --         tf dt a =
-- --             let bsfs  = rf a sfs
-- --                 sfcs' = fmap (\(b, sf) -> (sfTF' sf) dt b) bsfs
-- --                 sfs'  = fmap fst sfcs'
-- --                 cs    = fmap snd sfcs'
-- --             in
-- --                 (parAux rf sfs', cs)
-- --
-- --
-- -- -- Freezes a "running" signal function, i.e., turns it into a continuation in
-- -- -- the form of a plain signal function.
-- -- freeze :: SF' a b -> DTime -> SF a b
-- -- freeze sf dt = SF {sfTF = (sfTF' sf) dt}
-- --
-- -- freezeCol :: [SF' a b] -> DTime -> [SF a b]
-- -- freezeCol sfs dt = fmap (`freeze` dt) sfs
-- --
