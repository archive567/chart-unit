{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

module FakeData where

import Chart
import NumHask.Prelude

import Control.Monad.Primitive (PrimState)
import Data.Reflection
import Numeric.AD
import Numeric.AD.Internal.Reverse
import System.Random.MWC
import System.Random.MWC.Probability
import qualified Control.Foldl as L
import qualified Protolude as P
import Data.TDigest

{-
Standard normal random variates in one dimension.
-}

rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standard gen

{-
This generates n V2 random variates where the x and y parts are correlated.
-}

rvsCorr :: Gen (PrimState IO) -> Int -> Double -> IO [Pair Double]
rvsCorr gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zipWith Pair s0 s1'

mkScatterData :: IO [[Pair Double]]
mkScatterData = do
    g <- create
    xys <- rvsCorr g 1000 0.7
    xys1 <- rvsCorr g 1000 -0.5
    pure [ (\(Pair x y) -> Pair ( x^^2 + 3*x - 1) (y+1)) <$> xys
         , (\(Pair x y) -> Pair ( x^^2 + 3*x + 1) y) <$> xys1]

makeHist :: Int -> [Double] -> [Rect Double]
makeHist n xs = fromHist (IncludeOvers 1) (fill cuts xs)
  where
    r = space xs
    cuts = linearSpace OuterPos r n

makeRvs :: IO [[Double]]
makeRvs = do
    g <- create
    xys <- rvs g 1000
    xys1 <- rvs g 1000
    pure [xys, (1.5*) <$> xys1]

mkHistData :: IO [[Rect Double]]
mkHistData = do
    d0 <- makeRvs
    pure $ makeHist 30 <$> d0 

mkHistogramData :: IO [Histogram]
mkHistogramData = do
    d0 <- makeRvs
    let cuts = linearSpace OuterPos (Range -3.0 3.0) 6
    pure $ fill cuts  <$> d0

makeRectQuantiles :: Double -> IO [Rect Double]
makeRectQuantiles n = do
    vs <- makeQuantiles n
    let begin = ([],Nothing)
    let step :: ([Rect Double],Maybe Double) -> Double -> ([Rect Double],Maybe Double)
        step (_,   Nothing) a = ([], Just a)
        step (acc, Just l)  a = (acc <> [Rect l a 0 (0.1/(a-l))], Just a)
    let h = L.fold (L.Fold step begin fst) vs
    pure h

makeQuantiles :: Double -> IO [Double]
makeQuantiles n = do
    g <- create
    xs <- rvs g 100000
    let qs = ((1/n)*) <$> [0..n]
    let vs = L.fold (tDigestQuantiles qs) xs
    pure vs

tDigestQuantiles :: [Double] -> L.Fold Double [Double]
tDigestQuantiles qs = L.Fold step begin done
  where
    step x a = Data.TDigest.insert a x
    begin = tdigest ([]::[Double]) :: TDigest 25
    done x = fromMaybe nan . (`quantile` compress x) <$> qs

arrowData :: Pair Int -> [Arrow]
arrowData g = zipWith Arrow positions arrows
  where
    positions = gridP OuterPos (Rect -1 1 -1 1) g
    arrows = gradF rosenbrock 0.01 <$> positions

gradF ::
    (forall s. (Reifies s Tape) => [Reverse s Double] -> Reverse s Double) ->
    Double ->
    Pair Double ->
    Pair Double
gradF f step (Pair x y) =
    fmap negate ((\[x',y'] -> Pair x' y') $
          gradWith (\x0 x1 -> x0 + (x1 - x0) * step) f [x,y])

rosenbrock :: (Num a) => [a] -> a
rosenbrock [] = 0
rosenbrock [x] = 100 P.* (P.negate x P.^ 2) P.^ 2 P.+ (x P.- 1) P.^ 2
rosenbrock (x:y:_) = 100 P.* (y P.- x P.^ 2) P.^ 2 P.+ (x P.- 1) P.^ 2

