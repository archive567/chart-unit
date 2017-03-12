{-
various fake data
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds #-}

module FakeData where

import Chart
import Tower.Prelude

import Control.Monad.Primitive (PrimState)
import Data.List
import Data.Reflection
import Numeric.AD
import Numeric.AD.Internal.Reverse
import System.Random.MWC
import System.Random.MWC.Probability
import qualified Control.Foldl as L
import qualified Data.Map.Strict as Map
import qualified Protolude as P
import Data.TDigest
import Formatting

{-
Standard normal random variates in one dimension.
-}

rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standard gen

{-
This generates n V2 random variates where the x and y parts are correlated.
-}

rvsCorr :: Gen (PrimState IO) -> Int -> Double -> IO [V2 Double]
rvsCorr gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zipWith V2 s0 s1'

mkScatterData :: IO [[V2 Double]]
mkScatterData = do
    g <- create
    xys <- rvsCorr g 1000 0.7
    xys1 <- rvsCorr g 1000 -0.5
    pure [ over _y (+1) . over _x (\x -> x^2 + 3*x - 1) <$> xys
         , over _x (\x -> x^2 + 3*x + 1) <$> xys1]

makeHist :: Int -> [Double] -> [V4 Double]
makeHist n xs = fromHist (IncludeOvers 1) (fill cuts xs)
  where
    r = range xs
    cuts = ticksExact r n

makeRvs :: IO [[Double]]
makeRvs = do
    g <- create
    xys <- rvs g 1000
    xys1 <- rvs g 1000
    pure [xys, (1.5*) <$> xys1]

mkHistData :: IO [[V4 Double]]
mkHistData = do
    d0 <- makeRvs
    pure $ makeHist 30 <$> d0 

makeRectQuantiles :: Double -> IO [V4 Double]
makeRectQuantiles n = do
    vs <- makeQuantiles n
    let begin = ([],Nothing)
    let step :: ([V4 Double],Maybe Double) -> Double -> ([V4 Double],Maybe Double)
        step (_,   Nothing) a = ([], Just a)
        step (acc, Just l)  a = (acc <> [V4 l 0 a (0.1/(a-l))], Just a)
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

arrowData :: [V4 Double]
arrowData = zipWith (\(V2 x y) (V2 z w) -> V4 x y z w) pos dir'
  where
    pos = locs (-1 ... 1) (-1 ... 1) 20
    dir' = gradF rosenbrock 0.01 <$> pos

locs :: Range Double -> Range Double -> Double -> [V2 Double]
locs rx ry steps = [V2 x y | x <- grid' rx steps, y <- grid' ry steps]

grid' :: forall b. (Field b, Fractional b, Enum b) => Range b -> b -> [b]
grid' (Range (x,x')) steps = (\a -> x + (x'-x)/steps * a) <$> [0..steps]

gradF ::
    (forall s. (Reifies s Tape) => [Reverse s Double] -> Reverse s Double) ->
    Double ->
    V2 Double ->
    V2 Double
gradF f step (V2 x y) =
    - r2 ((\[x',y'] -> (x',y')) $
          gradWith (\x0 x1 -> x0 + (x1 - x0) * step) f [x,y])

rosenbrock :: (Num a) => [a] -> a
rosenbrock [] = 0
rosenbrock [x] = 100 P.* (P.negate x P.^ 2) P.^ 2 P.+ (x P.- 1) P.^ 2
rosenbrock (x:y:_) = 100 P.* (y P.- x P.^ 2) P.^ 2 P.+ (x P.- 1) P.^ 2

-- a histogram
data Histogram = Histogram
   { _cuts   :: [Double] -- bucket boundaries
   , _values :: Map.Map Int Double -- bucket counts
   } deriving (Show, Eq)

freq' :: Map.Map Int Double -> Map.Map Int Double
freq' m = Map.map (* recip (P.sum m)) m

freq :: Histogram -> Histogram
freq (Histogram c v) = Histogram c (freq' v)

count :: L.Fold Int (Map Int Double)
count = L.Fold (\x a -> Map.insertWith (+) a 1 x) Map.empty identity

countW :: L.Fold (Int,Double) (Map Int Double)
countW = L.Fold (\x (a,w) -> Map.insertWith (+) a w x) Map.empty identity

countBool :: L.Fold Bool Int
countBool = L.Fold (\x a -> x + if a then 1 else 0) 0 identity

histMap :: (Functor f, Functor g, Ord a, Foldable f, Foldable g) =>
    f a -> g a -> Map Int Double
histMap cuts xs = L.fold count $ (\x -> L.fold countBool (fmap (x >) cuts)) <$> xs

fill :: [Double] -> [Double] -> Histogram
fill cuts xs = Histogram cuts (histMap cuts xs)

mkHistogramData :: IO [Histogram]
mkHistogramData = do
    d0 <- makeRvs
    let cuts = rangeCuts 6 (-3) 3
    pure $ fill cuts  <$> d0

rangeCuts :: Int -> Double -> Double -> [Double]
rangeCuts n l u =
    (\x -> l+((u-l)/fromIntegral n)*fromIntegral x) <$>
    [0..n]

data DealOvers = IgnoreOvers | IncludeOvers Double

fromHist :: DealOvers -> Histogram -> [V4 Double]
fromHist o (Histogram cuts counts) = zipWith4 V4 x y z w'
  where
      y = repeat 0
      w = zipWith (/)
          ((\x0 -> Map.findWithDefault 0 x0 counts) <$> [f..l])
          (zipWith (-) z x)
      f = case o of
        IgnoreOvers -> 1
        IncludeOvers _ -> 0
      l = case o of
        IgnoreOvers -> length cuts - 1
        IncludeOvers _ -> length cuts
      w' = (/P.sum w) <$> w
      x = case o of
        IgnoreOvers -> cuts
        IncludeOvers outw -> [Data.List.head cuts - outw] <> cuts <> [Data.List.last cuts + outw]
      z = drop 1 x

labelsFromCuts :: DealOvers -> [Double] -> [Text]
labelsFromCuts o cuts =
    case o of
      IgnoreOvers -> inside
      IncludeOvers _ -> [ "< " <> sformat (prec 2) (Data.List.head cuts)] <> inside <> [ "> " <> sformat (prec 2) (Data.List.last cuts)]
  where
    inside = sformat (prec 2) <$> zipWith (\l u -> (l+u)/2) cuts (drop 1 cuts)
