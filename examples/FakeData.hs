{-
various fake data
-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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

makeHist :: Int -> [Double] -> [V4 Double]
makeHist n xs = zipWith4 V4 (init cuts) (replicate (length xs+1) 0) (drop 1 cuts) (fromIntegral <$> histList)
  where
    r = range xs
    cuts = ticksExact r n
    count = L.Fold (\x a -> Map.insertWith (+) a (1::Integer) x) Map.empty identity
    countBool = L.Fold (\x a -> x + if a then 1 else 0) 0 identity
    histMap = L.fold count $ (\x -> L.fold countBool (fmap (x >) cuts)) <$> xs
    histList = (\x -> Map.findWithDefault 0 x histMap) <$> [0..length xs]

arrowData :: [V4 Double]
arrowData = zipWith (\(V2 x y) (V2 z w) -> V4 x y z w) pos dir'
  where
    pos = locs (-1 ... 1) (-1 ... 1) 20
    dir' = gradF rosenbrock 0.01 <$> pos

locs :: Range Double -> Range Double -> Double -> [V2 Double]
locs rx ry steps = [V2 x y | x <- grid rx steps, y <- grid ry steps]

grid :: forall b. (Field b, Fractional b, Enum b) => Range b -> b -> [b]
grid (Range (x,x')) steps = (\a -> x + (x'-x)/steps * a) <$> [0..steps]

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

