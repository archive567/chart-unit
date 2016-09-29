```include
other/header.md
```

[chart-unit](http://tonyday567.github.io/chart-unit.html)
===

[![repo](https://a248.e.akamai.net/assets.github.com/images/icons/emoji/octocat.png)](https://github.com/tonyday567/chart-unit)

[![Build Status](https://travis-ci.org/tonyday567/chart-unit.png)](https://travis-ci.org/tonyday567/chart-unit)

chart-unit
===

scratchpad
---

My newest chart `padq $ linesXY def [[(0,0),(1,1)],[(0,0),(1,2)]]`

![](other/scratchpad.svg)

This slowly growing collection of charts:

- render nicely over a wide chart size range, svg and png formats.
- render similarly at different scale
- are opinionated minimalism
- are unit shapes in the spirit of the [diagrams](http://projects.haskell.org/diagrams/doc/quickstart.html) design space.
- can be quickly integrated into ad-hoc haskell data analytics, providing a visual feedback loop.

charts
---

Scatter

![](other/scatter.svg)

Histogram

![](other/hist.svg)

Line

![](other/line.svg)

Lines

![](other/lines.svg)

Labelled Bar Chart

![](other/bar.svg)

rasterific png renders
---

![](other/scratchpad.png)

Scatter

![](other/scatter.png)

Histogram

![](other/hist.png)

Line

![](other/line.png)

Lines

![](other/lines.png)

Labelled Bar Chart

![](other/bar.png)

> {-# OPTIONS_GHC -Wall #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> import Protolude
> import Control.Monad.Primitive (unsafeInlineIO)
> import Diagrams.Prelude hiding ((<>))
> import qualified Control.Foldl as L
> import qualified Data.Random as R
> import qualified Data.Map.Strict as Map
> import qualified Data.Text as Text
>
> import Chart.Unit

some test data
---

Standard normal random variates.  Called ys to distinguish from the horizontal axis of the chart (xs) which are often implicitly [0..]

> ys :: Int -> IO [Double]
> ys n =
>   replicateM n $ R.runRVar R.stdNormal R.StdRandom
>

A bunch of ys, accumulated.

> yss :: (Int, Int) -> [[Double]]
> yss (n,m) = unsafeInlineIO $ do
>   yss' <- replicateM m $ ys n
>   pure $ (drop 1 . L.scan L.sum) <$> yss'
>

xys is a list of X,Y pairs, correlated normal random variates to add some shape to chart examples.

> rXYs :: Int -> Double -> [(Double,Double)]
> rXYs n c = unsafeInlineIO $ do
>   s0 <- replicateM n $ R.runRVar R.stdNormal R.StdRandom
>   s1 <- replicateM n $ R.runRVar R.stdNormal R.StdRandom
>   let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
>   pure $ zip s0 s1'
>
> xys = rXYs 1000 0.8
>

XY random walk

> rwxy = L.scan (L.Fold (\(x,y) (x',y') -> (x+x',y+y')) (0.0,0.0) identity) (take 100 xys)
>

xysHist is a histogram of 10000 one-dim random normals.

The data out is a (X,Y) pair list, with mid-point of the bucket as X, and bucket count as Y.

> xysHist :: [(Double,Double)]
> xysHist = unsafeInlineIO $ do
>   ys' <- replicateM 10000 $ R.runRVar R.stdNormal R.StdRandom :: IO [Double]
>   let (f,s,n) = mkTicks' (range1D ys') 100
>   let cuts = (\x -> f+s*fromIntegral x) <$> [0..n]
>   let mids = (+(s/2)) <$> cuts
>   let count = L.Fold (\x a -> Map.insertWith (+) a 1 x) Map.empty identity
>   let countBool = L.Fold (\x a -> x + if a then 1 else 0) 0 identity
>   let histMap = L.fold count $ (\x -> L.fold countBool (fmap (x >) cuts)) <$> ys'
>   let histList = (\x -> Map.findWithDefault 0 x histMap) <$> [0..n]
>   return (zip mids (fromIntegral <$> histList))
>

Scale Robustness
---

xys rendered on the XY plane as dots - a scatter chart with no axes - is invariant to scale.  The data could be multiplied by any scalar, and look exactly the same.

![](other/dots.svg)

Axes break this scale invariance. Ticks and tick labels can hide this to some extent and look almost the same across scales.

![](other/scatter.svg)

This chart will look the same on a data scale change, except for tick magnitudes.

main
---

>
> main :: IO ()
> main = do
 
See develop section below for my workflow.

>   padsvg $
>       linesXY def [[(0,0),(1,1)],[(0,0),(1,2)]]
>   fileSvg "other/line.svg" (200,200) $
>     (lineXY def rwxy)
>   filePng "other/line.png" (200,200) $
>     (lineXY def rwxy)
>   fileSvg "other/lines.svg" (200,200) $
>     (linesXY def $ zip [0..] <$> yss (1000, 10))
>   filePng "other/lines.png" (200,200) $
>     (linesXY def $ zip [0..] <$> yss (1000, 10))
>   fileSvg "other/dots.svg" (200,200) $
>     (scatter def xys)
>   filePng "other/dots.png" (200,200) $
>     (scatter def xys)
>   fileSvg "other/scatter.svg" (200,200) $
>     (scatterXY def xys)
>   filePng "other/scatter.png" (200,200) $
>     (scatterXY def xys)
>   fileSvg "other/bar.svg" (200,200) $
>     barLabelled def (unsafeInlineIO $ ys 10) (fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..])
>   filePng "other/bar.png" (200,200) $
>     barLabelled def (unsafeInlineIO $ ys 10) (fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..])
>   fileSvg "other/hist.svg" (200,200) $
>     barRange def xysHist
>   filePng "other/hist.png" (200,200) $
>     barRange def xysHist

diagrams development recipe
---

In constructing new `units`:

- diagrams go from abstract to concrete
- start with the unitSquare: 4 points, 1x1, origin in the center
- work out where the origin should be, given the scaling needed.
- turn the pointful shape into a Trail
- close the Trail into a SVG-like loop
- turn the Trail into a QDiagram

You can slide up and down the various diagrams abstraction levels creating transformations at each level.  For example, here's something I use to work at the point level:

> unitp f = unitSquare # f # fromVertices # closeTrail # strokeTrail

workflow
---

> padsvg :: ChartSvg -> IO ()
> padsvg t =
>   fileSvg "other/scratchpad.svg" (400,400) t
>
> padpng :: ChartPng -> IO ()
> padpng t =
>   filePng "other/scratchpad.png" (400,400) t
>

Create a markdown version of readme.lhs:

~~~
pandoc -f markdown+lhs -t html -i readme.lhs -o index.html
~~~

Then fire up an intero session, and use padq to display coding results on-the-fly, mashing the refresh button on a browser pointed to readme.html.

or go for a compilation loop like:

~~~
stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o index.html --mathjax --filter pandoc-include && pandoc -f markdown+lhs -t markdown -i readme.lhs -o readme.md --mathjax --filter pandoc-include
~~~

