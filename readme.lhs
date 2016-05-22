<meta charset='utf-8'>
<link rel="stylesheet" href="lhs.css">

chart-svg
---

scratchpad

![](other/scratchpad.svg)

Scatter Chart

![](other/scatter.svg)

Bar Chart

![](other/bar.svg)


This is a scratchpad repo for some chart experiments.  The scratchpad usually has whatever I'm up to.  Next chart to develop is an area chart.

The aim is a series of charts that

- render robustly over a wide chart size range
- render automatically over different data magnitude scales
- are minimalist
- have the same vector space as the unit shapes in [diagrams](http://projects.haskell.org/diagrams/haddock/index.html)

>
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE GADTs #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
> {-# OPTIONS_GHC -fno-warn-unused-binds #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> {-# OPTIONS_GHC -fno-warn-unused-imports #-}
> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> 
> import Protolude
> import Control.Monad.Primitive (unsafeInlineIO)
> import Control.Category (id)
> import Data.List (transpose)
> import System.IO (FilePath)
> import Diagrams.Prelude
> import Diagrams.Backend.SVG
> import Diagrams.Core.Envelope

helper libraries
---

> import Formatting
> import GHC.Base (String)
> import qualified Control.Foldl as L
> import qualified Data.Vector.Unboxed as V
> import qualified Data.Random as R
> import qualified Control.Foldl.Incremental.Histogram as H
> import Data.Vector.Unboxed  (Vector,(!))
> import qualified Data.Vector.Unboxed as VU
> import qualified Data.Histogram as H
> import qualified Data.Histogram.Bin.BinDU as H

Chart library
---

> import Chart

some test data - a pair of correlated normal random variates.  random-fu has such a clear api for this.

> rXYs :: Int -> Double -> IO [(Double,Double)]
> rXYs n c = do
>   s0 <- replicateM n $ R.runRVar R.stdNormal R.StdRandom
>   s1 <- replicateM n $ R.runRVar R.stdNormal R.StdRandom
>   let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
>   pure $ zip s0 s1'
>
> xys = unsafeInlineIO $
>   fmap (\(x,y) -> (x,y)) <$> rXYs 1000 0.8
>

Rendered on the XY plane into a scatter chart with no axes:

![](other/dots.svg)

Axes break the scale invariance of the above chart (the diagram will look exactly the same at any data scale change). But ticks and tick labels can hide this info leakage so that scale invariance continues to hold.

![](other/scatter.svg)

histogram
---

Taking a histogram of the X data, using the tick technology as histogram bins.

> histX t xs =
>   zip
>   ((H.fromIndex bins) <$> [1..])
>   (V.toList $ V.init $ V.tail $ H.histData $ hist)
>   where
>     hist = 
>       L.fold
>       (H.incrementalizeHistU
>        (H.binDU $ V.fromList $ inf $
>         tickList (range1D xs) t) (const 1) 1) xs
>     bins = H.bins hist
>     inf x = [-1/0] ++ x ++ [1/0]

> histXs = histX 10 (fst <$> xys)

λ> histXs
[(-2.75,5.0e-3),(-2.25,1.9e-2),(-1.75,3.7e-2),(-1.25,9.5e-2),(-0.75,0.153),(-0.25,0.186),(0.25,0.184),(0.75,0.159),(1.25,9.3e-2),(1.75,4.5e-2),(2.25,1.8e-2)]

bar
---

Each bar is a rectangle with height equal to y in (x,y) and placement equal to x in (x,y). x is often dropped and left to the rendering assuming equal intervals. x = [1..] works for instance

![](other/bar.svg)



main
---

>
> main :: IO ()
> main = do
>   padq $ unitScatter def def xys
>   toFile "other/dots.svg" (100,100) (dots 0.01 (rgba(120,40,30,0.1)) xys)
>   toFile "other/scatter.svg" (100,100) (unitScatter def def xys)
>   toFile "other/bar.svg" (200,200) $
>     unitBar def (snd <$> histXs) (formatToString (prec 2) <$> (fst <$> histXs))
>     # barStyle def


recipe
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

Quick renderer
---

> padq :: QDiagram SVG V2 Double Any -> IO ()
> padq t =
>   toFile "other/scratchpad.svg" (400,400) t
>

develop
---

[![Build Status](https://travis-ci.org/tonyday567/chart-svg.png)](https://travis-ci.org/tonyday567/chart-svg)

Build, run, render readme

~~~
filewatcher '**/*.{lhs,hs,cabal}' 'stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o readme.html && echo "run"'
~~~

Publish

~~~
pandoc -f markdown+lhs -t html -i readme.lhs -o ~/git/tonyday567.github.io/perf.html
readme | pandoc -f markdown+lhs -t html -o ~/git/tonyday567.github.io/perf-out.html
cp other/* ~/git/tonyday567.github.io/other
pandoc -f markdown+lhs -t markdown -i readme.lhs -o readme.md
~~~

todo
---

My rough R&D roadmap is:

- transform data to 2D histogram and work up an area/heatmap/surface/contour chart
- add a line chart (or work out iso to a scatter chart)
- add a 1D chart (which might be iso with an axis)
