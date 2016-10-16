```include
other/header.md
```

[chart-unit](https://tonyday567.github.io/chart-unit.html) [![Build Status](https://travis-ci.org/tonyday567/chart-unit.png)](https://travis-ci.org/tonyday567/chart-unit)
===

scratchpad
---

![](other/scratchpad.svg)

tl;dr
---

This slowly growing collection of charts:

- renders nicely over a wide chart size range, svg and png formats.
- render similarly at different scale
- are opinionated minimalism
- are unit shapes in the spirit of the [diagrams](http://projects.haskell.org/diagrams/doc/quickstart.html) design space.
- can be quickly integrated into ad-hoc haskell data analytics, providing a visual feedback loop.

latest changes
---

The data api is pretty much the same as linear now.  I think there's enough room in that library for the extra dimensionality aspect of charting (what ggplot calls aesthetics, such as size, color and shape).

The bar charts have all become rectangle charts, with 4 dimensions representing (x,y) (z,w) corners of the rectangle.

Given any remotness to others using this library, it's also a testing ground for the tower project, hence the polymorph spagetti constraints.  You'll thanks me when you find rotating through a euclidean infinite dimension space a one liner.

The charts themselves, with an attached `V2 Range a` and `[[R2 a]]` data are themselves very close to numbers, and scaling operations could already be directly applied to the chart.

I mostly think of a chart as multiple data sets that share common-ranged dimensions in an XY plane.  If they don't share ranges, then representing them on a single XY plane is almost the same thing.  Regardless, in diagrams, combining two lines with different scales is an easy mappend.


chartWith
---

~~~
chartWith :: (Traversable f, Traversable g, R2 r) => 
    ChartConfig
    -> (g (f (r Double)) -> Chart b)
    -> V2 (Range Double)
    -> (V2 (Range Double) -> g (f (r Double)) -> g (f (r Double)))
    -> g (f (r Double))
    -> Chart b
chartWith (ChartConfig p axes) renderer range' scaler ms =
~~~

Chart rendering now takes:

- configuration
- a render of double-containered R2 data (has an x and y plane) as a concrete pretty picture
- range to render the data and axes on
- a scaler containing a recipe to squish the data down into V2 space.
- double-containered R2 data

And produces a `Chart b` available to turn into an svg or for further combinings.

charts
===

png's at the end for markdown viewers ...

Scatter

![](other/scatter.svg)

Scatter * 2

![](other/scatters.svg)

Histogram

![](other/hist.svg)

Line

![](other/line.svg)

Lines

![](other/lines.svg)

Labelled Bar Chart

![](other/bar.svg)

Grid Overlay

Note how the axis ticks line up exactly with middle of the dots.  I now think of the axes as a small subset of possible HUDs to help users interpret data.

![](other/grid.svg)

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
> import Linear hiding (identity)
> import Data.List
> import Chart.Unit
> import Chart.Types
> import Diagrams.Backend.SVG (SVG)
> import Diagrams.Backend.Rasterific (Rasterific)

some test data
---

Standard normal random variates in one dimension.

> rvs :: Int -> IO [Double]
> rvs n =
>   replicateM n $ R.runRVar R.stdNormal R.StdRandom
>

This generates n 2D random variate pairs where x and y are correlated.

> xys :: Int -> Double -> [V2 Double]
> xys n c = unsafeInlineIO $ do
>   s0 <- replicateM n $ R.runRVar R.stdNormal R.StdRandom
>   s1 <- replicateM n $ R.runRVar R.stdNormal R.StdRandom
>   let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
>   pure $ zipWith V2 s0 s1'
>

h is a histogram of 10000 one-dim random normals.

The data out is a V4 with xz as the bucket range, and yw as 0 and the bucket count. For a rectangle chart this translates to (x,y) as the bottom left point and (z,w) as the upper right point of the rectangle.

> h :: [V4 Double]
> h = unsafeInlineIO $ do
>   ys' <- replicateM 10000 $ R.runRVar R.stdNormal R.StdRandom :: IO [Double]
>   let n = 10
>   let r = range ys'
>   let cuts = mkTicksExact r n
>   let count = L.Fold (\x a -> Map.insertWith (+) a 1 x) Map.empty identity
>   let countBool = L.Fold (\x a -> x + if a then 1 else 0) 0 identity
>   let histMap = L.fold count $ (\x -> L.fold countBool (fmap (x >) cuts)) <$> ys'
>   let histList = (\x -> Map.findWithDefault 0 x histMap) <$> [0..n]
>   return (zipWith4 V4 (init cuts) (replicate (n+1) 0) (drop 1 cuts) (fromIntegral <$> histList))
>
> 

> hist :: Int -> [Double] -> [V4 Double]
> hist n xs = (zipWith4 V4 (init cuts) (replicate (length xs+1) 0) (drop 1 cuts) (fromIntegral <$> histList))
>   where
>     r = range xs
>     cuts = mkTicksExact r n
>     count = L.Fold (\x a -> Map.insertWith (+) a 1 x) Map.empty identity
>     countBool = L.Fold (\x a -> x + if a then 1 else 0) 0 identity
>     histMap = L.fold count $ (\x -> L.fold countBool (fmap (x >) cuts)) <$> xs
>     histList = (\x -> Map.findWithDefault 0 x histMap) <$> [0..length xs]
>



Scale Robustness
---

Starting with the lowest level scatter chart:

    scatter1 def (xys 1000 0.7)

![](other/predots.svg)

The size of the dots scale with the data, so to bring it back, we run the data through a scaling routine, which normalises the data according to the diagrams unit, which is `V2 (-0.5,0.5) (-0.5,0.5)`:

    scatter1 def ((\x -> scaleR2 (rangeR2 x) x) (xys 1000 0.7))

![](other/dots.svg)

This scaling ensures that axes absolute configuration has the same look and feel. Hiding the scaling in a more general function:

    scatter def [def] [xys 1000 0.7]

![](other/scatter.svg)

The chart is then robust to a wide range of magnitudes:

    scatter def [def] $ fmap (1e-8 *) <$> [xys 1000 0.7]

![](other/scale.svg)

The concrete manifestation of data on a page (the chart), and the heads-up-display (the hud) are then trivially separated:

    chartWith def (const unitSquare) (V2 (Range (0,1), Range(-1000,10000))) scaleR2s [[]]

![](other/axes.svg)

main
===

A few values pulled out of main, on their way to abstraction

> dGrid :: [(Double,Double)]
> dGrid = (,) <$> [0..10] <*> [0..10]
>
> lc1 = zipWith LineConfig [0.01,0.02,0.03] $ opacs 0.5 palette1
> sc1 = zipWith ScatterConfig [0.02,0.05,0.1] $ opacs 0.1 palette1
> swish = [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
> swish2 = [(0.0,0.0),(3.0,3.0)]
>
> linedef :: Chart a
> linedef = line def lc1 (fmap r2 <$> [swish,swish2])
>
> linesdef :: Chart a
> linesdef =
>     line def (((\c -> LineConfig 0.01 $ opac 0.5 c) <$> palette1)) $
>     ((\x -> zipWith V2 (fromIntegral <$> [0..] :: [Double]) x)) <$>
>     ((drop 1 . L.scan L.sum) <$> (unsafeInlineIO $ replicateM 5 $ rvs 100))
>
>
> predotsdef :: Chart a
> predotsdef = scatter1 def (xys 1000 0.7)
>
> dotsdef :: Chart a
> dotsdef = scatter1 def ((\x -> scaleR2 (rangeR2 x) x) (xys 1000 0.7))
>
> scatterdef :: Chart a
> scatterdef = scatter def [def] [xys 1000 0.7]
>
> scattersdef :: Chart a
> scattersdef = scatter def sc1 [xys 1000 0.8, xys 1000 -0.5]
>
> scaledef :: Chart a
> scaledef = scatter def [def] $ fmap (1e-8 *) <$> [xys 1000 0.7]
> 
> histdef :: Chart a
> histdef = rect' def [def] [h]
>
> grid :: Chart a
> grid = scatter def [def] [r2 <$> dGrid]
>
> bardef :: Chart a
> bardef = rect'
>     ( chartAxes .~
>       [ axisTickStyle .~
>         TickLabels labels $ def
>       ]
>       $ def
>     )
>     [def]
>     [zipWith4 V4 [0..10] (replicate 11 0) [1..11] ((view _x) <$> xys 10 0.8)]
>   where
>     labels = fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..]
>
>
>
> r1 = [V4 0 0 1 1, V4 1 0 1 2, V4 2 0 1 5]
>
> axesdef :: Chart a
> axesdef = chartWith def
>   (const unitSquare)
>   (V2 (Range (0,1)) (Range (-1000,10000))) scaleR2s ([[]] :: [[V2 Double]])
>
> doubleHist :: Chart a
> doubleHist = rect' def
>     [ def, rectBorderColor .~ Color 0 0 0 0 $ def]
>     [ hist 50 $ (\(V2 x y) -> x+y) <$> xys 10000 0.8
>     , hist 50 $ (\(V2 x y) -> x+y) <$> xys 10000 0
>     ]
>
> main :: IO ()
> main = do

See develop section below for my workflow.

>   scratchSvg $ doubleHist
>   scratchPng $ doubleHist
>   fileSvg "other/line.svg" (200,200) linedef
>   filePng "other/line.png" (200,200) linedef
>   fileSvg "other/lines.svg" (200,200) linesdef
>   filePng "other/lines.png" (200,200) linesdef
>   fileSvg "other/predots.svg" (200,200) predotsdef
>   filePng "other/predots.png" (200,200) predotsdef
>   fileSvg "other/dots.svg" (200,200) dotsdef
>   filePng "other/dots.png" (200,200) dotsdef
>   fileSvg "other/scatter.svg" (200,200) scatterdef
>   filePng "other/scatter.png" (200,200) scatterdef
>   fileSvg "other/scale.svg" (200,200) $ scaledef
>   filePng "other/scale.png" (200,200) $ scaledef
>   fileSvg "other/scatters.svg" (200,200) scattersdef
>   filePng "other/scatters.png" (200,200) scattersdef
>   fileSvg "other/bar.svg" (200,200) bardef
>   filePng "other/bar.png" (200,200) bardef
>   fileSvg "other/hist.svg" (200,200) histdef
>   filePng "other/hist.png" (200,200) histdef
>   fileSvg "other/grid.svg" (200,200) grid
>   filePng "other/grid.png" (200,200) grid
>   fileSvg "other/axes.svg" (200,200) axesdef
>   filePng "other/axes.png" (200,200) axesdef

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

    unitp f = unitSquare # f # fromVertices # closeTrail # strokeTrail

workflow
---

> scratchSvg :: Chart SVG -> IO ()
> scratchSvg = fileSvg "other/scratchpad.svg" (400,400)
> scratchPng :: Chart Rasterific -> IO ()
> scratchPng = filePng "other/scratchpad.png" (400,400)

Create a markdown version of readme.lhs:

~~~
pandoc -f markdown+lhs -t html -i readme.lhs -o index.html
~~~

Then fire up an intero session, and use padq to display coding results on-the-fly, mashing the refresh button on a browser pointed to readme.html.

or go for a compilation loop like:

~~~
stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o index.html --mathjax --filter pandoc-include && pandoc -f markdown+lhs -t markdown -i readme.lhs -o readme.md --mathjax --filter pandoc-include
~~~

rasterific png renders
===

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

Grid Overlay

![](other/grid.png)
