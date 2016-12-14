<meta charset="utf-8"> <link rel="stylesheet" href="other/lhs.css">
<script type="text/javascript" async
  src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>
[chart-unit](https://tonyday567.github.io/chart-unit) [![Build Status](https://travis-ci.org/tonyday567/chart-unit.png)](https://travis-ci.org/tonyday567/chart-unit)
===

[repo](https://tonyday567.github.com/chart-unit)

latest development:

![](other/scratchpad.svg)

~~~
scratch $ pad 1.1 $ beside (r2 (-1,0)) (beside (r2 (0,-1)) (((unitSquare # fcA (color $ Color 0.2 0.2 0.2 0.05) # lw 0) <> (centerXY $ scaleX (1/1.5) $ scaleY (1/10) $ (clipped (pathFromTrail $ (closeTrail $ fromVertices (p2 <$> [(0.5,0),(0.5,10),(2,10),(2,0)]))) $ ((mconcat . zipWith line1 (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)))) (fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)], [(0.0,0.0),(3.0,3.0)]]))))) (axis def (Range (0.5,2)))) (axis (axisOrientation .~ Y $ def) (Range (0,2)))
~~~

This is my manual hack of what should happen to the line chart example when drawn using a scale different to the data scale. 

Next step is to refactor back into the chartHub

tl;dr
-----

This slowly growing collection of charts:

-   renders nicely in svg over a wide chart size. png is not so good.
-   renders similarly at different scale
-   are opinionated minimalism
-   are unit shapes in the spirit of the
    [diagrams](http://projects.haskell.org/diagrams/doc/quickstart.html)
    design space, making combining a snap.
-   can be quickly integrated into ad-hoc haskell data analytics,
    providing a visual feedback loop.





charts - svg
======

Scatter

![](other/exampleScatter.svg)

Scatters

![](other/exampleScatter2.svg)

Histogram

![](other/exampleHist.svg)

Histograms

![](other/exampleHist2.svg)

Line

![](other/exampleLine.svg)

Lines

![](other/exampleManyLines.svg)

Labelled Bar Chart

![](other/exampleBar.svg)

Arrows Chart

![](other/exampleArrows.svg)

![](other/exampleArrows2.svg)

api notes
---------

Most of the api can be seen in the chartWith sig:

``` {.sourceCode .literate .haskell}
    chartWith :: (Traversable f, Traversable g, R2 r) => 
        ChartConfig
        -> (g (f (r Double)) -> Chart b) -- double-containered R? chart renderer
        -> V2 (Range Double)             -- render range
        -> (V2 (Range Double) -> g (f (r Double)) -> g (f (r Double))) -- scaler
        -> g (f (r Double)) -- data
        -> Chart b
    chartWith (ChartConfig p axes) renderer range' scaler ms =
```

data
---

Double-containered dimensioned data covers what a chart charts - one or more data sets with at least an x and y dimension (called R2 in the linear library).

Most chart variations are about what to do with the extra dimensions in the data. A rectangle, for example, is built from 4 dimensions, an anchoring of position in XY space, with a width (W) and height (Z).  A contour map is three dimensional data (V3 in linear), with placement as XY and color as Z.

ranging
---

A range represents boundaries of a space.  Diagrams often uses unit to refer to a `V2 (Range (-0.5,0.5)) (Range (-0.5,0.5))`. 

scaling
---

A scaler converts data to other ranges, and helps get everything on the same page (literally).


Scale Robustness
----------------

Starting with the lowest level scatter chart (see examples/examples.hs for complete code):

    scatter1 def (xys 1000 0.7)

![](other/exampleDots.svg)

The size of the dots scale with the data, so to bring it back, we run
the data through a scaling routine, which normalises the data according
to the diagrams unit, which is `V2 (-0.5,0.5) (-0.5,0.5)`:

    scatter1 def $ ((\x -> scaleR2 (rangeR2 x) x) <$> (xys 1000 0.7))

![](other/exampleDotsScaled.svg)

This scaling ensures that configuration paramters such as dot size, and axis look-n-feel is invariant to data scale. Hiding the scaling and axes creation in a more general function:

    scatter def [def] [xys 1000 0.7]

![](other/exampleScatter.svg)

The chart is then robust to a wide range of magnitudes:

    scatter def [def] $ fmap (1e-8 *) <$> [xys 1000 0.7]

![](other/exampleDotsScaled2.svg)


Separation of Chart and HUD
---

It's useful to separate a chart into two distinct bits:

- the Chart.  A concrete manifestation of data on the XY plane, with extra dimensions as aethestics (color, size, time, 1D or 3D projections into 2D)

- the HUD.  A Heads-Up-Display aka decoration around the chart designed to help users interpret and navigate the data representation.

The code below, for example, draws an X and Y axis without any data:

    chartHud def (const mempty) (V2 (Range (-1e8,1e8)) (Range (-1e-8,1e-8))) rescaleR2s ([[]] :: [[V2 Double]])

![](other/exampleAxes.svg)

HUDs can be built with chart routines.  Here's a grid overlay that can be attached to any unit chart. Note how the axis ticks line up exactly with middle of the dots. I now think of the axes as a small subset of possible HUDs to help users
interpret data.

    unitScatter def [def] [V2 <$> [0..10] <*> [0..10]]

![](other/exampleGrid.svg)

diagrams development recipe
---------------------------

In constructing new chart units, I keep this list around:

-   diagrams go from abstract to concrete
-   start with the unitSquare: 4 points, 1x1, origin in the center
-   work out where the origin should be, given the scaling needed.
-   turn the pointful shape into a Trail
-   close the Trail into a SVG-like loop
-   turn the Trail into a QDiagram

You can slide up and down the various diagrams abstraction levels
creating transformations at each level. For example, here's something I
use to work at the point level:

    unitp f = unitSquare # f # fromVertices # closeTrail # strokeTrail

workflow
--------

``` {.sourceCode .literate .haskell}
scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (400,400)
```

I tend to work in ghci a lot, using scratch to try code out, and mashing the refresh button in the browser.

As new charts emerge I then includ ethem in examples and integrate them with readme.md:

    stack install && chart-unit-examples && pandoc -f markdown -t html -i readme.md -o index.html --mathjax --filter pandoc-include

rasterific space leak
--

Until I can work out a space leak, I'm letting png slide.

-- import Diagrams.Backend.Rasterific (Rasterific)
-- scratchPng :: Chart Rasterific -> IO ()
-- scratchPng = filePng "other/scratchpad.png" (400,400)
-- filePng "other/bar.png" s exampleBar

