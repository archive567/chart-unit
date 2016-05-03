<link rel="stylesheet" href="other/lhs.css">
<article class="markdown-body">

chart-svg
===

[![Build Status](https://travis-ci.org/tonyday567/chart-svg.png)](https://travis-ci.org/tonyday567/chart-svg)

I've tried charting in haskell many ways: using the Charts package, going via d3, writing js in haskell, and going via ghcjs.

This is attempt 78: locking in a small canonical set of charts that are scale invariant.

todo
---

- explore haskell svg toolkit
- standard scatter chart spec
- bar chart
- line chart
- 1dline
- heatmap
- square composite

To build & run:

~~~
stack build && echo "built" && ./.stack-work/install/x86_64-osx/lts-5.13/7.10.3/bin/readme
~~~

Build, run, render svg output

~~~
filewatcher '**/*.{lhs,hs,cabal}' 'stack install && readme && echo "run"'
~~~

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
> {-# OPTIONS_GHC -fno-warn-unused-binds #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults #-}
> import Protolude
> import System.IO (FilePath)
> import Diagrams.Prelude
> import Diagrams.Backend.SVG

a random source of data. I usually like to work with (0,1) uniform variates, or standard normals.

> import Random


sRGB colour data type

> data C = C Double Double Double Double

What I'm aiming for is a core set of charts I can inject into adhoc number crunching, and the chart can take care of scaling to suit, rather than go through the costly boiler-plate of making a chart pretty.

Here's my first attempt at that - a scatterplot of shapes over the XY plane, with no accroutrements such as axes or legends.

> data Dot t = Dot
>   { _shape :: (Transformable t, TrailLike t, V t ~ V2, N t ~ Double) => Double -> t
>   , _size :: Double
>   , _colour :: C
>   , _sample :: [(Double,Double)]
>   }
>
> toFile :: FilePath -> (Double, Double) -> QDiagram SVG V2 Double Any -> IO ()
> toFile name size = renderSVG name (mkSizeSpec (Just <$> r2 size))
>
> renderDot :: (Alignable t, Monoid t, HasStyle t, Semigroup t, HasOrigin t, Transformable t, TrailLike t, V t ~ V2, N t ~ Double) => Dot t -> t
> renderDot (Dot shape size (C r b g a) sample) = atPoints (p2 <$> sample) (repeat $ shape size # fcA (withOpacity (sRGB r b g) a) # lcA (withOpacity black 0) # lw none # centerXY)
>
> test_001 :: IO ()
> test_001 = do
>   let n = 100
>   sample <- rvcorrL n 0.5
>   let dot = Dot circle 0.1 (C 0.5 0.1 0.9 0.3) sample
>   toFile "other/test_001.svg" (200,200) (renderDot dot)

Browser freezes somewhere above 10k points.

> test_002 :: IO ()
> test_002 = do
>   let n = 10000
>   sample <- rvcorrL n 0.5
>   let dot = Dot circle 0.1 (C 0.5 0.1 0.9 0.02) sample
>   toFile "other/test_002.svg" (200,200) (renderDot dot)
>

Adding an x axis

> test_003 :: IO ()
> test_003 = do
>   let n = 10000
>   sample <- rvcorrL n 0.5
>   let dot = Dot circle 0.1 (C 0.5 0.1 0.9 0.02) sample
>   -- let x = unitX
>   let dotx = beside (r2 (0,0)) mempty (renderDot dot)
>   toFile "other/test_002.svg" (200,200) dotx
>

>
> main :: IO ()
> main = do
>   test_001
>   test_002

100 dots

![](other/test_001.svg)

10000 dots
![](other/test_002.svg)

</article>
