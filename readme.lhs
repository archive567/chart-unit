<meta charset='utf-8'>
<link rel="stylesheet" href="lhs.css">

chart-svg
---

![](other/scratchpad.svg)


scratchpad
---

This is a scratchpad repo for some chart experiments.  The current scratchpad has up a minimalist and reasonably scale invariant scatterplot.

Hewing close to the spirit of [diagrams](http://projects.haskell.org/diagrams/haddock/index.html), the rendering units are in the x in the range (-0.5,0.5), y in the range (-0.5,0.5), and origin in the center. This is exactly the same setup as unitSquare.

The aim is a scatter chart that renders robustly over both a wide physical sized svg and a wide data range.

todo
---

My rough R&D roadmap is:

- transform data to 1D histograms and work up an iconic bar chart.
- transform data to 2D hitogram and work up a heatmap/surface/contour chart
- add a line chart
- add a 1D chart (which might be iso with an axis)
- add a tiny socket that can:
  - receive new data
  - send mouse clicks

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE NoMonomorphismRestriction #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE TemplateHaskell #-}
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
> import qualified Control.Foldl as L
> import qualified Data.Vector.Unboxed as V
> import qualified Data.Random as R

create some test data
--- 

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

The main downside of using lens is template haskell, which enforces an order to module functions.

The main upside is a rapid gathering of a config file, once you've hacked a prototype together.

> data ScatterConfig = ScatterConfig
>   { _scatterPad :: Double
>   , _scatterSize :: Double
>   , _scatterColor :: AlphaColour Double
>   , _scatterAxisHeight :: Double
>   , _scatterAxisColor :: AlphaColour Double
>   , _scatterNticks :: Int
>   , _scatterVruleSize :: Double
>   , _scatterVruleColor :: AlphaColour Double
>   , _scatterStrutSize :: Double
>   , _scatterTextSize :: Double
>   , _scatterTextColor :: AlphaColour Double
>   }
> 
> rgba (r,g,b,a) = withOpacity (sRGB (r/255) (g/255) (b/255)) a
>
> instance Default ScatterConfig where
>   def =
>     ScatterConfig
>     1.3
>     0.03
>     (rgba(128, 128, 128, 0.1))
>     0.02
>     (rgba(94, 19, 94, 0.5))
>     5
>     0.02
>     (rgba (40, 102, 200, 1))
>     0.02
>     0.04
>     (rgba (30,30,30,1))
>     
> makeLenses ''ScatterConfig
>

And, to try and limit lensification of the project, keep all the lens refs in one spot.

> scatter :: ScatterConfig -> [(Double,Double)] -> QDiagram SVG V2 Double Any
> scatter cfg xys =
>   (beside (r2 (-1,0))
>    (beside (r2 (0,-1))
>     (dots (view scatterSize cfg) (view scatterColor cfg) xys)
>     (axisX
>      (view scatterAxisHeight cfg)
>      (view scatterNticks cfg)
>      (view scatterVruleSize cfg)
>      (view scatterStrutSize cfg)
>      (view scatterTextSize cfg)
>      (view scatterAxisColor cfg)
>      (view scatterVruleColor cfg)
>      (view scatterTextColor cfg)
>      xys
>     )
>    )
>    (axisY
>     (view scatterAxisHeight cfg)
>     (view scatterNticks cfg)
>     (view scatterVruleSize cfg)
>     (view scatterStrutSize cfg)
>     (view scatterTextSize cfg)
>     (view scatterAxisColor cfg)
>     (view scatterVruleColor cfg)
>     (view scatterTextColor cfg)
>     xys
>    )
>    # pad (view scatterPad cfg)
>   )

main
---

>
> main :: IO ()
> main = do
>   padq $ scatter def xys
>


notes
---

This is the main series of steps from the abstract to the concrete, from left to right:

- start with a pointful, no origin shape
- turn it into a Trail
- close the Trail into a SVG-like loop
- turn the Trail into a QDiagram

> steps1 :: QDiagram SVG V2 Double Any
> steps1 = unitSquare # fromVertices # closeTrail # strokeTrail

unit' converts the data to the same space as unitSquare

> unit' xys =
>   let (minX,maxX,minY,maxY) = range2D xys in
>   (\(x,y) -> ( (x-minX)/(maxX-minX) - 0.5
>              , (y-minY)/(maxY-minY) - 0.5)) <$> xys
> unitX' xs =
>   let (minX,maxX) = range1D xs in
>   (\x -> (x-minX)/(maxX-minX) - 0.5) <$> xs

> dots size c xys =
>   atPoints (p2 <$> unit' xys)
>     (repeat $ circle size #
>      fcA c #
>      lcA (withOpacity black 0) #
>      lw none
>     )

axis concepts
---

> axisx height xs = let (min,max) = range1D xs in
>     moveTo (p2 (max,0))
>   . strokeTrail
>   . closeTrail
>   . fromVertices
>   . scaleX (max-min)
>   . scaleY height
>   $ unitSquare
>
> axisy width ys = let (min,max) = range1D ys in
>     moveTo (p2 (0,min))
>   . strokeTrail
>   . closeTrail
>   . fromVertices
>   . scaleY (max-min)
>   . scaleX width
>   $ unitSquare
>
> axisStyle c =
>   fcA c #
>   lcA (withOpacity black 0) #
>   lw none
>
> axisX height nticks vruleSize strutSize textSize axisColor vruleColor textColor xys =
>   let ticksX = tickList (range1D (fst <$> xys)) nticks in
>   let ticksX' = tickList (range1D (fst <$> unit' xys)) nticks in
>   ((atPoints (p2 <$> (\x -> (x,0)) <$> ticksX')
>    (fmap (\tick ->
>            (beside (r2 (0,-1))
>             (beside (r2 (0,-1))
>              (vrule vruleSize # fcA vruleColor)
>              (strutY strutSize))
>             (Diagrams.Prelude.alignedText 0.5 1 tick # scale textSize # fcA textColor)
>            ))
>     (formatToString (prec 2) <$> ticksX)))
>   `atop`
>   (axisx height (fst <$> unit' xys) # axisStyle axisColor))
>
> axisY width nticks vruleSize strutSize textSize
>       axisColor vruleColor textColor xys =
>   ((atPoints (p2 <$> (\y -> (-vruleSize,y)) <$> unitX' ticksY)
>     (fmap (\tick ->
>             (beside (r2 (-1,0))
>              (beside (r2 (-1,0))
>               (hrule vruleSize # fcA vruleColor)
>               (strutX strutSize))
>              (Diagrams.Prelude.alignedText 1 0.5 tick # scale textSize # fcA textColor)
>             ))
>      (formatToString (prec 2) <$> ticksY)))
>    `atop`
>    (axisy width (snd <$> unit' xys) # axisStyle axisColor))
>   where
>     ticksY = tickList (range1D (snd <$> xys)) nticks

ticks
---

> tickList :: (Double,Double) -> Int -> [Double]
> tickList (min,max) n = (first +) . (step *) . fromIntegral <$> [0..n']
>   where
>     span' = max - min
>     step' = 10 ^^ (floor (log(span'/fromIntegral n)/log 10))
>     err = fromIntegral n / span' * step'
>     step
>       | err <= 0.15 = 10 * step'
>       | err <= 0.35 = 5 * step'
>       | err <= 0.75 = 2 * step'
>       | otherwise = step'
>     first = step * fromIntegral (floor (min/step) + 1)
>     last = fromIntegral (floor (max/step)) * step
>     n' = round ((last - first)/step)

helpers
---

Quick access to a concrete rendering at various levels of abstraction
- a QDiagram
- a Trail
- a Path

> padq :: QDiagram SVG V2 Double Any -> IO ()
> padq t =
>   toFile "other/scratchpad.svg" (400,400) t
>
> padt :: Trail V2 Double -> IO ()
> padt = padq . strokeTrail
>
> padp :: Path V2 Double -> IO ()
> padp = padq . strokePath
>
> hex s = sRGB r b g
>   where
>     (RGB r g b) = toSRGB $ sRGB24read s
>
> toFile :: FilePath -> (Double, Double) -> QDiagram SVG V2 Double Any -> IO ()
> toFile name size = renderSVG name (mkSizeSpec (Just <$> r2 size))
>

ranges
---

>
> range2D = L.fold (L.Fold step initial extract)
>   where
>     step Nothing (x,y) = Just (x,x,y,y)
>     step (Just (minX,maxX,minY,maxY)) (x,y) =
>       Just (min' x minX, max' x maxX, min' y minY, max' y maxY)
>     max' x1 x2 = if x1 >= x2 then x1 else x2
>     min' x1 x2 = if x1 <= x2 then x1 else x2
>     initial = Nothing
>     extract x = case x of
>       Nothing -> (-0.5, 0.5, -0.5, 0.5)
>       Just x' -> x'
>
> range1D = L.fold (L.Fold step initial extract)
>   where
>     step Nothing x = Just (x,x)
>     step (Just (min,max)) x =
>       Just (min' x min, max' x max)
>     max' x1 x2 = if x1 >= x2 then x1 else x2
>     min' x1 x2 = if x1 <= x2 then x1 else x2
>     initial = Nothing
>     extract x = case x of
>       Nothing -> (-0.5, 0.5)
>       Just x' -> x'


chart-svg
---

[![Build Status](https://travis-ci.org/tonyday567/chart-svg.png)](https://travis-ci.org/tonyday567/chart-svg)

Build, run, render readme

~~~
filewatcher '**/*.{lhs,hs,cabal}' 'stack install && readme && pandoc -f markdown+lhs -t html -i readme.lhs -o readme.html && echo "run"' 
~~~
