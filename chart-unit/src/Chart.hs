{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | The Chart module exports all of the chart-unit functionality, and most of what you need from outside libraries.
--
-- `Chart` is designed to be used in conjunction with both the numhask and diagrams preludes. Diagrams.Prelude conatins much of the lens library and many re-exports that clash with NumHask, so best to import qualified.
--
-- > {-# LANGUAGE FlexibleContexts #-}
-- > {-# LANGUAGE NoImplicitPrelude #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- > {-# LANGUAGE OverloadedLabels #-}
-- > {-# OPTIONS_GHC -Wall #-}
-- > 
-- > import Chart
-- > import Control.Lens
-- > import Data.Generics.Labels()
-- > import qualified Diagrams.Prelude as D
-- > import NumHask.Prelude
--
-- Each chart type is built up from a few different elements:
--
-- - data
--
-- - a type of chart
--
-- - representation options specific to the chart type
--
-- - axes
--
-- - other chart bling, such as titles and legends
--
-- Chart data is most often one or more traversable series.  Also most often, the data is 2-dimensional, representing where on the chart to place the representation.  Some sort of `(Traversable f) => [f Pair]` is commonly used in the library api.
--
-- > ls :: [[Pair Double]]
-- > ls =
-- >   map (uncurry Pair) <$>
-- >   [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
-- >   , [(0.0, 0.0), (3.0, 3.0)]
-- >   , [(0.5, 4.0), (0.5, 0)]
-- >   ]
--
-- Each data series has representation options dependent on the chart type
--
-- > lopts :: [LineOptions]
-- > lopts =
-- >   zipWith
-- >   (\x y -> LineOptions x (withOpacity (d3Colors1 y) 0.6))
-- >   [0.01, 0.02, 0.005]
-- >   [0,1,2]
-- >
--
-- The lens library is used extensively for configuration, and generic-lens-labels has been adopted to reduce line noise ...
--
-- > as :: [AxisOptions]
-- > as = 
-- >   [ defXAxis
-- >   , defYAxis
-- >   , #label . #orientation .~ Pair 0 1 $
-- >     #place .~ PlaceTop $
-- >     defXAxis
-- >   , #label . #orientation .~ Pair 1 0 $
-- >     #place .~ PlaceRight $
-- >     defYAxis
-- >   ] 
--
-- in the examples (but not in core library code), Data.Generic.Labels is used which has its detractions in the form of orphan instance fuss and bother. For example, using core generic-lens:
--
-- > #label . #orientation .~ Pair 0 1
--
-- translates to:
--
-- > field @"label" . field @"orientation" .~ Pair 0 1
--
-- which is also a pretty fine api.
--
-- Using data-default, lens and OverloadedLabels tends to encourage a vertical style, which may annoy line counters, but lead to clear code and ease of editing.
--
-- > titles' :: [(TitleOptions, Text)]
-- > titles' =
-- >   [ (def, "Example Chart")
-- >   , ( #align .~ AlignCenter $
-- >       #text . #rotation .~ 90 $
-- >       #text . #size .~ 0.12 $
-- >       #place .~ PlaceLeft $
-- >       def
-- >     , "left axis title")
-- >   , ( #text . #color .~ ublue $
-- >       #text . #size .~ 0.08 $
-- >       #align .~ AlignRight $
-- >       #place .~ PlaceBottom $
-- >       def
-- >     , "bottom right, non-essential note")
-- >   ]
-- > 
-- > legends' :: [(LegendType, Text)]
-- > legends' =
-- >   [(LegendText def, "legend")] <>
-- >   [(LegendPixel (blob ublue) 0.05, "pixel")] <>
-- >   [(LegendRect def 0.05, "rect")] <>
-- >   [(LegendGLine def def 0.10, "glyph+line")] <>
-- >   [(LegendGlyph def, "just a glyph")] <>
-- >   zipWith
-- >     (\x y -> (LegendLine x 0.05, y))
-- >     lopts
-- >     ["short", "much longer name", "line 3"]
-- >
--
-- All of which makes chart-unit highly customisable ...
--
-- > mainExample :: Chart b
-- > mainExample = withHud_ opts sixbyfour (lineChart lopts) ls
-- >   where
-- >     opts =
-- >       #titles .~ titles' $
-- >       #axes .~ as $
-- >       #axes %~ map (#outerPad .~ 1) $
-- >       #legends .~ [#chartType .~ legends' $ def] $
-- >       def
-- > 
-- > main :: IO ()
-- > main = fileSvg "other/mainExample.svg" def mainExample
-- > 
--
-- There are three different ways of combining charts (and note adding a hud to a chart is a subset of combining charts):
--
-- - mappend them
--
-- > hud ho asp r <>
-- > lineChart ld sixbyfour r d
--
-- - use `withHud`
--
-- > withHud_ ho asp (lineChart ld) d
--
-- - use `renderChart`
--
-- > renderChart (ChartOptions (Just r) asp [HudChart ho, LineChart (zip ld d)])
--
-- And these three methods are morally equivalent.
--
module Chart
  ( -- * chart-unit
    module Chart.ADT
  , module Chart.Arrow
  , module Chart.Bar
  , module Chart.Core
  , module Chart.Data
  , module Chart.Data.Time
  , module Chart.Glyph
  , module Chart.Hud
  , module Chart.Line
  , module Chart.Rect
  , module Chart.Svg
  , module Chart.Text
    -- * numhask-range
  , module NumHask.Pair
  , module NumHask.Space
  , module NumHask.Range
  , module NumHask.Rect
    -- * color
  , module Data.Colour
  , module Data.Colour.Names
  , module Data.Colour.Palette.Harmony
  , module Data.Colour.Palette.ColorSet
    -- * fonts
  , module Graphics.SVGFonts
    -- * Default
  , Default(..)
  ) where

import Chart.ADT
import Chart.Arrow hiding (color)
import Chart.Bar hiding (orientation)
import Chart.Core
import Chart.Data
import Chart.Data.Time
import Chart.Glyph hiding (color, size, borderSize, borderColor)
import Chart.Hud
import Chart.Line hiding (color)
import Chart.Rect hiding (borderSize, borderColor)
import Chart.Svg
import Chart.Text hiding (orientation, size, color)
import Data.Colour
import Data.Colour.Names
import Data.Colour.Palette.ColorSet
import Data.Colour.Palette.Harmony
import Data.Default (Default(..))
import Graphics.SVGFonts hiding (textFont)
import NumHask.Pair
import NumHask.Range
import NumHask.Rect
import NumHask.Space hiding (width)

