{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Lens.Micro
import NumHask.Prelude
import Data.Generics.Product

ls :: [[Pair Double]]
ls =
  map (uncurry Pair) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

lopts :: [LineOptions]
lopts =
  zipWith
  (\x y -> LineOptions x (ucolor $ withOpacity (d3Colors1 y) 0.6))
  [0.01, 0.02, 0.005]
  [0,1,2]

as :: [AxisOptions]
as = 
  [ defXAxis
  , defYAxis
  , field @"label" . field @"orientation" .~ Pair 0 1 $
    field @"place" .~ PlaceTop $
    defXAxis
  , field @"label" . field @"orientation" .~ Pair 1 0 $
    field @"place" .~ PlaceRight $
    defYAxis
  ] 

titles' :: [(TitleOptions, Text)]
titles' =
  [ (defaultTitleOptions, "Example Chart")
  , ( field @"align" .~ AlignCenter $
      field @"text" . field @"rotation" .~ 90 $
      field @"text" . field @"size" .~ 0.12 $
      field @"place" .~ PlaceLeft $
      defaultTitleOptions
    , "left axis title")
  , ( field @"text" . field @"color" .~ ublue $
      field @"text" . field @"size" .~ 0.08 $
      field @"align" .~ AlignRight $
      field @"place" .~ PlaceBottom $
      defaultTitleOptions
    , "bottom right, non-essential note")
  ]

legends' :: [(LegendType, Text)]
legends' =
  [(LegendText defaultTextOptions, "legend")] <>
  [(LegendPixel (blob ublue) 0.05, "pixel")] <>
  [(LegendRect defaultRectOptions 0.05, "rect")] <>
  [(LegendGLine defaultGlyphOptions defaultLineOptions 0.10, "glyph+line")] <>
  [(LegendGlyph defaultGlyphOptions, "just a glyph")] <>
  zipWith
    (\x y -> (LegendLine x 0.05, y))
    lopts
    ["short", "much longer name", "line 3"]

mainExample :: Chart b
mainExample =
  renderChart
  (ChartOptions
   Nothing
   sixbyfour
   [ LineChart (zip lopts ls)
   , HudChart $
     field @"titles" .~ titles' $
     field @"axes" .~ as $
     field @"axes" %~ map (field @"outerPad" .~ 1) $
     field @"legends" .~ [field @"chartType" .~ legends' $ defaultLegendOptions] $
     defaultHudOptions])

main :: IO ()
main = fileSvg "./chart-unit/other/mainExample.svg" defaultSvgOptions mainExample
