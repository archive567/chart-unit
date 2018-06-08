{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}

import Chart
import Protolude

ls :: [[Pair Double]]
ls =
  map (uncurry Pair) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

lopts :: [LineOptions]
lopts =
  zipWith LineOptions
  [0.015, 0.03, 0.01]
  [ UColor 0.773 0.510 0.294 0.6
  , UColor 0.235 0.498 0.169 0.6
  , UColor 0.204 0.161 0.537 1.0
  ]

titles' :: [(TitleOptions, Text)]
titles' =
  [ (defaultTitleOptions, "Example Chart")
  , ( field @"text" . field @"size" .~ 0.08 $
      field @"align" .~ AlignRight $
      field @"place" .~ PlaceBottom $
      defaultTitleOptions
    , "an example chart for chart-unit")
  ]

legends' :: [(LegendType, Text)]
legends' =
  zipWith
    (\x y -> (LegendLine x 0.1, y))
    lopts
    ["hockey stick", "diagonal line", "verticle line"]

mainExample :: Chart b
mainExample =
  renderChart
  (ChartOptions
   Nothing
   sixbyfour
   [ LineChart (zip lopts ls)
   , HudChart $
     field @"titles" .~ titles' $
     field @"axes" %~ map (field @"outerPad" .~ 1) $
     field @"legends" .~ [ field @"chartType" .~ legends' $
                           defaultLegendOptions] $
     defaultHudOptions])

main :: IO ()
main = fileSvg "./chart-unit/other/mainExample.svg" defaultSvgOptions mainExample
