{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Chart
import Data.Binary
import NumHask.Prelude
import Test.DocTest
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec

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

specChart :: SpecWith ()
specChart =
  describe "withHud" $ do
    it "withHud + mempty ~ hud" $
      toText defaultSvgOptions
      (withHud_ defaultHudOptions sixbyfour (\_ _ _ -> mempty) [corners one])
        `shouldBe`
        toText defaultSvgOptions (hud defaultHudOptions sixbyfour one)
    it "hudWith + lineChart ~ hud <> lineChart_" $
      toText defaultSvgOptions
      (withHud_ defaultHudOptions sixbyfour (lineChart lopts) ls)
        `shouldBe`
        toText defaultSvgOptions (hud defaultHudOptions sixbyfour (range ls) <>
                    lineChart_ lopts sixbyfour ls)
    it "renderChart ChartOptions ~ hud <> chart" $
      toText defaultSvgOptions (renderChart testOptions)
        `shouldBe`
        toText defaultSvgOptions (hud defaultHudOptions sixbyfour (range ls) <>
                    lineChart_ lopts sixbyfour ls)
    it "check ChartOptions binary round trip" $
      (decode . encode) testOptions
        `shouldBe`
        testOptions

testOptions :: ChartOptions
testOptions =
  ChartOptions
   Nothing
   sixbyfour
   [ HudChart defaultHudOptions
   , LineChart (zip lopts ls)
   ]

main :: IO ()
main = do
  doctest
      [ "src/Chart/Data/Time.hs"
      , "src/Chart/Core.hs"
      ]
  -- t1 <- testSpec "Chart" specChart
  -- defaultMain t1
  -- pure ()
