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
      toText def (withHud_ def sixbyfour (\_ _ _ -> mempty) [corners one])
        `shouldBe`
        toText def (hud def sixbyfour one)
    it "hudWith + lineChart ~ hud <> lineChart_" $
      toText def (withHud_ def sixbyfour (lineChart lopts) ls)
        `shouldBe`
        toText def (hud def sixbyfour (range ls) <>
                    lineChart_ lopts sixbyfour ls)
    it "renderChart ChartOptions ~ hud <> chart" $
      toText def (renderChart testOptions)
        `shouldBe`
        toText def (hud def sixbyfour (range ls) <>
                    lineChart_ lopts sixbyfour ls)

    it "check ChartOptions binary round trip" $ do
      (decode . encode) testOptions
        `shouldBe`
        testOptions

testOptions :: ChartOptions
testOptions =
  ChartOptions
   Nothing
   sixbyfour
   [ HudChart def
   , LineChart (zip lopts ls)
   ]

main :: IO ()
main = do
  doctest
      [ "src/Chart/Data/Time.hs"
      , "src/Chart/Core.hs"
      ]
  t1 <- testSpec "Chart" specChart
  defaultMain t1
