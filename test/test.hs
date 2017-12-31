{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Chart
import Control.Lens
import Data.Generics.Labels()
import NumHask.Prelude
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec
import Test.DocTest

testWithChart :: SpecWith ()
testWithChart =
  describe "withChart" $ do
    it "axes and chartWith should render the same" $ do
      fileSvg "test/empty.svg" (400, 400) emptyChart
      fileSvg "test/justAxes.svg" (400, 400) justAxesChart
      t1 <- readFile "test/empty.svg"
      t2 <- readFile "test/justAxes.svg"
      t1 `shouldBe` t2
    it "chartWith lines and lines <> axes" $ do
      fileSvg "test/line.svg" (400, 400) line1Chart
      fileSvg "test/line2.svg" (400, 400) line2Chart
      t1 <- readFile "test/line.svg"
      t2 <- readFile "test/line2.svg"
      t1 `shouldBe` t2
  where
    emptyChart = withHud def (\_ _ -> mempty) [corners one]
    justAxesChart = hud def
    line1Chart = withHud def (lineChart (repeat def)) lineData
    line2Chart =
      lineChart_ (repeat def) sixbyfour lineData <>
      hud (#range .~ Just (range lineData) $ def)
    lineData :: [[Pair Double]]
    lineData =
      fmap (uncurry Pair) <$>
      [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
      , [(0.0, 0.0), (3.0, 3.0)]
      , [(0.5, 4.0), (0.5, 0)]
      ]

main :: IO ()
main = do
  doctest
      [ "src/Chart/Data/Time.hs"
      -- , "src/Chart/Core.hs"
      ]
  t1 <- testSpec "withChart" testWithChart
  defaultMain $ testGroup "chart-unit" [t1]
