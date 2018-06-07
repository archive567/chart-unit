{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}

module Main where

import Chart
import NumHask.Prelude
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec
import Test.DocTest

testWithChart :: SpecWith ()
testWithChart =
  describe "withHud" $ do
    it "withHud + mempty ~ hud" $
      toText def (withHud_ def sixbyfour (\_ _ _ -> mempty) [corners one])
        `shouldBe`
        toText def (hud def sixbyfour one)
    it "hudWith + chart ~ chart_ <> hud" $
      toText def (withHud_ def sixbyfour (lineChart (repeat def)) lineData)
        `shouldBe`
        toText def (hud def sixbyfour (range lineData) <>
                    lineChart_ (repeat def) sixbyfour lineData)
    it "renderChart [chart, hud] ~ hud <> chart" $
      toText def (renderChart
                  (ChartOptions Nothing sixbyfour
                    [ HudChart def
                    , LineChart ((\x -> (def,x)) <$> lineData)]))
        `shouldBe`
        toText def (hud def sixbyfour (range lineData) <>
                    lineChart_ (repeat def) sixbyfour lineData)

lineData :: [[Pair Double]]
lineData =
  fmap (uncurry Pair) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

main :: IO ()
main = do
  fileSvg "other/test1.svg" def (withHud_ def sixbyfour (lineChart (repeat def)) lineData)
  fileSvg "other/test2.svg" def
    (lineChart_ (repeat def) sixbyfour lineData <>
     hud def sixbyfour (range lineData))
  doctest
      [ "src/Chart/Data/Time.hs"
      -- , "src/Chart/Core.hs"
      ]
  t1 <- testSpec "withChart" testWithChart
  defaultMain $ testGroup "chart-unit" [t1]
