{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Chart
import NumHask.Prelude

import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.Hspec

testWithChart :: SpecWith ()
testWithChart = describe "withChart" $ do
    it "axes and chartWith should render the same" $ do
        fileSvg "test/empty.svg" (400,400) emptyChart 
        fileSvg "test/justAxes.svg" (400,400) justAxesChart 
        t1 <- readFile "test/empty.svg"
        t2 <- readFile "test/justAxes.svg"
        t1 `shouldBe` t2

    it "chartWith lines and lines <> axes" $ do
        fileSvg "test/line.svg" (400,400) line1Chart
        fileSvg "test/line2.svg" (400,400) line2Chart 
        t1 <- readFile "test/line.svg"
        t2 <- readFile "test/line2.svg"
        t1 `shouldBe` t2
      where
        emptyChart = withChart def (\_ _ -> mempty) [corners one]
        justAxesChart = axes def
        line1Chart = withChart def (lineChart lineDefs) lineData
        line2Chart =
            lineChart lineDefs sixbyfour lineData <>
            axes (chartRange .~ Just (rangeR2s lineData) $ def)

        lineDefs :: [LineConfig]
        lineDefs =
            [ LineConfig 0.01 (Color 0.945 0.345 0.329 0.8)
            , LineConfig 0.02 (Color 0.698 0.569 0.184 0.5)
            , LineConfig 0.005 (Color 0.5 0.5 0.5 1.0)
            ]
        lineData :: [[Pair Double]]
        lineData =
            fmap (uncurry Pair) <$>
            [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
            , [(0.0,0.0),(3.0,3.0)]
            , [(0.5,4.0),(0.5,0)]
            ]

main :: IO ()
main = do
    t1 <- testSpec "withChart" testWithChart
    defaultMain $ testGroup "chart-unit" [t1]
