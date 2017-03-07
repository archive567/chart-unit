{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Chart
import Tower.Prelude

import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec

data LawArity a =
    Nonary Bool |
    Unary (a -> Bool) |
    Binary (a -> a -> Bool) |
    Ternary (a -> a -> a -> Bool) |
    Ornary (a -> a -> a -> a -> Bool) |
    Failiary (a -> Property)

type Law a = (TestName, LawArity a)

testLawOf  :: (Arbitrary a, Show a) => [a] -> Law a -> TestTree
testLawOf _ (name, Nonary f) = testProperty name f
testLawOf _ (name, Unary f) = testProperty name f
testLawOf _ (name, Binary f) = testProperty name f
testLawOf _ (name, Ternary f) = testProperty name f
testLawOf _ (name, Ornary f) = testProperty name f
testLawOf _ (name, Failiary f) = testProperty name f

testRange :: TestTree
testRange = testGroup "Chart.Range" $ testLawOf ([]::[Range Double]) <$> rangeLaws

testWithChart :: SpecWith ()
testWithChart = describe "withChart" $ do
    it "axes and chartWith should render the same" $ do
        fileSvg "test/empty.svg" (400,400) emptyChart 
        fileSvg "test/justAxes.svg" (400,400) justAxesChart 
        t1 <- readFile "test/empty.svg"
        t2 <- readFile "test/justAxes.svg"
        t1 `shouldBe` t2

    it "chartWith lines and lines <> axes" $ do
        fileSvg "test/line.svg" (400,400) lineChart
        fileSvg "test/line2.svg" (400,400) line2Chart 
        t1 <- readFile "test/line.svg"
        t2 <- readFile "test/line2.svg"
        t1 `shouldBe` t2
      where
        emptyChart = withChart def (\_ _ -> mempty) [toCorners one]
        justAxesChart = axes def
        lineChart = withChart def (lines lineDefs) lineData
        line2Chart =
            lines lineDefs sixbyfour lineData <>
            axes (chartRange .~ Just (rangeR2s lineData) $ def)

        lineDefs :: [LineConfig]
        lineDefs =
            [ LineConfig 0.01 (Color 0.945 0.345 0.329 0.8)
            , LineConfig 0.02 (Color 0.698 0.569 0.184 0.5)
            , LineConfig 0.005 (Color 0.5 0.5 0.5 1.0)
            ]
        lineData :: [[V2 Double]]
        lineData =
            fmap r2 <$>
            [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
            , [(0.0,0.0),(3.0,3.0)]
            , [(0.5,4.0),(0.5,0)]
            ]

main :: IO ()
main = do
    t1 <- testSpec "withChart" testWithChart
    defaultMain $ testGroup "chart-unit" [testRange, t1]

rangeLaws :: [Law (Range Double)]
rangeLaws =
    [ ("associative: (a + b) + c = a + (b + c)", Ternary (\a b c -> (a + b) + c == a + (b + c)))
    , ("left id: zero + a = a", Unary (\a -> zero + a == a))
    , ("right id: a + zero = a", Unary (\a -> a + zero == a))
    , ("commutative: a + b == b + a", Binary (\a b -> a + b == b + a))
    , ("associative: a `times` (b `times` c) = (a `times` b) `times` c", Ternary (\a b c -> fuzzyeq 1e-4 ((a `times` b) `times` c) (a `times` (b `times` c))))
    , ("left id: one * a = a", Unary (\a -> fuzzyeq 1e-8 (one `times` a) a))
    , ("right id: a * one = a", Unary (\a -> fuzzyeq 1e-8 (a `times` one) a))
    , ("commutative: a * b == b * a", Failiary $ expectFailure . (\a b -> a `times` b == b `times` a))
    , ("recip iso: recip . recip == id", Unary (\a -> zeroRange a || fuzzyeq 1e-4 (recip . recip $ a) a))
    , ("divide: zero range || a /~ a = one", Unary (\a -> zeroRange a || fuzzyeq 1e-8 (a /~ a) one))
    , ("recip divide right: zero range || recip a == one /~ a", Unary (\a -> zeroRange a || fuzzyeq 1e-8 (recip a) (one /~ a)))
    , ("recip left: zero range || recip a * a == one",  Unary (\a -> zeroRange a ||fuzzyeq 1e-8 (recip a `times` a) one))
    , ("recip right: zero range || a * recip a == one", Unary (\a -> zeroRange a || fuzzyeq 1e-8 (a `times` recip a) one))
    ]

fuzzyeq :: (AdditiveGroup a, Ord a) => a -> Range a -> Range a -> Bool
fuzzyeq eps0 (Range (l0,u0)) (Range (l1,u1)) =
    (l0-l1) <= eps0 && (l1-l0) <= eps0 && (u0-u1) <= eps0 && (u1-u0) <= eps0 

zeroRange :: (Eq a) => Range a -> Bool
zeroRange (Range (l,u)) = l==u

