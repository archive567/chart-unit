{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Chart.Range

import Tower.Prelude
import Test.Tasty (TestName, TestTree, testGroup, defaultMain)
import Test.Tasty.QuickCheck
import Test.QuickCheck

instance (Ord a, Arbitrary a) => Arbitrary (Range a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        if a < b
           then pure (Range (a, b))
           else pure (Range (b, a))

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

tests :: TestTree
tests = testGroup "everything"
    [ testGroup "Chart.Range" $ testLawOf ([]::[Range Double]) <$> rangeLaws
    ]

main :: IO ()
main = defaultMain tests

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
fuzzyeq eps (Range (l0,u0)) (Range (l1,u1)) =
    (l0-l1) <= eps && (l1-l0) <= eps && (u0-u1) <= eps && (u1-u0) <= eps 

zeroRange :: (BoundedField a, Eq a) => Range a -> Bool
zeroRange (Range (l,u)) = l==u

