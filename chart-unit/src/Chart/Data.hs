{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

-- | chart data combinators
module Chart.Data
  ( lineOneD
  , vlineOneD
  , hlineOneD
  , rectBars
  , dataXY
  , dataYX
  , rectXY
  , rectYX
  , rectF
  , rectTrans
  ) where

import Diagrams.Prelude hiding (zero, Additive, width)
import NumHask.Pair
import NumHask.Prelude
import NumHask.Rect
import NumHask.Range
import NumHask.Space

-- | Convert a one-dimensional data set to line data
lineOneD :: (Enum a, Additive a) => [a] -> [Pair a]
lineOneD = zipWith Pair [zero..]

-- | Convert a one-dimensional data set to verticle line data
vlineOneD :: (Enum a, Additive a) => [a] -> [[Pair a]]
vlineOneD = zipWith (\x y -> [Pair x zero, Pair x y]) [zero..]

-- | Convert a one-dimensional data set to horizontal line data
hlineOneD :: (Enum a, Additive a) => [a] -> [[Pair a]]
hlineOneD = zipWith (\x y -> [Pair zero x, Pair y x]) [zero..]

-- | Convert a one-dimensional data set to rectangular bars
-- with a gap between
rectBars :: (Enum a, FromInteger a, Ord a, BoundedField a) => a -> [a] -> [Rect a]
rectBars gap' = zipWith (\x y -> abs (Rect (x+gap') (x+one-gap') zero y)) [zero..]

-- | Create line data for a formulae y = f(x)
dataXY :: (BoundedField a, Ord a, FromInteger a) => (a -> a) -> Range a -> Int -> [Pair a]
dataXY f r g = (\x -> Pair x (f x)) <$> grid OuterPos r g

-- | Create line data for a formulae x = f(y)
dataYX :: (BoundedField a, Ord a, FromInteger a) => (a -> a) -> Range a -> Int -> [Pair a]
dataYX f r g = (\x -> Pair (f x) x) <$> grid OuterPos r g

-- | Create rect data for a formulae y = f(x)
rectXY :: (BoundedField a, Ord a, FromInteger a) => (a -> a) -> Range a -> Int -> [Rect a]
rectXY f r g = (\x -> Rect (x-tick/(one+one)) (x+tick/(one+one)) zero (f x)) <$> grid MidPos r g
  where
    tick = width r / fromIntegral g

-- | Create rect data for a formulae x = f(y)
rectYX :: (BoundedField a, Ord a, FromInteger a) => (a -> a) -> Range a -> Int -> [Rect a]
rectYX f r g = (\x -> Rect zero (f x) (x-tick/(one+one)) (x+tick/(one+one))) <$> grid MidPos r g
  where
    tick = width r / fromIntegral g

-- | Create rect data for a formulae c = f(x,y)
rectF :: (Signed a, BoundedField a, Ord a, FromInteger a) => (Pair a -> b) -> Rect a -> Pair Int -> [(Rect a, b)]
rectF f r g = (\x -> (x, f (mid x))) <$> gridSpace r g

-- | transpose the dimensions of a Rect
rectTrans :: Rect a -> Rect a
rectTrans (Rect x y w z) = Rect y x z w
