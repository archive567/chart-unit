{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

-- | chart data combinators
module Chart.Data
  ( lineOneD
  , vlineOneD
  , rectOneD
  , dataXY
  , dataYX
  , rectXY
  , rectYX
  , rectF
  ) where

import Diagrams.Prelude hiding ((<>), zero, Additive)
import NumHask.Pair
import NumHask.Prelude
import NumHask.Rect
import NumHask.Range
import NumHask.Space

-- | Convert a one-dimensional data set to line data
lineOneD :: (Enum a, AdditiveUnital a) => [a] -> [Pair a]
lineOneD = zipWith Pair [zero..]

-- | Convert a one-dimensional data set to verticle line data
vlineOneD :: (Enum a, AdditiveUnital a) => [a] -> [[Pair a]]
vlineOneD = zipWith (\x y -> [Pair x zero, Pair x y]) [zero..]

-- | Convert a one-dimensional data set to bars
rectOneD :: (Enum a, FromInteger a, Ord a, BoundedField a, Additive a, MultiplicativeUnital a) => [a] -> [Rect a]
rectOneD xs = zipWith (\x y -> abs (Rect x (x+one) zero y)) [zero..] xs

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
    tick = (NumHask.Space.width r) / fromIntegral g

-- | Create rect data for a formulae x = f(y)
rectYX :: (BoundedField a, Ord a, FromInteger a) => (a -> a) -> Range a -> Int -> [Rect a]
rectYX f r g = (\x -> Rect zero (f x) (x-tick/(one+one)) (x+tick/(one+one))) <$> grid MidPos r g
  where
    tick = (NumHask.Space.width r) / fromIntegral g

-- | Create rect data for a formulae c = f(x,y)
rectF :: (Signed a, BoundedField a, Ord a, FromInteger a) => (Pair a -> b) -> Rect a -> Pair Int -> [(Rect a, b)]
rectF f r g = (\x -> (x, f (mid x))) <$> gridSpace r g
