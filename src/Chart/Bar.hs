{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | bar charts
module Chart.Bar
  ( BarOptions(BarOptions)
  , BarValueAcc(..)
  , BarData(..)
  , barDataLowerUpper
  , barChart
  ) where

import Chart.Core
import Chart.Data
import Chart.Hud
import Diagrams.Prelude hiding (Color, D, zero, Additive)
import NumHask.Prelude
import NumHask.Range
import NumHask.Rect
import NumHask.Space
import Chart.Rect
import Data.Colour.Palette.ColorSet
import qualified Control.Foldl as L

data BarValueAcc = BarValueSeparate | BarValueAccumulate deriving (Show, Generic)

-- | the usual bar chart eye-candy
data BarOptions a = BarOptions
  { rectOptions :: [RectOptions]
  , outerGap :: a
  , innerGap :: a
  , displayValues :: Bool
  , accumulateValues :: BarValueAcc
  , orientation :: Orientation
  , hudOptions :: HudOptions a
  } deriving (Show, Generic)

instance (AdditiveUnital a, Fractional a) => Default (BarOptions a) where
  def =
      BarOptions
      ((\x -> RectOptions 0.002 ugrey (d3Colors1 x `withOpacity` 0.5)) <$> [0..10])
      0.1 zero True BarValueSeparate Hori def

data BarData a =
    BarData
    { barData :: [[a]]
    , barRowLabels :: Maybe [Text]
    , barColumnLabels :: Maybe [Text]
    }

-- Convert BarData to rectangles
barRects :: (Enum a, FromInteger a, Ord a, BoundedField a) =>
    BarOptions a -> [[a]] -> [[Rect a]]
barRects (BarOptions _ ogap igap _ add orient _) bs = rects'' orient
    where
      rects'' Hori = rects'
      rects'' Vert = fmap rectTrans <$> rects'
      rects' = zipWith batSet [zero..] (barDataLowerUpper add bs)
      batSet z ys =
          zipWith
          (\x (yl, yh) ->
             abs (Rect (x+ogap+z*bstep) (x+ogap+z*bstep+bstep-igap') yl yh)) [zero..] ys
      n = fromIntegral (length bs)
      bstep = (one - (one+one)*ogap + (n - one) * igap') / n
      igap' = igap * (one - (one+one)*ogap)


-- | convert data to a range assuming a zero bound
barDataLowerUpper :: Additive a => BarValueAcc -> [[a]] -> [[(a, a)]]
barDataLowerUpper add bs = case add of
  BarValueSeparate -> fmap (\x -> (zero,x)) <$> bs
  BarValueAccumulate -> accBarData bs
    where
      accBarData [] = []
      accBarData (x:xs) =
          L.fold
          (L.Fold
            (\(acc,res) a ->
               let acc' = zipWith (+) acc a in
                 (acc', zip acc acc':res))
            (x,[(\x' -> (zero,x')) <$> x])
            (reverse . snd)) xs

barRange :: (BoundedField a, AdditiveUnital a, Ord a, FromInteger a) => [[a]] -> Rect a
barRange ys =
    Rect
    zero
    (fromIntegral $ maximum (length <$> ys))
    (min zero l)
    u
  where
    (Range l u) = foldMap space ys

barChart :: BarOptions Double -> BarData Double -> Chart b
barChart opt@(BarOptions bopts _ _ _ _ _ _) (BarData bs _ _) = ch
  where
    ch = rectChart bopts sixbyfour (barRange bs) (barRects opt bs)

-- fileSvg "other/t1.svg" (600,400) $ (axis (axisTickStyle_ .~ TickRound 20 $ def) (Range -0.75 0.75) (Range 0 3)) <> (barChart (barAdditive_ .~ BarValueSeparate $ barInnerGap_ .~ -0.5 $ def) (BarData [[1,2,3],[4,5,6]] Nothing Nothing))
