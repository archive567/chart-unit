{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | bar charts
module Chart.Bar
  ( BarOptions(..)
  , BarValueAcc(..)
  , BarData(..)
  , barDataLowerUpper
  , barRange
  , barChart
  ) where

import Chart.Core
import Chart.Data
import Chart.Hud
import Chart.Rect
import qualified Control.Foldl as L
import Data.Colour.Palette.ColorSet
import Data.Generics.Product
import Diagrams.Prelude hiding (Additive, Color, D, zero, (<>))
import NumHask.Prelude
import NumHask.Range
import NumHask.Rect
import NumHask.Space

-- | whether to separate each Rect grouping or accumulate them
data BarValueAcc
  = BarValueSeparate
  | BarValueAccumulate
  deriving (Show, Eq, Generic)

-- | the usual bar chart eye-candy
data BarOptions = BarOptions
  { rectOptions :: [RectOptions]
  , outerGap :: Double
  , innerGap :: Double
  , displayValues :: Bool
  , accumulateValues :: BarValueAcc
  , orientation :: Orientation
  , hudOptions :: HudOptions
  } deriving (Show, Eq, Generic)

instance Default BarOptions where
  def =
    BarOptions
      ((\x -> RectOptions 0.002 ugrey (ucolor $ d3Colors1 x `withOpacity` 0.5)) <$>
       [0 .. 10])
      0.1
      zero
      True
      BarValueSeparate
      Hori
      def

-- | imagine a data frame ...
data BarData = BarData
  { barData :: [[Double]]
  , barRowLabels :: Maybe [Text]
  , barColumnLabels :: Maybe [Text]
  } deriving (Show, Eq, Generic)

-- | Convert BarData to rectangles
barRects ::
    BarOptions
  -> [[Double]]
  -> [[Rect Double]]
barRects (BarOptions _ ogap igap _ add orient _) bs = rects'' orient
  where
    rects'' Hori = rects'
    rects'' Vert = fmap rectTrans <$> rects'
    rects' = zipWith batSet [zero ..] (barDataLowerUpper add bs)
    batSet z ys =
      zipWith
        (\x (yl, yh) ->
           abs
             (Rect
                (x + ogap + z * bstep)
                (x + ogap + z * bstep + bstep - igap')
                yl
                yh))
        [zero ..]
        ys
    n = fromIntegral (length bs)
    bstep = (one - (one + one) * ogap + (n - one) * igap') / n
    igap' = igap * (one - (one + one) * ogap)

-- | convert data to a range assuming a zero bound
-- a very common but implicit assumption in a lot of bar charts
barDataLowerUpper :: BarValueAcc -> [[Double]] -> [[(Double, Double)]]
barDataLowerUpper add bs =
  case add of
    BarValueSeparate -> fmap (\x -> (zero, x)) <$> bs
    BarValueAccumulate -> accBarData bs
      where accBarData [] = []
            accBarData (x:xs) =
              L.fold
                (L.Fold
                   (\(acc, res) a ->
                      let acc' = zipWith (+) acc a
                      in (acc', zip acc acc' : res))
                   (x, [(\x' -> (zero, x')) <$> x])
                   (reverse . snd))
                xs

-- | calculate the Rect range of a bar data set (imagine a data frame ...)
barRange ::
     [[Double]] -> Rect Double
barRange ys = Rect zero (fromIntegral $ maximum (length <$> ys)) (min zero l) u
  where
    (Range l u) = foldMap space ys

-- | A bar chart
--
-- > barExample :: Chart b
-- > barExample  =
-- >   barChart def (BarData [ys] Nothing Nothing) <>
-- >   hud
-- >   ( #titles .~ [(def,"Bar Chart")] $
-- >     #axes .~
-- >     [ #tickStyle .~
-- >       TickLabels labels' $
-- >       def
-- >     ] $
-- >     #range .~ Just (fold (abs <$> rs)) $
-- >     def)
-- >   where
-- >     labels' = fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..]
-- >     rs = rectBars 0.1 ys
-- >     ys = [1,2,3,5,8,0,-2,11,2,1]
--
-- ![barChart example](other/barExample.svg)
--
barChart :: BarOptions -> BarData -> Chart b
barChart bo bd =
  rectChart (bo ^. field @"rectOptions") sixbyfour
  (barRange (bd ^. field @"barData")) (barRects bo (bd ^. field @"barData"))
