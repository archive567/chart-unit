{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
 
-- | Experimental Chart ADT
--
module Chart.ADT
  ( ChartOptions(..)
  , ChartSpec(..)
  , renderSpec
  , renderChart
  , rangeSpec
  , rangeChart
  ) where

import Chart.Arrow
import Chart.Bar
import Chart.Core
import Chart.Glyph
import Chart.Hud
import Chart.Line
import Chart.Rect
import Chart.Text
import Control.Lens
import Data.Default
import Data.Generics.Product (field)
import NumHask.Pair
import NumHask.Prelude
import NumHask.Rect
import NumHask.Space

-- | ADT for charts
data ChartSpec
  = GlyphChart [(GlyphOptions, [Pair Double])]
  | LGlyphChart [(LabelOptions, GlyphOptions, [(Text, Pair Double)])]
  | LineChart [(LineOptions, [Pair Double])]
  | GlineChart [(LineOptions, GlyphOptions, [Pair Double])]
  | TextChart [(TextOptions, [(Text, Pair Double)])]
  | RectChart [(RectOptions, [Rect Double])]
  | PixelChart [[Pixel]]
  | ArrowChart [(ArrowOptions, [Arrow])]
  | BarChart BarOptions BarData
  deriving (Show, Generic)

-- | Chart options
data ChartOptions = ChartOptions
  { chartRange :: Maybe (Rect Double)
  , chartAspect :: Rect Double
  , charts :: [ChartSpec]
  , chartHud :: Maybe HudOptions
  } deriving (Show, Generic)

instance Default ChartOptions where
  def = ChartOptions Nothing sixbyfour [] Nothing

renderChart :: ChartOptions -> Chart b
renderChart ch@(ChartOptions _ a cs mh) =
  mconcat (renderSpec a (rangeChart ch) <$> cs) <>
  maybe mempty hud mh

renderSpec :: Rect Double -> Rect Double -> ChartSpec -> Chart b
renderSpec a r (GlyphChart xs) = glyphChart (fst <$> xs) a r (snd <$> xs)
renderSpec a r (LGlyphChart xs) =
  lglyphChart
  ((\(x,_,_) -> x) <$> xs)
  ((\(_,x,_) -> x) <$> xs)
  a
  r
  ((\(_,_,x) -> x) <$> xs)
renderSpec a r (LineChart xs) = lineChart (fst <$> xs) a r (snd <$> xs)
renderSpec a r (GlineChart xs) =
  glineChart
  ((\(x,_,_) -> x) <$> xs)
  ((\(_,x,_) -> x) <$> xs)
  a
  r
  ((\(_,_,x) -> x) <$> xs)
renderSpec a r (TextChart xs) = textChart (fst <$> xs) a r (snd <$> xs)
renderSpec a r (RectChart xs) = rectChart (fst <$> xs) a r (snd <$> xs)
renderSpec a r (PixelChart xs) = pixelChart a r xs
renderSpec a r (ArrowChart xs) = arrowChart (fst <$> xs) a r (snd <$> xs)
renderSpec _ _ (BarChart o d) = barChart o d

rangeSpec :: ChartSpec -> Rect Double
rangeSpec (GlyphChart xs) = range (snd <$> xs)
rangeSpec (LGlyphChart xs) = range $ (\x -> fmap snd . toList <$> x)
  ((\(_,_,x) -> x) <$> xs)
rangeSpec (LineChart xs) = range (snd <$> xs)
rangeSpec (GlineChart xs) = range ((\(_,_,x) -> x) <$> xs)
rangeSpec (TextChart xs) = range $ (\x -> fmap snd . toList <$> x) (snd <$> xs)
rangeSpec (RectChart xs) = (\rs -> fold $ fold <$> rs) (snd <$> xs)
rangeSpec (PixelChart xs) = fold $ fold . map pixelRect <$> xs
rangeSpec (ArrowChart xs) = (\xss -> fold (space . map arrowPos <$> xss)) (snd <$> xs)
rangeSpec (BarChart _ d) = barRange (d ^. field @"barData")

rangeChart :: ChartOptions -> Rect Double
rangeChart (ChartOptions mr _ cs _) = fromMaybe (mconcat (rangeSpec <$> cs)) mr
