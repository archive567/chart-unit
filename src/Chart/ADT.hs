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

-- | A single Chart specification
data ChartSpec
  = GlyphChart [(GlyphOptions, [Pair Double])]
  | LGlyphChart [((LabelOptions, GlyphOptions), [(Text, Pair Double)])]
  | LineChart [(LineOptions, [Pair Double])]
  | GlineChart [((LineOptions, GlyphOptions), [Pair Double])]
  | TextChart [(TextOptions, [(Text, Pair Double)])]
  | RectChart [(RectOptions, [Rect Double])]
  | PixelChart [[Pixel]]
  | ArrowChart [(ArrowOptions, [Arrow])]
  | BarChart BarOptions BarData
  | HudChart HudOptions
  deriving (Show, Generic)

-- | (compound) Chart options
data ChartOptions = ChartOptions
  { chartRange :: Maybe (Rect Double)
  , chartAspect :: Rect Double
  , charts :: [ChartSpec]
  } deriving (Show, Generic)

instance Default ChartOptions where
  def = ChartOptions Nothing sixbyfour []

-- | render a Chart specified using ChartOptions
renderChart :: ChartOptions -> Chart b
renderChart ch@(ChartOptions _ a cs) =
  mconcat (renderSpec a (rangeChart ch) <$> cs)

-- | render a ChartSpec
renderSpec :: Rect Double -> Rect Double -> ChartSpec -> Chart b
renderSpec a r (GlyphChart xs) = glyphChart (fst <$> xs) a r (snd <$> xs)
renderSpec a r (LGlyphChart xs) =
  lglyphChart
  (fst . fst <$> xs)
  (snd . fst <$> xs)
  a
  r
  (snd <$> xs)
renderSpec a r (LineChart xs) = lineChart (fst <$> xs) a r (snd <$> xs)
renderSpec a r (GlineChart xs) =
  glineChart
  (fst . fst <$> xs)
  (snd . fst <$> xs)
  a
  r
  (snd <$> xs)
renderSpec a r (TextChart xs) = textChart (fst <$> xs) a r (snd <$> xs)
renderSpec a r (RectChart xs) = rectChart (fst <$> xs) a r (snd <$> xs)
renderSpec a r (PixelChart xs) = pixelChart a r xs
renderSpec a r (ArrowChart xs) = arrowChart (fst <$> xs) a r (snd <$> xs)
renderSpec _ _ (BarChart o d) = barChart o d
renderSpec a r (HudChart o) = hud o a r

-- | extract the range of a single specification
rangeSpec :: ChartSpec -> Maybe (Rect Double)
rangeSpec (GlyphChart xs) = Just $ range (snd <$> xs)
rangeSpec (LGlyphChart xs) = Just $ range $ (\x -> fmap snd . toList <$> x)
  (snd <$> xs)
rangeSpec (LineChart xs) = Just $ range (snd <$> xs)
rangeSpec (GlineChart xs) = Just $ range (snd <$> xs)
rangeSpec (TextChart xs) = Just $ range $ (\x -> fmap snd . toList <$> x) (snd <$> xs)
rangeSpec (RectChart xs) = Just $ (\rs -> fold $ fold <$> rs) (snd <$> xs)
rangeSpec (PixelChart xs) = Just $ fold $ fold . map pixelRect <$> xs
rangeSpec (ArrowChart xs) = Just $ (\xss -> fold (space . map arrowPos <$> xss)) (snd <$> xs)
rangeSpec (BarChart _ d) = Just $ barRange (d ^. field @"barData")
rangeSpec (HudChart _) = Nothing

-- | calculate the range of a ChartOptions
rangeChart :: ChartOptions -> Rect Double
rangeChart (ChartOptions mr _ cs) = fromMaybe (mconcat $ catMaybes (rangeSpec <$> cs)) mr
