{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Chart.Types where

import Protolude
import Data.List ((!!))
import Diagrams.Prelude hiding (Color(..))
import qualified Diagrams.TwoD.Text

data Range = Range
    { upper :: Double
    , lower :: Double
    } deriving (Show, Eq)

data RangeXY = RangeXY
    { rangeX :: Range
    , rangeY :: Range
    } deriving (Show, Eq)

type P = (Double,Double)
type D = [Double]
type S = [(Double,Double)]
type M = [[(Double,Double)]]

type Chart a =
    ( Renderable (Path V2 Double) a
    , Renderable (Diagrams.TwoD.Text.Text Double) a) =>
    QDiagram a V2 Double Any

data Orientation = X | Y

data Placement = AxisLeft | AxisRight | AxisTop | AxisBottom

data TickStyle = TickNone | TickLabels [Text] | TickRound Int | TickExact Int

data Color = Color { _r :: Double, _g :: Double, _b :: Double, _a :: Double}

color ∷ Color → AlphaColour Double
color (Color r g b a)= withOpacity (sRGB r g b) a

colorAxis1 ∷ Color
colorAxis1 = Color 0.4 0.1 0.4 0.5
colorAxis2 ∷ Color
colorAxis2 = Color 0.1 0.4 0.8 0.5
colorAxis3 ∷ Color
colorAxis3 = Color 0.2 0.2 0.2 0.7
colorScatter ∷ Color
colorScatter = Color 0.5 0.5 0.5 0.1
colorBar ∷ Color
colorBar = palette1 !! 1
colorLine ∷ Color
colorLine = palette1 !! 5

palette1 ∷ [Color]
palette1 = [ Color 0.333 0.333 0.333 1.00 -- grey
           , Color 0.365 0.647 0.855 1.00 -- blue
           , Color 0.980 0.647 0.855 1.00 -- orange
           , Color 0.376 0.741 0.408 1.00 -- green
           , Color 0.945 0.486 0.690 1.00 -- pink
           , Color 0.698 0.569 0.184 1.00 -- brown
           , Color 0.698 0.463 0.698 1.00 -- purple
           , Color 0.871 0.812 0.247 1.00 -- yellow
           , Color 0.945 0.345 0.329 1.00 -- red
           ]

opac :: Double -> [Color] -> [Color]
opac t cs = (\(Color r g b o) -> Color r g b (o*t)) <$> cs

data AxisConfig = AxisConfig
  { _axisPad :: Double
  , _axisOrientation :: Orientation
  , _axisPlacement :: Placement
  , _axisHeight :: Double
  , _axisColor :: Color
  , _axisMarkSize :: Double -- mark length
  , _axisMarkColor :: Color
  , _axisInsideStrut :: Double -- distance of axis from plane
  , _axisLabelStrut :: Double -- distance of label from mark
  , _axisTextSize :: Double
  , _axisTextColor :: Color
  , _axisTickStyle :: TickStyle
  , _axisAlignedTextRight :: Double
  , _axisAlignedTextBottom :: Double
  }

instance Default AxisConfig where
  def =
    AxisConfig
    1
    X
    AxisBottom
    0.02
    colorAxis1
    0.02
    colorAxis2
    0.3
    0.02
    0.04
    colorAxis3
    (TickRound 8)
    0.5
    1

makeLenses ''AxisConfig 

data ChartConfig = ChartConfig
  { _chartPad :: Double
  , _chartAxes :: [AxisConfig]
  }

instance Default ChartConfig where
  def =
    ChartConfig
    1.3
    [def,
     axisAlignedTextBottom .~ 0.65 $
     axisAlignedTextRight .~ 1 $
     axisOrientation .~ Y $
     axisPlacement .~ AxisLeft $
     def]

makeLenses ''ChartConfig

data ScatterConfig = ScatterConfig
  { _scatterSize :: Double
  , _scatterColor :: Color
  }

instance Default ScatterConfig where
  def = ScatterConfig 0.03 colorScatter

makeLenses ''ScatterConfig

data BarConfig = BarConfig
  { _barSep :: Double
  , _barColor :: Color
  }

instance Default BarConfig where
  def = BarConfig 0.01 colorBar

makeLenses ''BarConfig

data LineConfig = LineConfig
  { _lineSize :: Double
  , _lineColor :: Color
  }

instance Default LineConfig where
  def = LineConfig 0.02 colorLine

makeLenses ''LineConfig

data ChartType =
    LineChart LineConfig |
    BarChart BarConfig |
    ScatterChart ScatterConfig


