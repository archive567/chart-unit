{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Chart.Types where

import Tower.Prelude
import Data.List ((!!))
import Diagrams.Prelude hiding (Color(..))
import qualified Diagrams.TwoD.Text
import Chart.Range

-- | a Chart has a concrete scale, and combinatory options amount to mappend (on top of) and beside
type Chart a =
    ( Renderable (Path V2 Double) a
    ) =>
    QDiagram a V2 Double Any

-- | an alternative synonym where text is involved.
type Chart' a =
    ( Renderable (Path V2 Double) a
    , Renderable (Diagrams.TwoD.Text.Text Double) a
    ) =>
    QDiagram a V2 Double Any

-- | The concrete nature of a QDiagram, and a desire to scale data and hud items naturally, a QChart is mostly a late binding of the XY plane (or aspect ratio) that the chart is to be projected on to and the data.
data QChart a = forall b. QChart
    { _qChart :: ( ( Renderable (Diagrams.TwoD.Text.Text Double) a)
                  , Renderable (Path V2 Double) a) =>
                XY -> b -> QDiagram a V2 Double Any
    , _qXY :: XY
    , _qData :: b
    }

makeLenses ''QChart

-- | chart rendering polymorphic to backend and configurator
type Renderer b c =
    forall f r. (Renderable (Path V2 Double) b, R2 r, Traversable f) =>
    [c] ->
    XY ->
    [f (r Double)] ->
    QDiagram b V2 Double Any

data Orientation = X | Y

data Placement = AxisLeft | AxisRight | AxisTop | AxisBottom

data TickStyle = TickNone | TickLabels [Text] | TickRound Int | TickExact Int

data Color = Color { _red :: Double, _green :: Double, _blue :: Double, _aaa :: Double}

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

opacs :: Double -> [Color] -> [Color]
opacs t cs = (\(Color r g b o) -> Color r g b (o*t)) <$> cs

opac :: Double -> Color -> Color
opac t (Color r g b o) = Color r g b (o*t)

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
    (Color 0.333 0.333 0.333 0.2)
    0.02
    colorAxis2
    0.05
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
  -- , _chartAspect :: XY
  , _chartCanvasColor :: Color
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
    -- one
    (Color 1 1 1 0.02)


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

data RectConfig = RectConfig
  { _rectBorderWidth :: Double
  , _rectBorderColor :: Color
  , _rectColor :: Color
  }

instance Default RectConfig where
  def = RectConfig 1 (Color 0.333 0.333 0.333 0.4) (Color 0.365 0.647 0.855 0.5)

makeLenses ''RectConfig

data ArrowConfig a = ArrowConfig
  { _arrowMinHeadSize :: a
  , _arrowMaxHeadSize :: a
  , _arrowHeadSize :: a
  , _arrowMinStaffLength :: a
  , _arrowMaxStaffLength :: a
  , _arrowStaffLength :: a
  , _arrowMinStaffWidth :: a
  , _arrowMaxStaffWidth :: a
  , _arrowStaffWidth :: a
  , _arrowColor :: Color
  }

instance Default (ArrowConfig Double) where
  def = ArrowConfig 0.01 0.05 0.03 0.1 0.1 0.1 0.01 0.005 0.2 (Color 0.333 0.333 0.888 0.8)

makeLenses ''ArrowConfig


