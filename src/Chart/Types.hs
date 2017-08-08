{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Chart.Types
  ( Chart
  , Chart'
  , Aspect(..)
  , aspect
  , asquare
  , sixbyfour
  , golden
  , widescreen
  , skinny
  , QChart(..)
  , qXY
  , qChart
  , qData
  , Orientation(..)
  , Placement(..)
  , TickStyle(..)
  , Color(..)
  , color
  , uncolor
  , opac
  , opacs
  , palette
  , AxisConfig(..)
  , axisPad
  , axisOrientation
  , axisPlacement
  , axisHeight
  , axisColor
  , axisMarkSize
  , axisMarkColor
  , axisInsideStrut
  , axisLabelStrut
  , axisLabelText
  , axisTickStyle
  , ChartConfig(..)
  , chartPad
  , chartAxes
  , chartRange
  , chartAspect
  , chartCanvasColor
  , LineConfig(..)
  , lineSize
  , Chart.Types.lineColor
  , ScatterConfig(..)
  , scatterSize
  , scatterColor
  , RectConfig(..)
  , rectBorderWidth
  , rectBorderColor
  , rectColor
  , ArrowConfig(..)
  , arrowMaxLength
  , arrowMinLength
  , arrowMinHeadLength
  , arrowMaxHeadLength
  , arrowMinStaffWidth
  , arrowMaxStaffWidth
  , arrowColor
  , arrowHeadStyle
  , PixelConfig(..)
  , pixelGradient
  , pixelGrain
  , TextConfig(..)
  , textPad
  , textSize
  , textColor
  , textRight
  , textBottom
  , textRotation
  ) where

import NumHask.Prelude
import Diagrams.Prelude hiding (Color(..), aspect)
import qualified Diagrams.TwoD.Text
import NumHask.Range
import NumHask.Rect
import NumHask.Pair
import Data.Colour

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

-- | the rendering plane
newtype Aspect = Aspect { unAspect :: Rect Double }

-- | the rendering aspect of a chart expressed as a ratio of x-plane : y-plane.
aspect :: Double -> Aspect
aspect a = Aspect $ Ranges ((a*) <$> one) one

asquare :: Aspect
asquare = aspect 1

sixbyfour :: Aspect
sixbyfour = aspect 1.5

golden :: Aspect
golden = aspect 1.61803398875

widescreen :: Aspect
widescreen = aspect 3

skinny :: Aspect
skinny = aspect 5

-- | The concrete nature of a QDiagram, and a desire to scale data and hud items naturally, a QChart is mostly a late binding of the Aspect that the chart is to be projected on to and the data.
data QChart a b = QChart
    { _qChart :: ( ( Renderable (Diagrams.TwoD.Text.Text Double) a)
                  , Renderable (Path V2 Double) a) =>
                Aspect -> Rect Double -> b -> QDiagram a V2 Double Any
    , _qXY :: Rect Double
    , _qData :: b
    }

makeLenses ''QChart

data Orientation = X | Y

data Placement = AxisLeft | AxisRight | AxisTop | AxisBottom

data TickStyle = TickNone | TickLabels [Text] | TickRound Int | TickExact Int | TickPlaced [(Double,Text)]

data Color =
    Color
    { _red :: Double
    , _green :: Double
    , _blue :: Double
    , _aaa :: Double
    } deriving (Eq, Show)

color ∷ Color → AlphaColour Double
color (Color r g b a)= withOpacity (sRGB r g b) a

uncolor ∷ AlphaColour Double → Color
uncolor c = Color r g b a
  where
    a = alphaChannel c
    (RGB r g b) = toSRGB (Data.Colour.over c black)

palette ∷ [Color]
palette =
    [ Color 0.333 0.333 0.333 1.00 -- grey
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

data TextConfig = TextConfig
    { _textPad :: Double
    , _textSize :: Double
    , _textColor :: Color
    , _textRight :: Double
    , _textBottom :: Double
    , _textRotation :: Double
    }

instance Default TextConfig where
    def =
        TextConfig
        1
        0.08
        (Color 0.333 0.333 0.333 0.8)
        0.5
        1
        0

makeLenses ''TextConfig

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
    , _axisLabelText :: TextConfig
    , _axisTickStyle :: TickStyle
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
        (Color 0.1 0.4 0.8 0.5)
        0.05
        0.02
        (textSize .~ 0.06 $ def)
        (TickRound 8)

makeLenses ''AxisConfig

data ChartConfig = ChartConfig
    { _chartPad :: Double
    , _chartAxes :: [AxisConfig]
    , _chartRange :: Maybe (Rect Double)
    , _chartAspect :: Aspect
    , _chartCanvasColor :: Color
    }

instance Default ChartConfig where
    def =
        ChartConfig
        1.3
        [def,
         axisLabelText .~
          ( textSize .~ 0.06 $
            textBottom .~ 0.65 $
            textRight .~ 1 $
            textRotation .~ 0 $
            def) $
         axisOrientation .~ Y $
         axisPlacement .~ AxisLeft $
         def]
        Nothing
        sixbyfour
        (Color 1 1 1 0.02)

makeLenses ''ChartConfig

data LineConfig = LineConfig
    { _lineSize :: Double
    , _lineColor :: Color
    }

instance Default LineConfig where
    def = LineConfig 0.02 (Color 0.365 0.647 0.855 1.00)

makeLenses ''LineConfig

data ScatterConfig = ScatterConfig
    { _scatterSize :: Double
    , _scatterColor :: Color
    }

instance Default ScatterConfig where
    def = ScatterConfig 0.03 (Color 0.33 0.33 0.33 0.2)

makeLenses ''ScatterConfig

data RectConfig = RectConfig
    { _rectBorderWidth :: Double
    , _rectBorderColor :: Color
    , _rectColor :: Color
    }

instance Default RectConfig where
    def = RectConfig 1 (Color 0.333 0.333 0.333 0.4) (Color 0.365 0.647 0.855 0.5)

makeLenses ''RectConfig

data ArrowConfig a = ArrowConfig
    { _arrowMinLength :: a
    , _arrowMaxLength :: a
    , _arrowMinHeadLength :: a
    , _arrowMaxHeadLength :: a
    , _arrowMinStaffWidth :: a
    , _arrowMaxStaffWidth :: a
    , _arrowColor :: Color
    , _arrowHeadStyle :: ArrowHT a
    }

instance Default (ArrowConfig Double) where
    def = ArrowConfig 0.02 0.2 0.01 0.1 0.002 0.005 (Color 0.333 0.333 0.888 0.8) dart

makeLenses ''ArrowConfig

data PixelConfig =
    PixelConfig
    { _pixelGradient :: Range Color
    , _pixelGrain :: Pair Int
    }

instance Default PixelConfig where
    def = PixelConfig
        (Range (Color 1 1 1 1) (Color 0 0 0 1))
        (Pair 20 20)

makeLenses ''PixelConfig

