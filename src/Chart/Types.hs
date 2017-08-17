{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Chart.Types
  ( vert
  , hori
  , sepVert
  , sepHori
  , color
  , GlyphConfig(..)
  , glyphSize
  , glyphColor 
  , glyphBorderColor
  , glyphBorderSize
  , glyphShape
  , hline_
  , vline_
  , Aspect(..)
  , aspect
  , asquare
  , sixbyfour
  , golden
  , widescreen
  , skinny
  , Chart(..)
  , chartRenderer
  , chartRenderRange
  , chartData
  , Orientation(..)
  , Place(..)
  , placeOutside
  , placeGap
  , AlignH(..)
  , AlignV(..)
  , alignH
  , alignV
  , TickStyle(..)
  , AxisConfig(..)
  , axisPad
  , axisOrientation
  , axisPlace
  , axisTickStyle
  , axisRect
  , axisRectHeight
  , axisLabel
  , axisMark
  , axisMarkStart
  , axisGap
  , defXAxis
  , defYAxis
  , TitleConfig(..)
  , titleText
  , titleAlign
  , titlePlace
  , titleStrut
  , ChartConfig(..)
  , chartPad
  , chartAxes
  , chartTitles
  , chartLegends
  , chartRange
  , chartAspect
  , chartCanvas
  , LineConfig(..)
  , lineSize
  , Chart.Types.lineColor
  , RectConfig(..)
  , rectBorderWidth
  , rectBorderColor
  , rectColor
  , blob
  , box
  , clear
  , bound
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
  , textAlignH
  , textAlignV
  , textColor
  , textFillRule
  , textRotation
  , LabelConfig(..)
  , labelText
  , labelOrientation
  , labelStrut
  , LegendType(..)
  , LegendConfig(..)
  , legendChartType
  , legendInnerPad
  , legendInnerSep
  , legendOuterPad
  , legendRowPad
  , legendPlace
  , legendAlign
  , legendSep
  , legendRect
  , legendText
  ) where

import NumHask.Prelude hiding (local)
import Diagrams.Prelude hiding (Color(..), aspect, (&))
import NumHask.Range
import NumHask.Rect
import NumHask.Pair
import Diagrams.Backend.SVG


-- | chart combinators
vert :: (V a ~ V2, Foldable t, Juxtaposable a, Semigroup a, Num (N a), Monoid a) => (a->a) -> t a -> a
vert f xs = foldr (\a x -> beside (r2(0,-1)) (f a) x) mempty xs

hori :: (V a ~ V2, Foldable t, Juxtaposable a, Semigroup a, Num (N a), Monoid a) => (a->a) -> t a -> a
hori f xs = foldr (\a x -> beside (r2(1,0)) (f a) x) mempty xs

sepHori :: Double -> Diagram B -> Diagram B
sepHori s x = beside (r2(0,-1)) x (strutX s)

sepVert :: Double -> Diagram B -> Diagram B
sepVert s x = beside (r2(1,0)) x (strutY s)

color :: (Floating a, Ord a) => a -> a -> a -> a -> AlphaColour a
color r g b o = withOpacity (sRGB r g b) o

data GlyphConfig = GlyphConfig
    { _glyphSize :: Double
    , _glyphColor :: AlphaColour Double
    , _glyphBorderColor :: AlphaColour Double
    , _glyphBorderSize :: Double
    , _glyphShape :: Double -> Diagram B
    }

instance Default GlyphConfig where
    def = GlyphConfig 0.03 (color 0.333 0.333 0.333 0.4) (color 0.365 0.647 0.855 0.5) 0.015 circle

makeLenses ''GlyphConfig

vline_ :: (Transformable a, TrailLike a, InSpace V2 Double a) => Double -> Double -> a
vline_ fatness x = vrule x # scaleX (1.6/0.5*fatness)

hline_ :: (Transformable a, TrailLike a, InSpace V2 Double a) => Double -> Double -> a
hline_ fatness x = hrule x # scaleY (1.6/0.5*fatness)

-- | the rendering plane
newtype Aspect = Aspect { aspectRect :: Rect Double }

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

-- | a Chart is mostly a late binding of the Diagram B Aspect that is to be projected on to and the data.
data Chart a = Chart
    { _chartRenderer :: ( ) =>
                Aspect -> Rect Double -> a -> Diagram B
    , _chartRenderRange :: Rect Double
    , _chartData :: a
    }

makeLenses ''Chart

data Orientation = X | Y

data Place = PlaceLeft | PlaceRight | PlaceTop | PlaceBottom

placeOutside :: Num n => Place -> V2 n
placeOutside pl =
    case pl of
      PlaceBottom -> r2 (0,-1)
      PlaceTop -> r2 (0,1)
      PlaceLeft -> r2 (-1,0)
      PlaceRight -> r2 (1,0)

placeGap :: (Monoid m, Semigroup m, Ord n, Floating n) =>
    Place -> QDiagram b V2 n m -> n -> QDiagram b V2 n m
placeGap pl x s = beside (placeOutside pl) (strut' pl s) x
  where
    strut' PlaceTop = strutY
    strut' PlaceBottom = strutY
    strut' PlaceLeft = strutX
    strut' PlaceRight = strutX

data AlignH = AlignLeft | AlignCenter | AlignRight

data AlignV = AlignTop | AlignMid | AlignBottom

alignH :: AlignH -> Double
alignH a =
    case a of
      AlignLeft -> 0.5
      AlignCenter -> 0
      AlignRight -> -0.5

alignV :: AlignV -> Double
alignV a =
    case a of
      AlignTop -> -0.5
      AlignMid -> 0
      AlignBottom -> 0.5

data TickStyle = TickNone | TickLabels [Text] | TickRound Int | TickExact Int | TickPlaced [(Double,Text)]

data TextConfig = TextConfig
    { _textPad :: Double
    , _textSize :: Double
    , _textAlignH :: AlignH
    , _textAlignV :: AlignV
    , _textColor :: AlphaColour Double
    , _textFillRule :: FillRule
    , _textRotation :: Double
    }

instance Default TextConfig where
    def =
        TextConfig
        1
        0.08
        AlignCenter
        AlignMid
        (withOpacity black 0.33)
        EvenOdd
        0

makeLenses ''TextConfig

data LabelConfig = LabelConfig
    { _labelText :: TextConfig
    , _labelOrientation :: Pair Double
    , _labelStrut :: Double
    }

instance Default LabelConfig where
    def =
        LabelConfig
        def
        (Pair 0 1)
        0.05

makeLenses ''LabelConfig

data RectConfig = RectConfig
    { _rectBorderWidth :: Double
    , _rectBorderColor :: AlphaColour Double
    , _rectColor :: AlphaColour Double
    }

instance Default RectConfig where
    def = RectConfig 0.015 (color 0.333 0.333 0.333 0.4) (color 0.365 0.647 0.855 0.5)

makeLenses ''RectConfig

-- | solid rect, no border
blob :: AlphaColour Double -> RectConfig
blob c = RectConfig 0 transparent c

-- | clear and transparent rect
clear :: RectConfig
clear = RectConfig 0 transparent transparent

-- | clear rect, with border
box :: AlphaColour Double -> RectConfig
box c = RectConfig 0.015 c transparent

-- | place a rect around an Diagram B
bound :: RectConfig -> Double -> Diagram B -> Diagram B
bound cfg p x =
    (boundingRect (pad p x) #
     lcA (cfg^.rectBorderColor) #
     lw (local $ cfg^.rectBorderWidth) #
     fcA (cfg^.rectColor)) <>
    (pad p $ x)

data AxisConfig = AxisConfig
    { _axisPad :: Double
    , _axisOrientation :: Orientation
    , _axisPlace :: Place
    , _axisRect :: RectConfig
    , _axisRectHeight :: Double
    , _axisMark :: GlyphConfig
    , _axisMarkStart :: Double
    , _axisGap :: Double -- distance of axis from plane
    , _axisLabel :: LabelConfig
    , _axisTickStyle :: TickStyle
    }

defXAxis :: AxisConfig
defXAxis = AxisConfig 1 X PlaceBottom
    (RectConfig 0 transparent (withOpacity black 0.1)) -- ok
    0.02 -- ok
    (GlyphConfig 0.03 transparent (withOpacity black 0.6) 0.005 (vline_ 1)) -- ok
    0 -- ok
    0.04 -- ok
    (LabelConfig
     ( TextConfig
        1
        0.06 -- ok
        AlignCenter
        AlignMid
        (withOpacity black 0.6) -- ok
        EvenOdd
        0
     )
     (Pair 0 -1) -- ok
     0.015) -- ok
    (TickRound 8) -- ok

defYAxis :: AxisConfig
defYAxis = AxisConfig 1 Y PlaceLeft
    (RectConfig 0 transparent (withOpacity black 0.1)) -- ok
    0.02 -- ok
    (GlyphConfig 0.03 transparent (withOpacity black 0.6) 0.005 (hline_ 1)) -- ok
    0 -- ok
    0.04 -- ok
    (LabelConfig
     ( TextConfig
       1
       0.06 -- ok
       AlignCenter
       AlignMid
       (withOpacity black 0.6) -- ok
       EvenOdd
       0
     )
     (Pair -1 0)
     0.015)
    (TickRound 8) -- ok

instance Default AxisConfig where
    def = defXAxis

makeLenses ''AxisConfig

data TitleConfig = TitleConfig
    { _titleText :: TextConfig
    , _titleAlign :: AlignH
    , _titlePlace :: Place
    , _titleStrut :: Double
    }

instance Default TitleConfig where
    def = TitleConfig
        (TextConfig
         1
         0.16
         AlignCenter
         AlignMid
         (withOpacity black 0.8)
         EvenOdd
         0
        )
        AlignCenter
        PlaceTop
        0.02

makeLenses ''TitleConfig

data LineConfig = LineConfig
    { _lineSize :: Double
    , _lineColor :: AlphaColour Double
    }

instance Default LineConfig where
    def = LineConfig 0.02 (color 0.365 0.647 0.855 1.00)

makeLenses ''LineConfig

data ArrowConfig a = ArrowConfig
    { _arrowMinLength :: a
    , _arrowMaxLength :: a
    , _arrowMinHeadLength :: a
    , _arrowMaxHeadLength :: a
    , _arrowMinStaffWidth :: a
    , _arrowMaxStaffWidth :: a
    , _arrowColor :: AlphaColour Double
    , _arrowHeadStyle :: ArrowHT a
    }

instance Default (ArrowConfig Double) where
    def = ArrowConfig 0.02 0.2 0.01 0.1 0.002 0.005 (color 0.333 0.333 0.888 0.8) dart

makeLenses ''ArrowConfig

data PixelConfig =
    PixelConfig
    { _pixelGradient :: Range (AlphaColour Double)
    , _pixelGrain :: Pair Int
    }

instance Default PixelConfig where
    def = PixelConfig
        (Range (color 1 1 1 1) (color 0 0 0 1))
        (Pair 20 20)

makeLenses ''PixelConfig

data LegendType =
    LegendText TextConfig |
    LegendGlyph GlyphConfig |
    LegendLine LineConfig Double |
    LegendGLine GlyphConfig LineConfig Double |
    LegendRect RectConfig Double |
    LegendArrow (ArrowConfig Double) Double |
    LegendPixel RectConfig Double

data LegendConfig =
    LegendConfig
    { _legendChartType :: [(LegendType, Text)]
    , _legendInnerPad :: Double
    , _legendInnerSep :: Double
    , _legendOuterPad :: Double
    , _legendRowPad :: Double
    , _legendPlace :: Place
    , _legendAlign :: AlignH
    , _legendSep :: Double
    , _legendRect :: RectConfig
    , _legendText :: TextConfig
    }

instance Default LegendConfig where
    def =
        LegendConfig
        []
        1.1
        0.03
        1.2
        1
        PlaceRight
        AlignRight
        0.02
        (RectConfig 0.002 (withOpacity black 0.2) transparent)
        (TextConfig
         1
         0.07
         AlignCenter
         AlignMid
         (withOpacity black 0.63)
         EvenOdd
         0)

makeLenses ''LegendConfig

data ChartConfig = ChartConfig
    { _chartPad :: Double
    , _chartAxes :: [AxisConfig]
    , _chartTitles :: [(TitleConfig, Text)]
    , _chartLegends :: [LegendConfig]
    , _chartRange :: Maybe (Rect Double)
    , _chartAspect :: Aspect
    , _chartCanvas :: RectConfig
    }

instance Default ChartConfig where
    def =
        ChartConfig
        1.3
        [defXAxis, defYAxis]
        []
        []
        Nothing
        sixbyfour
        clear

makeLenses ''ChartConfig

