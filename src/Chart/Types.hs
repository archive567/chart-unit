{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chart.Types where

import Protolude
import Control.Monad.Primitive (unsafeInlineIO)
import Control.Category (id)
import Data.List (transpose)
import System.IO (FilePath)
import Diagrams.Prelude
import Diagrams.Backend.SVG
-- import Diagrams.Backend.Cairo
import Diagrams.Core.Envelope
import GHC.Base (String)
import qualified Control.Foldl as L
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed  (Vector,(!))
import qualified Data.Vector.Unboxed as VU

type ChartSvg a = QDiagram SVG V2 a Any

rgba :: (Floating a, Ord a) => (a, a, a, a) -> AlphaColour a
rgba (r,g,b,a) = withOpacity (sRGB (r/255) (g/255) (b/255)) a

data Orientation = X | Y

data Placement = AxisLeft | AxisRight | AxisTop | AxisBottom

data TickStyle = TickNone | TickLabels [String] | TickNumber Int

data AxisConfig = AxisConfig
  { _axisOrientation :: Orientation
  , _axisPlacement :: Placement
  , _axisHeight :: Double
  , _axisColor :: AlphaColour Double
  , _axisMarkSize :: Double -- mark length
  , _axisMarkColor :: AlphaColour Double
  , _axisStrutSize :: Double -- distance of label from mark
  , _axisTextSize :: Double
  , _axisTextColor :: AlphaColour Double
  , _axisTickStyle :: TickStyle
  , _axisAlignedTextRight :: Double
  , _axisAlignedTextBottom :: Double
  }

instance Default AxisConfig where
  def =
    AxisConfig 
    X
    AxisBottom
    0.02
    (rgba(94, 19, 94, 0.5))
    0.02
    (rgba (0, 102, 200, 0.5))
    0.02
    0.04
    (rgba (30,30,30,0.7))
    (TickNumber 10)
    0.5
    1

makeLenses ''AxisConfig 

data ChartConfig = ChartConfig
  { _chartPad :: Double
  , _chartColor :: AlphaColour Double
  , _chartAxes :: [AxisConfig]
  }

instance Default ChartConfig where
  def =
    ChartConfig
    1.3
    (rgba(128, 128, 128, 0.6))
    [def,
     axisAlignedTextBottom .~ 0.65 $
     axisAlignedTextRight .~ 1 $
     axisOrientation .~ Y $
     axisPlacement .~ AxisLeft $
     def]

makeLenses ''ChartConfig

data ScatterConfig = ScatterConfig
  { _scatterChart :: ChartConfig
  , _scatterSize :: Double
  }

instance Default ScatterConfig where
  def = ScatterConfig (chartColor .~ rgba(128, 128, 128, 0.1) $ def) 0.03

makeLenses ''ScatterConfig

data BarConfig = BarConfig
  { _barChart :: ChartConfig
  , _barSep :: Double
  }

instance Default BarConfig where
  def = BarConfig (chartColor .~ rgba(59, 89, 152, 0.6) $ def) 0.01

makeLenses ''BarConfig

data LineConfig = LineConfig
  { _lineChart :: ChartConfig
  , _lineSize :: Double
  }

instance Default LineConfig where
  def = LineConfig (chartColor .~ rgba(59, 89, 152, 0.6) $ def) 0.001

makeLenses ''LineConfig


data OneLine = OneLine
    { _lColor :: AlphaColour Double
    , _lSize :: Double
    }

instance Default OneLine where
  def = OneLine (rgba(59, 89, 152, 0.6)) 0.001

makeLenses ''OneLine

data LinesConfig = LinesConfig
  { _linesChart :: ChartConfig
  , _linesLines :: [OneLine]
  }

instance Default LinesConfig where
  def = LinesConfig def [ OneLine (rgba(60,90,150,1)) 0.002
                        , OneLine (rgba(0,128,255,1)) 0.002
                        , OneLine (rgba(255,128,0,1)) 0.002
                        , OneLine (rgba(128,0,255,1)) 0.002
                        , OneLine (rgba(0,255,255,1)) 0.002
                        ]

makeLenses ''LinesConfig


