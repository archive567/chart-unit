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
import Diagrams.Core.Envelope
import Formatting
import GHC.Base (String)
import qualified Control.Foldl as L
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed  (Vector,(!))
import qualified Data.Vector.Unboxed as VU



rgba (r,g,b,a) = withOpacity (sRGB (r/255) (g/255) (b/255)) a

data Orientation = X | Y

data TickStyle = TickNone | TickLabels [String] | TickNumber Int

data AxisConfig = AxisConfig
  { _axisOrientation :: Orientation
  , _axisHeight :: Double
  , _axisColor :: AlphaColour Double
  , _axisVruleSize :: Double
  , _axisVruleColor :: AlphaColour Double
  , _axisStrutSize :: Double
  , _axisTextSize :: Double
  , _axisTextColor :: AlphaColour Double
  , _axisTickStyle :: TickStyle
  }
  
instance Default AxisConfig where
  def =
    AxisConfig
    X
    0.02
    (rgba(94, 19, 94, 0.5))
    0.02
    (rgba (40, 102, 200, 1))
    0.02
    0.04
    (rgba (30,30,30,1))
    (TickNumber 10)

makeLenses ''AxisConfig 

data ScatterConfig = ScatterConfig
  { _scatterPad :: Double
  , _scatterSize :: Double
  , _scatterColor :: AlphaColour Double
  }

instance Default ScatterConfig where
  def =
    ScatterConfig
    1.3
    0.03
    (rgba(128, 128, 128, 0.1))
    
makeLenses ''ScatterConfig

data BarConfig = BarConfig
  { _barPad :: Double
  , _barSep :: Double
  , _barColor :: AlphaColour Double
  }

instance Default BarConfig where
  def =
    BarConfig
    1.2
    0.1
    (rgba(59, 89, 152, 0.6
          
         ))
    
makeLenses ''BarConfig
