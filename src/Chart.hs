{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | The Chart module exports all of the chart-unit functionality, and most of what you need from outside libraries.
--
-- Chart is designed to be used in conjunction with both the numhask and diagrams preludes. Diagrams.Prelude conatins much of the lens library and many re-exports that clash with NumHask, so best to import qualified.
--
-- > {-# NoImplicitPrelude #-}
-- > {-# DuplicateRecordFields #-}
-- > import NumHask.Prelude
-- > import qualified Diagrams.Prelude as D
-- > import Chart
--
module Chart
  ( -- * chart-unit
    module Chart.Core
  , module Chart.Data
  , module Chart.Arrow
  , module Chart.Glyph
  , module Chart.Hud
  , module Chart.Line
  , module Chart.Rect
  , module Chart.Text
  , module Chart.Bar

    -- * numhask-range
  , module NumHask.Pair
  , module NumHask.Space
  , module NumHask.Range
  , module NumHask.Rect

    -- * color
  , module Data.Colour
  , module Data.Colour.Palette.Harmony
  , module Data.Colour.Palette.ColorSet

    -- * fonts
  , module Graphics.SVGFonts

    -- * Default
  , Default(..)

    -- * Text
  , Text

    -- * scratch pad
  , scratch
  ) where

import Chart.Arrow
import Chart.Bar
import Chart.Core
import Chart.Data
import Chart.Glyph
import Chart.Hud
import Chart.Line
import Chart.Rect
import Chart.Text
import Graphics.SVGFonts hiding (textFont)
import Data.Colour
import Data.Colour.Palette.ColorSet
import Data.Colour.Palette.Harmony
import Data.Default (Default(..))
import Data.Text
import Diagrams.Backend.SVG (SVG)
import Diagrams.Prelude
import NumHask.Pair
import NumHask.Range
import NumHask.Rect
import NumHask.Space hiding (width)
import NumHask.Prelude (IO)

-- | a scratch pad
scratch :: Diagram SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600, 400)
