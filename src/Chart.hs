{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | native haskell charts
--
-- > {-# NoImplicitPrelude #-}
-- > {-# OverloadedString #-}
-- > import NumHask.Prelude
-- > import Chart
--
-- chart-unit is designed to be used in conjunction with the numhask prelude.
--

module Chart
  ( module Chart.Core
  , module Chart.Arrow
  , module Chart.Glyph
  , module Chart.Hud
  , module Chart.Line
  , module Chart.Rect
  , module Chart.Text
  , module Chart.Lenses
  , module NumHask.Pair
  , module NumHask.Space
  , module NumHask.Range
  , module NumHask.Rect
  , module Data.Colour
  , module Data.Colour.Palette.Harmony
  , module Data.Colour.Palette.ColorSet
  , Diagram
  , triangle
  , square
  , circle
  , SVG
  , B
  , Renderable
  , Path
  , V2
  , Default(..)
  , Text
  , scratch
  ) where

import Chart.Arrow
import Chart.Core
import Chart.Glyph
import Chart.Hud
import Chart.Lenses
import Chart.Line
import Chart.Rect
import Chart.Text
import Data.Colour
import Data.Colour.Palette.ColorSet
import Data.Colour.Palette.Harmony
import Data.Default (Default(..))
import Data.Text
import Diagrams.Backend.SVG (SVG, B)
import Diagrams.Prelude
import NumHask.Pair
import NumHask.Range
import NumHask.Rect
import NumHask.Space hiding (width)
import NumHask.Prelude (IO)

-- | a scratch pad
scratch :: Diagram SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600, 400)
