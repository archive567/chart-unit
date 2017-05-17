{-# OPTIONS_GHC -Wall #-}

module Chart
  ( module X
  , SVG
  , V4(..)
  , zipWith4
  , module Diagrams.Prelude
  ) where

import Data.List (zipWith4)
import Diagrams.Backend.SVG (SVG)
import Diagrams.Prelude hiding
    ((<>), arrow, Color(..), lineColor, scaleX, scaleY, width, zero, unsnoc, uncons, Additive, Loop, Magma, Metric, getLast, getFirst, normalize, inv, First, Last, (&), Strict, unit, from, to, size, distance, (.-.), (-~), (*.), rect, element, aspect, project, intersection)
import Linear (V4(..))

import NumHask.Range as X
import NumHask.Rect as X
import NumHask.Histogram as X
import Chart.Types as X
import Chart.Unit as X
