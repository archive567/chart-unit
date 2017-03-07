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
    ((<>), arrow, Color(..), lineColor, scaleX, scaleY, width, zero, unsnoc, uncons, Additive, Loop, Magma, Metric, getLast, getFirst, normalize, inv, First, Last, (&), Strict, unit, from, to, size, distance, (.-.), (-~), (*.))
import Linear (V4(..))

import Chart.Types as X
import Chart.Range as X
import Chart.Unit as X
-- import Tower.Prelude as X hiding (local, rotate, shift, trace, clamp, conjugate)
