{-# OPTIONS_GHC -Wall #-}

module Chart
  ( module X
  , SVG
  , V4(..)
  , zipWith4
  , Pair(..)
  , pattern Pair
  ) where

import Data.List (zipWith4)
import Diagrams.Backend.SVG (SVG)
import Linear (V4(..))

import NumHask.Pair

import NumHask.Space as X
import NumHask.Range as X
import NumHask.Rect as X
import Chart.Types as X
import Chart.Unit as X
