{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart (
    range1D,
    unit,
    unitXY,
    chartXY,
    scatter,
    scatterXY,
    barRange,
    barLabelled,
    bars,
    line,
    lineXY,
    axisXY,
    toFile,
    mkTicks,
    tickList,
    module Chart.Types,
    module Control.Lens,
    module Data.Default
) where

import Protolude
import GHC.Base (String)
import System.IO (FilePath)
import Diagrams.Prelude hiding (unit)
import Diagrams.Backend.SVG
import Formatting
import qualified Control.Foldl as L
import Control.Lens hiding (beside, none, (#))
import Chart.Types
import Data.Default (def)

range1D :: (Fractional t, Ord t, Foldable f) => f t -> (t, t)
range1D = L.fold (L.Fold step initial extract)
  where
    step Nothing x = Just (x,x)
    step (Just (min,max)) x =
      Just (min' x min, max' x max)
    max' x1 x2 = if x1 >= x2 then x1 else x2
    min' x1 x2 = if x1 <= x2 then x1 else x2
    initial = Nothing
    extract = fromMaybe (-0.5,0.5)

{-
unit scales and translates to a [-0.5,0.5] extent
-}

unit :: (Fractional b, Functor f, Ord b, Foldable f) => f b -> f b
unit xs =
  let (minX,maxX) = range1D xs in
  (\x -> (x-minX)/(maxX-minX) - 0.5) <$> xs

unitXY :: (Fractional b, Fractional a, Ord b, Ord a) => [(a, b)] -> [(a, b)]
unitXY xys = zip (unit $ fst <$> xys) (unit $ snd <$> xys)

rect c = fcA c # lcA (withOpacity black 0) # lw none

chartXY ::
  ChartConfig
  -> ([(Double, Double)] -> QDiagram SVG V2 Double Any)
  -> [(Double, Double)]
  -> QDiagram SVG V2 Double Any
chartXY (ChartConfig p _ axes) chart xys =
  L.fold (L.Fold step (chart xys) (pad p)) axes
  where
    step x a =
      beside (v (a ^. axisPlacement))
      x
      (axisXY a (d (a ^. axisOrientation)))

    v AxisBottom = r2 (0,-1)
    v AxisTop = r2 (0,1)
    v AxisLeft = r2 (-1,0)
    v AxisRight = r2 (1,0)

    d X = fst <$> xys
    d Y = snd <$> xys

-- polymorphic dots
scatter :: (Typeable (N r), Monoid r, Semigroup r, Transformable r,HasStyle r, HasOrigin r, TrailLike r, V r ~ V2, N r ~ Double) => ScatterConfig -> [(N r, N r)] -> r
scatter cfg xys =
  atPoints (p2 <$> unitXY xys)
    (repeat $ circle (cfg ^. scatterSize) #
     Chart.rect (cfg ^. scatterChart ^. chartColor)
    )

scatterXY :: ScatterConfig -> [(Double,Double)] -> QDiagram SVG V2 Double Any
scatterXY cfg xys =
  chartXY (cfg ^. scatterChart) (scatter cfg) xys

-- bar
barRange :: BarConfig -> [(Double, Double)] -> QDiagram SVG V2 Double Any
barRange cfg xys = chartXY (cfg ^. barChart) (\xys -> bars cfg (snd <$> xys)) xys

barLabelled :: Real a => BarConfig -> [Double] -> [String] -> QDiagram SVG V2 Double Any
barLabelled cfg ys labels = barRange
     ( barChart . chartAxes .~
       [ axisTickStyle .~
         TickLabels labels $ def
       ]
       $ cfg
     ) (zip [0..] ys)

bars :: BarConfig -> [Double] -> QDiagram SVG V2 Double Any
bars cfg ys =
  cat' (r2 (1,0)) (with Diagrams.Prelude.& sep .~ cfg ^. barSep)
  ((\y ->
    unitSquare
    # moveOriginTo (p2 (-0.5,-0.5))
    # if y==0 then scaleY epsilon else scaleY y) <$> ys)
    # Chart.rect (cfg ^. barChart ^. chartColor)
    # centerXY
    # scaleX (1/fromIntegral (length ys)) # scaleY (1/(max-min))
  where
    (min,max) = range1D ys
    epsilon = 1e-8

-- a line is just a scatter chart rendered with a line (and with a usually stable x-value series)
line xys = strokeT $ trailFromVertices $ p2 <$> unitXY xys

lineXY cfg xys = chartXY (cfg ^. lineChart) (\xys -> line xys # centerXY # lcA (cfg ^. lineChart ^. chartColor) # lwN (cfg ^. lineSize)) xys

-- axis rendering
axisXY :: AxisConfig -> [Double] -> QDiagram SVG V2 Double Any
axisXY cfg xs = centerXY $
  atPoints
    (p2 . trans <$> tickLocations)
    ((\x -> mkLabel x cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) (range1D $ unit xs)
   # Chart.rect (cfg ^. axisColor))
  where
    trans = case cfg ^. axisOrientation of
      X -> \x -> (x,0)
      Y -> \y -> (-(cfg ^. axisMarkSize), y)
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      TickNumber n -> unit $ tickList (range1D xs) n
      TickLabels ls -> unit $ fromIntegral <$> [1..length ls]
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickNumber n -> formatToString (prec 2) <$> tickList (range1D xs) n
      TickLabels ls -> ls
    axisRect height (min, max) = case cfg ^. axisOrientation of
      X -> moveTo (p2 (max,0)) . strokeTrail . closeTrail . fromVertices . scaleX (max-min) . scaleY height $ unitSquare
      Y -> moveTo (p2 (0,min)) . strokeTrail . closeTrail . fromVertices . scaleY (max-min) . scaleX height $ unitSquare

tickList :: (Double,Double) -> Int -> [Double]
tickList (min,max) n = (first +) . (step *) . fromIntegral <$> [0..n']
  where
    span' = max - min
    step' = 10 ^^ floor (logBase 10 (span'/fromIntegral n))
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    first = step * fromIntegral (floor (min/step) + 1)
    last = fromIntegral (floor (max/step)) * step
    n' = round ((last - first)/step)

mkTickList :: Bool -> [Double] -> Int -> [Double]
mkTickList outerTicks xs n = (first +) . (step *) . fromIntegral <$> [0..n']
  where
    (first,step,n') = mkTicks outerTicks xs n

mkTicks :: Bool -> [Double] -> Int -> (Double,Double,Int)
mkTicks outerTicks xs n = (first,step,n')
  where
    (min, max) = range1D xs
    span' = max - min
    step' = 10 ^^ floor (logBase 10 (span'/fromIntegral n))
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    first = step * fromIntegral (floor (min/step) + if outerTicks then 0 else 1)
    last = step * fromIntegral (floor (max/step) + if outerTicks then 1 else 0)
    n' = round ((last - first)/step)


mkLabel :: String -> AxisConfig -> QDiagram SVG V2 Double Any
mkLabel label cfg =
  beside dir
  (beside dir
   (rule (cfg ^. axisMarkSize) #
   lcA (cfg ^. axisMarkColor))
    gap)
  (Diagrams.Prelude.alignedText
    (cfg ^. axisAlignedTextRight)
    (cfg ^. axisAlignedTextBottom)
    label #
  scale (cfg ^. axisTextSize) #
  fcA (cfg ^.axisTextColor))
  where
    dir = case cfg ^. axisOrientation of
      X -> r2 (0,-1)
      Y -> r2 (-1,0)
    rule = case cfg ^. axisOrientation of
      X -> vrule
      Y -> hrule
    gap = case cfg ^. axisOrientation of
      X -> strutY (cfg ^. axisStrutSize)
      Y -> strutX (cfg ^. axisStrutSize)

-- helpers
hex :: (Floating b, Ord b) =>  String -> Colour b
hex s = sRGB r b g
  where
    (RGB r g b) = toSRGB $ sRGB24read s

toFile :: FilePath -> (Double, Double) -> QDiagram SVG V2 Double Any -> IO ()
toFile name size = renderSVG name (mkSizeSpec (Just <$> r2 size))
