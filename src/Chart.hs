{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart (
    range1D,
    range1Ds,
    unit,
    units,
    unitXY,
    unitsXY,
    chartXY,
    scatter,
    scatterXY,
    barRange,
    barLabelled,
    bars,
    line,
    lines,
    lineXY,
    linesXY,
    axisXY,
    -- toFile,
    mkTicks,
    mkTicks',
    module Chart.Types,
    module Control.Lens,
    module Data.Default
) where

import Chart.Types
import qualified Control.Foldl as L
import Control.Lens hiding (beside, none, (#))
import Data.Default (def)
import qualified Data.Text as Text
import Diagrams.Prelude hiding (unit) 
import Protolude hiding (min,max)
import Text.Printf
import qualified Diagrams.TwoD.Text

range1D :: (Fractional t, Ord t, Foldable f) => f t -> (t, t)
range1D = L.fold (L.Fold step initial extract)
  where
    step Nothing x = Just (x,x)
    step (Just (min,max)) x =
      Just (min' x min, max' x max)
    max' x1 x2 = if x1 > x2 then x1 else x2
    min' x1 x2 = if x1 < x2 then x1 else x2
    initial = Nothing
    extract = fromMaybe (-0.5,0.5)

range1Ds :: (Fractional t, Ord t, Foldable f, Foldable f') => f' (f t) -> (t, t)
range1Ds xss = L.fold (L.Fold step initial extract) xss
  where
    step Nothing x = Just (range1D x)
    step (Just (min, max)) x =
      Just (min', max')
      where
        (min'', max'') = range1D x
        min' = if min'' < min then min'' else min
        max' = if max'' > max then max'' else max
    initial = Nothing
    extract = fromMaybe (-0.5,0.5)

-- unit scales and translates to a [-0.5,0.5] range
unit :: (Fractional b, Functor f, Ord b, Foldable f) => f b -> f b
unit xs =
  let (minX,maxX) = range1D xs in
  (\x -> (x-minX)/(maxX-minX) - 0.5) <$> xs

-- units scales multiple xs to a common range
units :: (Fractional b, Functor f, Ord b, Foldable f, Functor f', Foldable f') =>
    f' (f b) -> f' (f b)
units xss =
  let (minX,maxX) = range1Ds xss in
  (fmap (\x -> (x-minX)/(maxX-minX) - 0.5)) <$> xss

-- scale 2d points (XY) to ((-0.5,-0.5), (0.5,0.5))
unitXY :: (Fractional b, Fractional a, Ord b, Ord a) => [(a, b)] -> [(a, b)]
unitXY xys = zip (unit $ fst <$> xys) (unit $ snd <$> xys)

-- scale multiple 2d series
unitsXY :: (Fractional a, Ord a, Fractional b, Ord b) => [[(a, b)]] -> [[(a, b)]]
unitsXY xyss = zipWith (\x y -> zip x y) xs' ys'
  where
    xs = fmap fst <$> xyss
    ys = fmap snd <$> xyss
    xs' = units xs
    ys' = units ys

chartXY :: (Renderable (Path V2 Double) a, (Renderable (Diagrams.TwoD.Text.Text Double) a)) => ChartConfig
  -> ([(Double, Double)] -> QDiagram a V2 Double Any)
  -> [(Double, Double)]
  -> QDiagram a V2 Double Any
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

    d X = range1D $ fst <$> xys
    d Y = range1D $ snd <$> xys

-- axis rendering
axisXY :: ((Renderable (Diagrams.TwoD.Text.Text Double) a), Renderable (Path V2 Double) a) => AxisConfig -> (Double,Double) -> QDiagram a V2 Double Any
axisXY cfg range = centerXY $
  atPoints
    (p2 . t <$> tickLocations)
    ((\x -> mkLabel x cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) (-0.5,0.5)
   # unitRect (cfg ^. axisColor))
  where
    t = case cfg ^. axisOrientation of
      X -> \x -> (x,0)
      Y -> \y -> (-(cfg ^. axisMarkSize), y)
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      TickNumber n -> unit $ mkTicks range n
      TickLabels ls -> unit $ fromIntegral <$> [1..length ls]
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickNumber n -> Text.pack . printf "%7.1g" <$> mkTicks range n
      TickLabels ls -> ls
    axisRect h (min, max) = case cfg ^. axisOrientation of
      X -> moveTo (p2 (max,0)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleX (max-min) .
          scaleY h $
          unitSquare
      Y -> moveTo (p2 (0,min)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleY (max-min) .
          scaleX h $
          unitSquare

-- somewhat a base class of a wide variety of chart marks
unitRect ∷ (Floating (N a), Ord (N a), Typeable (N a), HasStyle a, V a ~ V2) ⇒
    AlphaColour Double → a → a
unitRect c = fcA c # lcA (withOpacity black 0) # lw none

-- polymorphic dots
scatter :: (Typeable (N r), Monoid r, Semigroup r, Transformable r,HasStyle r, HasOrigin r, TrailLike r, V r ~ V2, N r ~ Double) =>
    ScatterConfig -> [(N r, N r)] -> r
scatter cfg xys =
  atPoints (p2 <$> unitXY xys)
    (repeat $ circle (cfg ^. scatterSize) #
     unitRect (cfg ^. scatterChart ^. chartColor)
    )

scatterXY :: (Renderable (Path V2 Double) a, (Renderable (Diagrams.TwoD.Text.Text Double) a)) => ScatterConfig -> [(Double,Double)] -> QDiagram a V2 Double Any
scatterXY cfg xys =
  chartXY (cfg ^. scatterChart) (scatter cfg) xys

-- bar
bars :: (Renderable (Path V2 Double) a) => BarConfig -> [Double] -> QDiagram a V2 Double Any
bars cfg ys =
  cat' (r2 (1,0)) (with Diagrams.Prelude.& sep .~ cfg ^. barSep)
  ((\y ->
    unitSquare
    # moveOriginTo (p2 (-0.5,-0.5))
    # if y==0 then scaleY epsilon else scaleY y) <$> ys)
    # unitRect (cfg ^. barChart ^. chartColor)
    # centerXY
    # scaleX (1/fromIntegral (length ys)) # scaleY (1/(max-min))
  where
    (min,max) = range1D ys
    epsilon = 1e-8

barRange :: (Renderable (Path V2 Double) a, (Renderable (Diagrams.TwoD.Text.Text Double) a)) => BarConfig -> [(Double, Double)] -> QDiagram a V2 Double Any
barRange cfg xys = chartXY (cfg ^. barChart) (\x -> bars cfg (snd <$> x)) xys

barLabelled :: (Renderable (Path V2 Double) a, (Renderable (Diagrams.TwoD.Text.Text Double) a)) => BarConfig -> [Double] -> [Text] -> QDiagram a V2 Double Any
barLabelled cfg ys labels = barRange
     ( barChart . chartAxes .~
       [ axisTickStyle .~
         TickLabels labels $ def
       ]
       $ cfg
     ) (zip [0..] ys)

-- a line is just a scatter chart rendered with a line (and with a usually stable x-value series)
line :: (Renderable (Path V2 Double) a) => [(Double,Double)] -> QDiagram a V2 Double Any
line xys = strokeT $ trailFromVertices $ p2 <$> unitXY xys

lineXY :: (Renderable (Path V2 Double) a, (Renderable (Diagrams.TwoD.Text.Text Double) a)) => LineConfig -> [(Double,Double)] -> QDiagram a V2 Double Any
lineXY cfg xys =
    chartXY (cfg ^. lineChart)
    (\x -> line x # centerXY # lcA (cfg ^. lineChart ^. chartColor) # lwN (cfg ^. lineSize))
    xys

-- multiple lines with a common range for both x and y values
lines :: (Renderable (Path V2 Double) a) => LinesConfig -> [[(Double,Double)]] -> QDiagram a V2 Double Any
lines cfg xyss = centerXY $ mconcat $
    zipWith (\d c -> d # lcA (c ^. lColor) # lwN (c ^. lSize))
    (l xyss)
    (cycle $ cfg ^.linesLines)
  where
    l xyss' = strokeT . trailFromVertices . fmap p2 <$> unitsXY xyss'

linesXY :: ((Renderable (Diagrams.TwoD.Text.Text Double) a), Renderable (Path V2 Double) a) =>
  LinesConfig
  -> [[(Double, Double)]]
  -> QDiagram a V2 Double Any
linesXY cfg@(LinesConfig (ChartConfig p _ axes) _) xyss =
  L.fold (L.Fold step (lines cfg xyss) (pad p)) axes
  where
    step x a =
      beside (v (a ^. axisPlacement))
      x
      (axisXY a (d (a ^. axisOrientation)))

    v AxisBottom = r2 (0,-1)
    v AxisTop = r2 (0,1)
    v AxisLeft = r2 (-1,0)
    v AxisRight = r2 (1,0)

    d X = range1Ds $ fmap fst <$> xyss
    d Y = range1Ds $ fmap snd <$> xyss


mkTicks :: (Double,Double) -> Int -> [Double]
mkTicks r n = (f +) . (s *) . fromIntegral <$> [0..n']
  where
    (f,s,n') = mkTicks' r n

mkTicks' :: (Double,Double) -> Int -> (Double, Double, Int)
mkTicks' (min, max) n = (f, step', n')
  where
    span' = max - min
    step' = 10 ^^ floor (logBase 10 (span'/fromIntegral n))
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    f = step * fromIntegral (floor (min/step))
    l = step * fromIntegral (floor (max/step))
    n' = round ((l - f)/step)

mkLabel :: ((Renderable (Diagrams.TwoD.Text.Text Double) a), Renderable (Path V2 Double) a) => Text -> AxisConfig -> QDiagram a V2 Double Any
mkLabel label cfg =
  beside dir
  (beside dir
   (rule (cfg ^. axisMarkSize) #
   lcA (cfg ^. axisMarkColor))
    s)
  (Diagrams.Prelude.alignedText
    (cfg ^. axisAlignedTextRight)
    (cfg ^. axisAlignedTextBottom)
    (Text.unpack label) #
  scale (cfg ^. axisTextSize) #
  fcA (cfg ^.axisTextColor))
  where
    dir = case cfg ^. axisOrientation of
      X -> r2 (0,-1)
      Y -> r2 (-1,0)
    rule = case cfg ^. axisOrientation of
      X -> vrule
      Y -> hrule
    s = case cfg ^. axisOrientation of
      X -> strutY (cfg ^. axisStrutSize)
      Y -> strutX (cfg ^. axisStrutSize)

