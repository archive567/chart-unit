{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart.Unit where

import Chart.Types
import Chart.Range
import Tower.Prelude hiding (min,max,from,to, (&))

import qualified Control.Foldl as L
import Control.Lens hiding (beside, none, (#), at)
import Data.List hiding (head)
import Data.Ord (max, min)
import qualified Data.Text as Text
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
import Diagrams.Backend.SVG (SVG, renderSVG)
import qualified Diagrams.Prelude as Diagrams
import Diagrams.Prelude hiding (unit, D, Color, scale, zero)
import Formatting
import Linear hiding (zero, identity, unit)

-- a solid blob (shape) with a colour fill and no border
blob ∷ (Floating (N a), Ord (N a), Typeable (N a), HasStyle a, V a ~ V2) ⇒
    Color → a → a
blob c = fcA (color c) # lcA (withOpacity black 0) # lw none

-- axis rendering
axis :: (ExpRing Double) =>
    AxisConfig -> Extrema Double -> Chart' b
axis cfg r = pad (cfg ^. axisPad) $ strut' $ centerXY $
  atPoints
    (t <$> tickLocations)
    ((`mkLabel` cfg) <$> tickLabels)
  `atop` 
  (axisRect (cfg ^. axisHeight) r
   # blob (cfg ^. axisColor))
  where
    strut' x = beside dir x $ strut'' (cfg ^. axisInsideStrut)
    dir = case cfg ^. axisPlacement of
      AxisBottom -> r2 (0,1)
      AxisTop -> r2 (0,-1)
      AxisLeft -> r2 (1,0)
      AxisRight -> r2 (-1,0)
    strut'' = case cfg ^. axisOrientation of
      X -> strutX
      Y -> strutY
    t = case cfg ^. axisOrientation of
      X -> \x -> p2 (x, 0)
      Y -> \y -> p2 (-(cfg ^. axisMarkSize), y)
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound n -> rescale r <$> mkTicksRound r n
      TickExact n -> rescale r <$> mkTicksExact r n
      TickLabels ls -> rescale r . (\x -> x - 0.5) . fromIntegral <$> [1..length ls]
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> tickFormat <$> mkTicksRound r n
      TickExact n -> tickFormat <$> mkTicksExact r n
      TickLabels ls -> ls
    tickFormat = sformat (prec 2)
    axisRect h (Extrema (_,_)) = case cfg ^. axisOrientation of
      X -> moveTo (p2 (0.5,0)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleY h $
          unitSquare
      Y -> moveTo (p2 (0,-0.5)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleX h $
          unitSquare


-- axis rendering
axis' ::
    AxisConfig -> Extrema Double -> Chart' b
axis' cfg r = pad (cfg ^. axisPad) $ strut' $ centerXY $
  atPoints
    (t <$> tickLocations)
    ((`mkLabel` cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) r
   # blob (cfg ^. axisColor))
  where
    strut' x = beside dir x $ strut'' (cfg ^. axisInsideStrut)
    dir = case cfg ^. axisPlacement of
      AxisBottom -> r2 (0,1)
      AxisTop -> r2 (0,-1)
      AxisLeft -> r2 (1,0)
      AxisRight -> r2 (-1,0)
    strut'' = case cfg ^. axisOrientation of
      X -> strutX
      Y -> strutY
    t = case cfg ^. axisOrientation of
      X -> \x -> p2 (x, 0)
      Y -> \y -> p2 (-(cfg ^. axisMarkSize), y)
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound n -> mkTicksRound r n
      TickExact n -> mkTicksExact r n
      TickLabels ls -> (\x -> x - 0.5) . fromIntegral <$> [1..length ls]
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> tickFormat <$> mkTicksRound r n
      TickExact n -> tickFormat <$> mkTicksExact r n
      TickLabels ls -> ls
    tickFormat = sformat (prec 2)
    axisRect h (Extrema (l,u)) = case cfg ^. axisOrientation of
      X -> moveTo (p2 (u,0)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleY h .
          scaleX (u-l) $
          unitSquare
      Y -> moveTo (p2 (0,l)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleX h .
          scaleY (u-l) $
          unitSquare


-- axis rendering
unitAxis ::
    AxisConfig -> Extrema Double -> Chart' b
unitAxis cfg r = pad (cfg ^. axisPad) $ strut' $ centerXY $
  atPoints
    (t <$> tickLocations)
    ((`mkLabel` cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) r
   # blob (cfg ^. axisColor))
  where
    strut' x = beside dir x $ strut'' (cfg ^. axisInsideStrut)
    dir = case cfg ^. axisPlacement of
      AxisBottom -> r2 (0,1)
      AxisTop -> r2 (0,-1)
      AxisLeft -> r2 (1,0)
      AxisRight -> r2 (-1,0)
    strut'' = case cfg ^. axisOrientation of
      X -> strutX
      Y -> strutY
    t = case cfg ^. axisOrientation of
      X -> \x -> p2 (x, 0)
      Y -> \y -> p2 (-(cfg ^. axisMarkSize), y)
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound n -> rescale r <$> mkTicksRound r n
      TickExact n -> rescale r <$> mkTicksExact r n
      TickLabels ls -> rescale r . (\x -> x - 0.5) . fromIntegral <$> [1..length ls]
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> tickFormat <$> mkTicksRound r n
      TickExact n -> tickFormat <$> mkTicksExact r n
      TickLabels ls -> ls
    tickFormat = sformat (prec 2)
    axisRect h (Extrema (_,_)) = case cfg ^. axisOrientation of
      X -> moveTo (p2 (0.5,0)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleY (-h) $
          unitSquare
      Y -> moveTo (p2 (0,-0.5)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleX h $
          unitSquare


-- axis rendering
unitHud ::
    AxisConfig -> Extrema Double -> Chart' b
unitHud cfg r = pad (cfg ^. axisPad) $ strut' $ centerXY $
  atPoints
    (t <$> tickLocations)
    ((`mkLabel` cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) r
   # blob (cfg ^. axisColor))
  where
    strut' x = beside dir x $ strut'' (cfg ^. axisInsideStrut)
    dir = case cfg ^. axisPlacement of
      AxisBottom -> r2 (0,1)
      AxisTop -> r2 (0,-1)
      AxisLeft -> r2 (1,0)
      AxisRight -> r2 (-1,0)
    strut'' = case cfg ^. axisOrientation of
      X -> strutX
      Y -> strutY
    t = case cfg ^. axisOrientation of
      X -> \x -> p2 (x, 0)
      Y -> \y -> p2 (-(cfg ^. axisMarkSize), y)
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound n -> rescale r <$> mkTicksRound r n
      TickExact n -> rescale r <$> mkTicksExact r n
      TickLabels ls -> rescale r . (\x -> x - 0.5) . fromIntegral <$> [1..length ls]
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> tickFormat <$> mkTicksRound r n
      TickExact n -> tickFormat <$> mkTicksExact r n
      TickLabels ls -> ls
    tickFormat = sformat (prec 2)
    axisRect h (Extrema (_,_)) = case cfg ^. axisOrientation of
      X -> moveTo (p2 (0.5,0)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleY (-h) $
          unitSquare
      Y -> moveTo (p2 (0,-0.5)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleX h $
          unitSquare

mkTicksRound :: (ExpRing a, MultiplicativeGroup a, Floating a, RealFrac a) => Extrema a -> Int -> [a]
mkTicksRound (Extrema (l, u)) n = (first' +) . (step *) . fromIntegral <$> [0..n']
  where
    span' = u - l
    step' = 10 ^^ (floor (logBase 10 (span'/fromIntegral n))::Int)
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    first' = step * fromIntegral (ceiling (l/step) :: Int)
    last' = step * fromIntegral (floor (u/step) :: Int)
    n' = round ((last' - first')/step) :: Int

mkTicksExact :: (MultiplicativeGroup a, AdditiveGroup a, Fractional a) => Extrema a -> Int -> [a]
mkTicksExact (Extrema (l, u)) n = (l +) . (step *) . fromIntegral <$> [0..n]
  where
    step = (u - l)/fromIntegral n

mkLabel ::
    Text ->
    AxisConfig ->
    Chart' b
mkLabel label cfg =
  beside dir
  (beside dir
   (rule (cfg ^. axisMarkSize) #
   lcA (color $ cfg ^. axisMarkColor))
    s)
  (Diagrams.Prelude.alignedText
    (cfg ^. axisAlignedTextRight)
    (cfg ^. axisAlignedTextBottom)
    (Text.unpack label) #
  Diagrams.scale (cfg ^. axisTextSize) #
  fcA (color $ cfg ^.axisTextColor))
  where
    dir = case cfg ^. axisOrientation of
      X -> r2 (0,-1)
      Y -> r2 (-1,0)
    rule = case cfg ^. axisOrientation of
      X -> vrule
      Y -> hrule
    s = case cfg ^. axisOrientation of
      X -> strutY (cfg ^. axisLabelStrut)
      Y -> strutX (cfg ^. axisLabelStrut)

chartWith ::
    ChartConfig
    -> (g (f (r Double)) -> Chart' b)
    -> V2 (Extrema Double)
    -> (V2 (Extrema Double) -> g (f (r Double)) -> g (f (r Double)))
    -> g (f (r Double))
    -> Chart' b
chartWith (ChartConfig p axes _) renderer range' scaler ms =
  L.fold (L.Fold step (renderer (scaler range' ms)) (pad p)) axes
  where
    step x cfg = beside dir x (axis cfg r)
      where
        r = case view axisOrientation cfg of
              X -> rx
              Y -> ry
        dir = case view axisPlacement cfg of
          AxisBottom -> r2 (0,-1)
          AxisTop -> r2 (0,1)
          AxisLeft -> r2 (-1,0)
          AxisRight -> r2 (1,0)
    (V2 rx ry) = range'

chartWith'' ::
    (R2 r) =>
    ChartConfig ->
    (g (f (r Double)) -> Chart' b) ->
    r (Extrema Double) ->
    (r (Extrema Double) -> g (f (r Double)) -> g (f (r Double))) ->
    g (f (r Double)) ->
    Chart' b
chartWith'' (ChartConfig p axes _) renderer range' scaler ms =
  L.fold (L.Fold step (renderer (scaler range' ms)) (pad p)) axes
  where
    step x cfg = beside dir x (axis cfg r)
      where
        r = case view axisOrientation cfg of
              X -> view _x range'
              Y -> view _y range'
        dir = case view axisPlacement cfg of
          AxisBottom -> r2 (0,-1)
          AxisTop -> r2 (0,1)
          AxisLeft -> r2 (-1,0)
          AxisRight -> r2 (1,0)

chartHud ::
    ChartConfig
    -> (g (f (r Double)) -> Chart' b)
    -> V2 (Extrema Double)
    -> (V2 (Extrema Double) -> g (f (r Double)) -> g (f (r Double)))
    -> g (f (r Double))
    -> Chart' b
chartHud (ChartConfig p axes cc) renderer range'@(V2 (Extrema (_,_)) (Extrema (_,_))) scaler ms =
  L.fold (L.Fold step begin (pad p)) axes
  where
    begin = (unitSquare # fcA (color cc) # lw 0) <> renderer (scaler range' ms)
    step x cfg = beside dir x (axis cfg r)
      where
        r = case view axisOrientation cfg of
              X -> rx
              Y -> ry
        dir = case view axisPlacement cfg of
          AxisBottom -> r2 (0,-1)
          AxisTop -> r2 (0,1)
          AxisLeft -> r2 (-1,0)
          AxisRight -> r2 (1,0)
    (V2 rx ry) = range'

chartHudTrunc ::
    ChartConfig
    -> ([[r Double]] -> Chart' b)
    -> V2 (Extrema Double)
    -> (V2 (Extrema Double) -> [[r Double]] -> [[r Double]])
    -> [[r Double]]
    -> Chart' b
chartHudTrunc (ChartConfig p axes cc) renderer range'@(V2 (Extrema (lx, ux)) (Extrema (ly, uy))) scaler ms =
  L.fold (L.Fold step begin (pad p)) axes
  where
    begin = clipTo clip' $ (unitSquare # fcA (color cc) # lw 0) <> renderer (scaler range' ms)
    step x cfg = beside dir x (axis cfg r)
      where
        r = case view axisOrientation cfg of
              X -> rx
              Y -> ry
        dir = case view axisPlacement cfg of
          AxisBottom -> r2 (0,-1)
          AxisTop -> r2 (0,1)
          AxisLeft -> r2 (-1,0)
          AxisRight -> r2 (1,0)
    (V2 rx ry) = range'
    clip' = pathFromTrail $ closeTrail $ fromVertices (p2 <$> [(lx,ly),(lx,uy),(ux,uy),(ux,ly)])



-- render chart with same axes scale as data
chart ::
    ( a ~ g (f (r Double))
    , Traversable g
    , Traversable f
    , R2 r) =>
    ChartConfig -> (a -> Chart' b) -> (V2 (Extrema Double) -> a -> a) -> a -> Chart' b
chart cc renderer scaler ms = chartWith cc renderer (rangeR2s ms) scaler ms

-- a line is just a scatter chart rendered with a line
-- (and with a usually stable x-value series)
line1 ∷ (Traversable f, R2 r) => LineConfig → f (r Double) → Chart b
line1 (LineConfig s c) ps = case head ps of
  Nothing -> mempty
  Just p0 -> stroke (trailFromVertices (toList $ (p2 . unr2) . view _xy <$> ps)
              `at`
              p2 (unr2 (view _xy p0))) # lcA (color c) # lwN s

-- multiple lines with a common range for both x and y values
unitLine ::
    ( Traversable f
    , R2 r) =>
    ChartConfig -> [LineConfig] -> [f (r Double)] -> Chart' b
unitLine cc cfgs = chart cc (centerXY . mconcat . zipWith line1 cfgs) rescaleR2s

-- dots on the XY plane
scatter1 ∷ (Traversable f, R2 r) => ScatterConfig → f (r Double) → Chart b
scatter1 (ScatterConfig s c) ps =
  atPoints (toList $ (p2 . unr2) . view _xy <$> ps)
    (repeat $ circle s #
     blob c
    )

-- scatter
unitScatter ::
    ( Traversable f
    , R2 r) =>
    ChartConfig -> [ScatterConfig] -> [f (r Double)] -> Chart' b
unitScatter cc cfgs =
  chart cc (centerXY . mconcat . zipWith scatter1 cfgs) rescaleR2s

rect1 :: (Traversable f) => RectConfig -> f (V4 Double) -> Chart b
rect1 cfg qs = mconcat $ toList $
    (\(V4 x y z w) ->
       (unitSquare #
        moveTo (p2 (0.5,0.5)) #
        scaleX (z - x) #
        scaleY (w - y) #
        moveTo (p2 (x,y)) #
        fcA (color $ cfg ^. rectColor) #
        lcA (color $ cfg ^. rectBorderColor) #
        lw 1
       )) <$> qs

rangeRect :: (BoundedField a, Traversable f, Ord a) => f (V4 a) -> V2 (Extrema a)
rangeRect qs = V2 rx ry
  where
    rx = Chart.Range.range $ toList (view _x <$> qs) <> toList (view _z <$> qs)
    ry = Chart.Range.range $ toList (view _y <$> qs) <> toList (view _w <$> qs)

rangeRects ::
    ( Traversable g
    , Traversable f) =>
    g (f (V4 Double)) -> V2 (Extrema Double)
rangeRects qss =
    over _y (+ zero) $
    foldl1 (\(V2 x y) (V2 x' y') -> V2 (x+x') (y+y')) $ rangeRect <$> qss

rescaleRect :: (Field a) => V2 (Extrema a) -> V4 a -> V4 a
rescaleRect (V2 rx ry) q =
    over _x (rescale rx) $
    over _y (rescale ry) $
    over _z (rescale rx) $
    over _w (rescale ry)
    q

rescaleRect1 :: (Field a, Traversable f) => V2 (Extrema a) -> f (V4 a) -> f (V4 a)
rescaleRect1 r qs = rescaleRect r <$> qs

rescaleRects :: (Field a, Traversable g, Traversable f) =>
    V2 (Extrema a) -> g (f (V4 a)) -> g (f (V4 a))
rescaleRects r qss = rescaleRect1 r <$> qss

unitRect ::
    ( Traversable f) =>
    ChartConfig -> [RectConfig] -> [f (V4 Double)] -> Chart' b
unitRect cc cfgs qss =
  chartWith
  cc
  (centerXY . mconcat . zipWith rect1 cfgs)
  (rangeRects qss)
  rescaleRects
  qss

arrowLength :: ArrowConfig Double -> Double
arrowLength cfg =
    (0.667 * view arrowHeadSize cfg) +
    min
    (view arrowStaffLength cfg + view arrowMinStaffLength cfg)
    (view arrowMaxStaffLength cfg)

arrow1 :: (Traversable f) => ArrowConfig Double -> f (V4 Double) -> Chart b
arrow1 cfg qs =
    fcA (color $ cfg ^. arrowColor) $ position $
    zip
    (p2 . (\(V4 x y _ _) -> (x,y)) <$> toList qs)
    (arrowStyle cfg <$> toList qs)

arrowStyle :: ArrowConfig Double -> V4 Double -> Chart b
arrowStyle cfg (V4 x y z w) =
    arrowAt' opts (p2 (x, y)) (sL *^ V2 z w)
  where
    trunc minx maxx a = min (max minx a) maxx
    m = norm (V2 z w)
    hs = trunc (cfg ^. arrowMinHeadSize) (cfg ^. arrowMaxHeadSize) (cfg ^. arrowHeadSize * m)
    sW = trunc (cfg ^. arrowMinStaffWidth) (cfg ^. arrowMaxStaffWidth) (cfg ^. arrowStaffWidth * m)
    sL = trunc (cfg ^. arrowMinStaffLength) (cfg ^. arrowMaxStaffLength) (cfg ^. arrowStaffLength * m)
    opts = with & arrowHead .~ tri &
           headLength .~ global hs &
           shaftStyle %~ (lwG sW & lcA (color $ cfg ^. arrowColor)) &
           headStyle %~ (lcA (color $ cfg ^. arrowColor) & fcA (color $ cfg ^. arrowColor))

unitArrow ::
    ( Traversable f) =>
    ChartConfig -> [ArrowConfig Double] -> [f (V4 Double)] -> Chart' b
unitArrow cc cfgs qss =
  chartWith
  cc
  (centerXY . mconcat . zipWith arrow1 cfgs)
  (rangeArrows cfgs qss)
  (rescaleArrows cfgs)
  qss

rangeArrow :: (Traversable f) => ArrowConfig Double -> f (V4 Double) -> V2 (Extrema Double)
rangeArrow cfg qs = V2 rx ry
  where
    rx = Chart.Range.range $
        toList (view _x <$> qs) <>
        toList ((\q -> view _x q + arrowLength cfg * view _z q) <$> qs)
    ry = Chart.Range.range $
        toList (view _y <$> qs) <>
        toList ((\q -> view _y q + arrowLength cfg * view _w q) <$> qs)

rangeV4 :: (Traversable f) => f (V4 Double) -> V4 (Extrema Double)
rangeV4 qs = V4 rx ry rz rw
  where
    rx = Chart.Range.range $ toList (view _x <$> qs)
    ry = Chart.Range.range $ toList (view _y <$> qs)
    rz = Chart.Range.range $ toList (view _z <$> qs)
    rw = Chart.Range.range $ toList (view _w <$> qs)

rangeV4s :: (Traversable g, Traversable f) =>
    g (f (V4 Double)) -> V4 (Extrema Double)
rangeV4s qss = foldl1 (\(V4 x y z w) (V4 x' y' z' w') -> V4 (x+x') (y+y') (z+z') (w+w')) $
    rangeV4 <$> toList qss

rangeArrows :: (Traversable g, Traversable f) =>
    [ArrowConfig Double] -> g (f (V4 Double)) -> V2 (Extrema Double)
rangeArrows cfgs qss = foldl1 (\(V2 x y) (V2 x' y') -> V2 (x+x') (y+y')) $
    zipWith rangeArrow cfgs (toList qss)

rescaleArrow :: ArrowConfig Double-> V2 (Extrema Double) -> V4 Double -> V4 Double
rescaleArrow _ (V2 rx ry) q =
    over _x (rescale rx) $
    over _y (rescale ry) $
    -- over _z (rescale (fmap (* arrowLength cfg) rx)) $
    -- over _w (rescale (fmap (* arrowLength cfg) ry))
    over _z (rescale rx) $
    over _w (rescale ry)
    q

rescaleV4 :: V4 (Extrema Double) -> V4 Double -> V4 Double
rescaleV4 (V4 rx ry rz rw) q =
    over _x (rescale rx) $
    over _y (rescale ry) $
    over _z (rescale rz) $
    over _w (rescale rw)
    q

rescaleV4s :: (Traversable g, Traversable f) => V4 (Extrema Double) -> g (f (V4 Double)) -> g (f (V4 Double))
rescaleV4s r qss = fmap (rescaleV4 r) <$> qss

rescaleArrow1 :: (Traversable f) =>
    ArrowConfig Double-> V2 (Extrema Double) -> f (V4 Double) -> f (V4 Double)
rescaleArrow1 cfg r qs = rescaleArrow cfg r <$> qs

rescaleArrows :: (Traversable g, Traversable f) =>
    [ArrowConfig Double] -> V2 (Extrema Double) -> g (f (V4 Double)) -> [f (V4 Double)]
rescaleArrows cfgs r qss = zipWith (\cfg qs -> rescaleArrow1 cfg r qs) cfgs (toList qss)

fileSvg ∷ FilePath → (Double, Double) → Chart SVG → IO ()
fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

filePng ∷ FilePath → (Double,Double) → Chart Rasterific → IO ()
filePng f s = renderRasterific f (mkSizeSpec (Just <$> r2 s))

-- outline of a chart
bubble ∷ ∀ a. (MultiplicativeGroup (N a), RealFloat (N a), Traced a, V a ~ V2) ⇒ [a] → Int → [V a (N a)]
bubble chart' n = bubble'
  where
    bubble' = ps
    ps = catMaybes $ maxRayTraceV (p2 (0,0)) <$>
        ((\x -> view (Diagrams.Prelude.from r2PolarIso) (1, x @@ rad)) .
         (\x -> fromIntegral x/10.0) <$> [0..n]) <*>
        chart'
