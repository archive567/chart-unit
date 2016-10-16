{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart.Unit where

import Chart.Types
import qualified Control.Foldl as L
import Control.Lens hiding (beside, none, (#), at)
import qualified Data.Text as Text
import Diagrams.Prelude hiding (unit, D, Color, scale, zero)
import qualified Diagrams.Prelude as Diagrams
import Tower.Prelude hiding (min,max,from,to)
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
import qualified Diagrams.TwoD.Text
import Linear hiding (zero, identity)
import Data.List
import Formatting

newtype Range a = Range { unRange :: (a,a) } deriving (Show, Eq)

instance (Ord a) => Semigroup (Range a) where
    (Range (l,u)) <> (Range (l',u')) =
        Range (if l < l' then l else l',if u > u' then u else u')

half :: (Field a) => a
half = one/(one+one)

unitRange :: (Field a) => Range a
unitRange = Range (negate half,half)

-- range of a foldable
range :: (Foldable f, Ord a, Field a) => f a -> Range a
range xs = L.fold (L.Fold step initial extract) xs
  where
    step Nothing x = Just (Range (x, x))
    step (Just (Range (l, u))) x = Just $ Range (l', u')
      where
        l'= if l < x then l else x
        u' = if u > x then u else x
    initial = Nothing
    extract = fromMaybe unitRange

-- R2 range
rangeR2 :: (Traversable f, R2 r, Field a, Ord a) => f (r a) -> V2 (Range a)
rangeR2 qs = V2 xs ys
  where
    xs = range $ (view _x) <$> qs
    ys = range $ (view _y) <$> qs

-- 2D range of multiple data
rangeR2s :: (Traversable g, Traversable f, R2 r, Field a, Ord a) => g (f (r a)) -> V2 (Range a)
rangeR2s qss = foldl1 (\(V2 x y) (V2 x' y') -> V2 (x<>x') (y<>y')) $ rangeR2 <$> qss

-- scale by a range
scale :: (Field a, Eq a) => Range a -> a -> a
scale (Range (l, u)) a = if
    | l==u -> a
    | otherwise -> (\x -> (x-l)/(u-l) - (one/(one+one))) a

scaleR2 :: (Functor f, Field a, Eq a, R2 r) => V2 (Range a) -> f (r a) -> f (r a)
scaleR2 (V2 rx ry) qs = (\q -> over _x (scale rx) $ over _y (scale ry) $ q) <$> qs

scaleR2s :: (Functor g, Functor f, Field a, Eq a, R2 r) => V2 (Range a) -> g (f (r a)) -> g (f (r a))
scaleR2s r qss = scaleR2 r <$> qss

-- it's seems a flaw in SVG that a line isn't a series of connected rectangles
blob ∷ (Floating (N a), Ord (N a), Typeable (N a), HasStyle a, V a ~ V2) ⇒
    Color → a → a
blob c = fcA (color c) # lcA (withOpacity black 0) # lw none

-- axis rendering
axis :: AxisConfig -> Range Double -> Chart b
axis cfg r = pad (cfg ^. axisPad) $ strut' $ centerXY $
  atPoints
    (t <$> tickLocations)
    ((\x -> mkLabel x cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) (-0.5,0.5)
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
      X -> \x -> p2 (x, zero)
      Y -> \y -> p2 ((-(cfg ^. axisMarkSize)), y)
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound n -> scale r <$> mkTicksRound r n
      TickExact n -> scale r <$> mkTicksExact r n
      TickLabels ls -> scale r <$> (\x -> x - 0.5) <$> fromIntegral <$> [1..length ls]
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> tickFormat <$> mkTicksRound r n
      TickExact n -> tickFormat <$> mkTicksExact r n
      TickLabels ls -> ls
    tickFormat d = sformat (prec 2) d
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

mkTicksRound :: (Ord a, Fractional a, ExpRing a, QuotientField a Int, Field a) => Range a -> Int -> [a]
mkTicksRound (Range (l, u)) n = (first' +) . (step *) . fromIntegral <$> [0..n']
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

mkTicksExact :: (Field a) => Range a -> Int -> [a]
mkTicksExact (Range (l, u)) n = (l +) . (step *) . fromIntegral <$> [0..n]
  where
    step = (u - l)/fromIntegral n

mkLabel :: ((Renderable (Diagrams.TwoD.Text.Text Double) a), Renderable (Path V2 Double) a) => Text -> AxisConfig -> QDiagram a V2 Double Any
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

chartWith :: (Traversable f, Traversable g, R2 r) => 
    ChartConfig
    -> (g (f (r Double)) -> Chart b)
    -> V2 (Range Double)
    -> (V2 (Range Double) -> g (f (r Double)) -> g (f (r Double)))
    -> g (f (r Double))
    -> Chart b
chartWith (ChartConfig p axes) renderer range' scaler ms =
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

-- render chart with same axes scale as data
chart :: (a ~ g (f (r Double)), Traversable g, Traversable f, R2 r) =>
    ChartConfig -> (a -> Chart b) -> (V2 (Range Double) -> a -> a) -> a -> Chart b
chart cc renderer scaler ms = chartWith cc renderer (rangeR2s ms) scaler ms

-- a line is just a scatter chart rendered with a line (and with a usually stable x-value series)
line1 ∷ (Traversable f, R2 r) => LineConfig → f (r Double) → Chart b
line1 (LineConfig s c) ps = case Tower.Prelude.head ps of
  Nothing -> mempty
  Just p0 -> (stroke $ trailFromVertices (toList $ (p2 . unr2) <$> view _xy <$> ps)
              `at`
             (p2 $ unr2 (view _xy p0))) # lcA (color c) # lwN s

-- multiple lines with a common range for both x and y values
line :: (Traversable f, R2 r) =>
    ChartConfig -> [LineConfig] -> [f (r Double)] -> Chart b
line cc cfgs ms = chart cc (centerXY . mconcat . (zipWith line1 cfgs)) scaleR2s ms

-- dots on the XY plane
scatter1 ∷ (Traversable f, R2 r) => ScatterConfig → f (r Double) → Chart b
scatter1 (ScatterConfig s c) ps =
  atPoints (toList $ (p2 . unr2) <$> view _xy <$> ps)
    (repeat $ circle s #
     blob c
    )

-- scatter
scatter :: (Traversable f, R2 r) => ChartConfig -> [ScatterConfig] -> [f (r Double)] -> Chart b
scatter cc cfgs ms =
  chart cc (centerXY . mconcat . (zipWith scatter1 cfgs)) scaleR2s ms


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

rangeRect :: (Traversable f, Ord a, Field a) => f (V4 a) -> V2 (Range a)
rangeRect qs = V2 rx ry
  where
    rx = range $ (toList $ (view _x) <$> qs) <> (toList $ (view _z) <$> qs)
    ry = range $ (toList $ (view _y) <$> qs) <> (toList $ (view _w) <$> qs)

rangeRects :: (Traversable g, Traversable f, Ord a, Field a) => g (f (V4 a)) -> V2 (Range a)
rangeRects qss = over _y (<> Range (zero, zero)) $ foldl1 (\(V2 x y) (V2 x' y') -> V2 (x<>x') (y<>y')) $ rangeRect <$> qss


scaleRect :: (Field a, Eq a) => V2 (Range a) -> V4 a -> V4 a
scaleRect (V2 rx ry) q =
    over _x (scale rx) $
    over _y (scale ry) $
    over _z (scale rx) $
    over _w (scale ry) $
    q

scaleRect1 :: (Traversable f, Ord a, Field a) => V2 (Range a) -> f (V4 a) -> f (V4 a)
scaleRect1 r qs = scaleRect r <$> qs

scaleRects :: (Traversable g, Traversable f, Ord a, Field a) => V2 (Range a) -> g (f (V4 a)) -> g (f (V4 a))
scaleRects r qss = scaleRect1 r <$> qss

rect' :: (Traversable f) => ChartConfig -> [RectConfig] -> [(f (V4 Double))] -> Chart b
rect' cc cfgs ms =
  chartWith cc (centerXY . mconcat . (zipWith rect1 cfgs)) (rangeRects ms) scaleRects ms

fileSvg ∷ FilePath → (Double, Double) → Chart SVG → IO ()
fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

filePng ∷ FilePath → (Double,Double) → Chart Rasterific → IO ()
filePng f s = renderRasterific f (mkSizeSpec (Just <$> r2 s))

bubble ∷ ∀ a. (RealFloat (N a), Field (N a), Traced a, V a ~ V2) ⇒ [a] → Int → [(V a (N a))]
bubble chart' n = bubble'
  where
    bubble' = ps
    ps = catMaybes $ maxRayTraceV (p2 (0,0)) <$> ((\x -> view (Diagrams.Prelude.from r2PolarIso) (1, x @@ rad)) <$> (\x -> fromIntegral x/10.0) <$> [0..n]) <*> chart'


