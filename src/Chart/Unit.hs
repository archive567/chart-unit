{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart.Unit where

import Chart.Types
import qualified Control.Foldl as L
import Control.Lens hiding (beside, none, (#), at)
import qualified Data.Text as Text
import Diagrams.Prelude hiding (unit, D, Color) 
import Protolude hiding (min,max)
import Text.Printf
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
import qualified Diagrams.TwoD.Text

rangeD :: D -> Range
rangeD xs = L.fold (L.Fold step initial extract) xs
  where
    step Nothing x = Just (Range x x)
    step (Just (Range l u)) x = Just $ Range l' u'
      where
        l'= if l < x then l else x
        u' = if u > x then u else x
    initial = Nothing
    extract = fromMaybe (Range -0.5 0.5)

rangeS :: S -> RangeXY
rangeS ps = RangeXY xs ys
  where
    xs = rangeD $ fst <$> ps
    ys = rangeD $ snd <$> ps

rangeM :: M -> RangeXY
rangeM ss = RangeXY xs ys
  where
    xs = rangeD $ mconcat $ fmap fst <$> ss
    ys = rangeD $ mconcat $ fmap snd <$> ss


-- scale according to supplied range
scaleD :: Range -> D -> D
scaleD (Range l u) xs =
  (\x -> (x-l)/(u-l) - 0.5) <$> xs

-- unit scales and translates to a [-0.5,0.5] range
unitD :: D -> D
unitD xs =
  f <$> xs
  where
    (Range l u) = rangeD xs
    f = if
        | l == u -> const 0.0
        | otherwise -> (\x -> (x-l)/(u-l) - 0.5)

-- scale 2d points (XY) to ((-0.5,-0.5), (0.5,0.5))
unitS :: S -> S
unitS ps = zip (unitD $ fst <$> ps) (unitD $ snd <$> ps)

unitS' :: RangeXY -> S -> S
unitS' (RangeXY rx ry) ps = zip (scaleD rx $ fst <$> ps) (scaleD ry $ snd <$> ps)

unitM :: M -> M
unitM ms = unitS' (rangeM ms) <$> ms

chartS ::
    ChartConfig
    -> (S -> Chart a)
    -> S
    -> Chart a
chartS (ChartConfig p axes) chart vs =
  L.fold (L.Fold step (chart (unitS vs)) (pad p)) axes
  where
    step x a =
      beside (v (a ^. axisPlacement))
      x
      (axisXY a (case (a ^. axisOrientation) of
                   X -> rx
                   Y -> ry))

    v AxisBottom = r2 (0,-1)
    v AxisTop = r2 (0,1)
    v AxisLeft = r2 (-1,0)
    v AxisRight = r2 (1,0)

    (RangeXY rx ry) = rangeS vs

chartM ::
    ChartConfig
    -> (M -> Chart a)
    -> M
    -> Chart a
chartM (ChartConfig p axes) chart ms =
  L.fold (L.Fold step (chart (unitM ms)) (pad p)) axes
  where
    step x a =
      beside (v (a ^. axisPlacement))
      x
      (axisXY a (case (a ^. axisOrientation) of
                   X -> rx
                   Y -> ry))

    v AxisBottom = r2 (0,-1)
    v AxisTop = r2 (0,1)
    v AxisLeft = r2 (-1,0)
    v AxisRight = r2 (1,0)

    (RangeXY rx ry) = rangeM ms

-- axis rendering
axisXY :: AxisConfig -> Range -> Chart a
axisXY cfg r = pad (cfg ^. axisPad) $ strut' $ centerXY $
  atPoints
    (p2 . t <$> tickLocations)
    ((\x -> mkLabel x cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) (-0.5,0.5)
   # unitRect (cfg ^. axisColor))
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
      X -> \x -> (x,0)
      Y -> \y -> (-(cfg ^. axisMarkSize), y)
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> unitD $ mkTicksRound r n
      TickExact n -> unitD $ mkTicksExact r n
      TickLabels ls -> unitD $ fromIntegral <$> [1..length ls]
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> Text.pack . printf "%7.1g" <$> mkTicksRound r n
      TickExact n -> Text.pack . printf "%7.1g" <$> mkTicksExact r n
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


-- a line is just a scatter chart rendered with a line (and with a usually stable x-value series)
line ∷ LineConfig → S → Chart a
line _ [] = mempty
line (LineConfig s c) ps@(p0:_) = (stroke $ trailFromVertices (p2 <$> ps) `at` (p2 p0)) # lcA (color c) # lwN s

-- multiple lines with a common range for both x and y values
lineM :: ChartConfig -> [LineConfig] -> M -> Chart a
lineM cc cfgs ms = chartM cc (centerXY . mconcat . (zipWith line cfgs)) ms

-- somewhat a base class of a wide variety of chart marks
-- it's seems a flaw in SVG that a line isn't a series of connected rectangles
unitRect ∷ ∀ a. (Floating (N a), Ord (N a), Typeable (N a), HasStyle a, V a ~ V2) ⇒ Color → a → a
unitRect c = fcA (color c) # lcA (withOpacity black 0) # lw none

-- polymorphic dots
scatter ∷ ScatterConfig → S → Chart a
scatter _ [] = mempty
scatter (ScatterConfig s c) ps = showOrigin $
  atPoints (p2 <$> unitS ps)
    (repeat $ circle s #
     unitRect c
    )

scatterM :: ChartConfig -> [ScatterConfig] -> M -> Chart a
scatterM cc cfgs ms =
  chartM cc (centerXY . mconcat . (zipWith scatter cfgs)) ms

-- bar
bar :: BarConfig -> D -> Chart a
bar (BarConfig s c) ys =
  cat' (r2 (1,0)) (with Diagrams.Prelude.& sep .~ s)
  ((\y ->
    unitSquare
    # moveOriginTo (p2 (-0.5,-0.5))
    # if y==0 then scaleY epsilon else scaleY y) <$> ys)
    # unitRect c
    # centerXY
    # scaleX (1/fromIntegral (length ys)) # scaleY (1/(u - l))
  where
    (Range l u) = rangeD ys
    epsilon = 1e-8

barD :: ChartConfig -> BarConfig -> D -> Chart a
barD cc cfg ys = chartS cc (bar cfg . (snd <$>)) (zip (fromIntegral <$> [0..]) ys)

barDLabelled :: ChartConfig -> BarConfig -> D -> [Text] -> Chart a
barDLabelled cc cfg ys labels = chartS
     ( chartAxes .~
       [ axisTickStyle .~
         TickLabels labels $ def
       ]
       $ cc
     )
     (bar cfg . (fmap fst))
     (zip (fromIntegral <$> [0..]) ys)

mkTicksRound :: Range -> Int -> [Double]
mkTicksRound (Range l u) n = (first' +) . (step *) . fromIntegral <$> [0..n']
  where
    span' = u - l
    step' = 10 ^^ floor (logBase 10 (span'/fromIntegral n))
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    first' = step * fromIntegral (floor (l/step))
    last = step * fromIntegral (floor (u/step))
    n' = round ((last - first')/step)

mkTicksExact :: Range -> Int -> [Double]
mkTicksExact (Range l u) n = (l +) . (step *) . fromIntegral <$> [0..n]
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
  scale (cfg ^. axisTextSize) #
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

fileSvg ∷ FilePath → (Double, Double) → Chart SVG → IO ()
fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

filePng ∷ FilePath → (Double,Double) → Chart Rasterific → IO ()
filePng f s = renderRasterific f (mkSizeSpec (Just <$> r2 s))

bubble ∷ ∀ a. (RealFloat (N a), Traced a, V a ~ V2) ⇒ [a] → Int → [(V a (N a))]
bubble chart n = bubble'
  where
    bubble' = ps
    ps = catMaybes $ maxRayTraceV (p2 (0,0)) <$> ((\x -> view (Diagrams.Prelude.from r2PolarIso) (1, x @@ rad)) <$> (\x -> fromIntegral x/10.0) <$> [0..n]) <*> chart
