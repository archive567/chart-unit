{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart.Unit where

import Chart.Types
import qualified Control.Foldl as L
import Control.Lens hiding (beside, none, (#), at)
import qualified Data.Text as Text
import Diagrams.Prelude hiding (unit, D, Color) 
import Protolude hiding (min,max,from,to)
import Text.Printf
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Backend.Rasterific (Rasterific, renderRasterific)
import qualified Diagrams.TwoD.Text

rangeD :: [Double] -> Range
rangeD xs = L.fold (L.Fold step initial extract) xs
  where
    step Nothing x = Just (Range x x)
    step (Just (Range l u)) x = Just $ Range l' u'
      where
        l'= if l < x then l else x
        u' = if u > x then u else x
    initial = Nothing
    extract = fromMaybe (Range -0.5 0.5)

range :: Q2 -> RangeXY
range ss = RangeXY xs ys
  where
    xs = rangeD $ mconcat $ fmap (view _x) <$> ss
    ys = rangeD $ mconcat $ fmap (view _y) <$> ss

-- scale according to supplied range
scaleD :: Range -> [Double] -> [Double]
scaleD (Range l u) xs =
  (\x -> (x-l)/(u-l) - 0.5) <$> xs

-- unit scales and translates to a [-0.5,0.5] range
unitD :: [Double] -> [Double]
unitD xs =
  f <$> xs
  where
    (Range l u) = rangeD xs
    f = if
        | l == u -> const 0.0
        | otherwise -> (\x -> (x-l)/(u-l) - 0.5)

unit :: Q2 -> Q2
unit ms = s <$> ms
  where
    (RangeXY rx ry) = range ms
    s ps = zipWith V2 (scaleD rx $ (view _x) <$> ps) (scaleD ry $ (view _y) <$> ps)

-- axis rendering
axisXY :: AxisConfig -> Range -> Chart a
axisXY cfg r = pad (cfg ^. axisPad) $ strut' $ centerXY $
  atPoints
    (p2 . t <$> tickLocations)
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

-- chart rendering
chart ::
    ChartConfig
    -> (Q2 -> Chart a)
    -> Q2
    -> Chart a
chart (ChartConfig p axes) renderer ms =
  L.fold (L.Fold step (renderer (unit ms)) (pad p)) axes
  where
    step x cfg = beside dir x (axisXY cfg r)
      where
        r = case view axisOrientation cfg of
              X -> rx
              Y -> ry
        dir = case view axisPlacement cfg of
          AxisBottom -> r2 (0,-1)
          AxisTop -> r2 (0,1)
          AxisLeft -> r2 (-1,0)
          AxisRight -> r2 (1,0)
    (RangeXY rx ry) = range ms


-- a line is just a scatter chart rendered with a line (and with a usually stable x-value series)
line1 ∷ LineConfig → Q1 → Chart a
line1 _ [] = mempty
line1 (LineConfig s c) ps@(p0:_) = (stroke $ trailFromVertices ((p2 . unr2) <$> ps) `at` (p2 $ unr2 p0)) # lcA (color c) # lwN s

-- multiple lines with a common range for both x and y values
line :: ChartConfig -> [LineConfig] -> Q2 -> Chart a
line cc cfgs ms = chart cc (centerXY . mconcat . (zipWith line1 cfgs)) ms

-- it's seems a flaw in SVG that a line isn't a series of connected rectangles
blob ∷ ∀ a. (Floating (N a), Ord (N a), Typeable (N a), HasStyle a, V a ~ V2) ⇒ Color → a → a
blob c = fcA (color c) # lcA (withOpacity black 0) # lw none

-- dots on the XY plane
scatter1 ∷ ScatterConfig → Q1 → Chart a
scatter1 _ [] = mempty
scatter1 (ScatterConfig s c) ps = showOrigin $
  atPoints ((p2 . unr2) <$> ps)
    (repeat $ circle s #
     blob c
    )

-- scatter
scatter :: ChartConfig -> [ScatterConfig] -> Q2 -> Chart a
scatter cc cfgs ms =
  chart cc (centerXY . mconcat . (zipWith scatter1 cfgs)) ms

-- bar
barD (BarConfig s c) ys =
  cat' (r2 (1,0)) (with Diagrams.Prelude.& sep .~ s)
  ((\y ->
    unitSquare
    # moveOriginTo (p2 (-0.5,-0.5))
    # if y==0 then scaleY epsilon else scaleY y)
    <$> ys
  )
  # scaleX (1/fromIntegral (length ys))
  # scaleY (1/(u - l))
  # blob c
  where
    (Range l u) = rangeD ys
    epsilon = 1e-8

barD' _ [] = mempty
barD' (BarConfig s c) ys =
    blob c $
    scaleY (1/(u - l)) $
    scaleX (1/fromIntegral (length ys)) $ 
    cat' (r2 (1,0)) (with Diagrams.Prelude.& sep .~ s)
    ((\y ->
        unitSquare
       # moveOriginTo (p2 (-0.5,-0.5))
       # if y==0 then scaleY epsilon else scaleY y) <$> ys)
  where
    (Range l u) = rangeD ys
    vscale' = if
        | l > 0 -> if l==u then 0 else 1/u
        | u < 0 -> if l==u then 0 else 1/l
        | otherwise -> if l==u then 0 else 1/(u-l)
    epsilon = 1e-8

{-
blob ((Color 0.5 0.5 0.3 0.8)) $ scaleY (1/(2853.0)) $ scaleX (1/fromIntegral (length xysHist)) $ cat' (r2 (1,0)) (with Diagrams.Prelude.& sep .~ 0.01) $ (\y -> unitSquare # moveOriginTo (p2 (-0.5,-0.5)) # scaleY y) <$> (snd <$> xysHist)
-}

bar :: ChartConfig -> [BarConfig] -> Q2 -> Chart a
bar cc cfgs ms =
    chart
    cc
    (centerXY . mconcat .
      (zipWith
        (\c ps -> barD c ((view _y) <$> ps))
        cfgs))
    ms

bar' :: ChartConfig -> [BarConfig] -> Q2 -> Chart a
bar' cc cfgs ms =
    chart
    cc
    (centerXY . mconcat .
      (zipWith
        (\c ps -> barD' c ((view _y) <$> ps))
        cfgs))
    ms


{-
-- bar
bars :: BarConfig -> [Double] -> QDiagram SVG V2 Double Any
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

barRange :: BarConfig -> [(Double, Double)] -> QDiagram SVG V2 Double Any
barRange cfg xys = chartXY (cfg ^. barChart) (\x -> bars cfg (snd <$> x)) xys

-}

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
bubble chart' n = bubble'
  where
    bubble' = ps
    ps = catMaybes $ maxRayTraceV (p2 (0,0)) <$> ((\x -> view (Diagrams.Prelude.from r2PolarIso) (1, x @@ rad)) <$> (\x -> fromIntegral x/10.0) <$> [0..n]) <*> chart'
