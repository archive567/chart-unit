{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart.Unit
  ( scaleX
  , scaleY
  , blob
  , line1
  , scatter1
  , rect1
  , pixel1
  , arrow1
  , box
  , scatterChart
  , scatterChartWithRange
  , lineChart
  , lineChartWithRange
  , histChart
  , histChartWithRange
  , arrowChart
  , arrowChartWithRange
  , rangeV4
  , rangeV42Rect
  , scaleV4s
  , toPixels
  , rescalePixels
  , pixelf
  , withChart
  , axes
  , combine
  , fileSvg
  , bubble
  -- , text1
  , histCompare
  ) where

import NumHask.Prelude hiding (min,max,from,to,(&))

import NumHask.Range
import NumHask.Rect
import NumHask.Histogram
import Chart.Types

import Control.Lens hiding (beside, none, (#), at)
import Data.Ord (max, min)
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude hiding (width, unit, D, Color, scale, zero, scaleX, scaleY, aspect, rect, project)
import Formatting
import Linear hiding (zero, identity, unit, project)

import qualified Control.Foldl as L
import qualified Data.Text as Text
import qualified Diagrams.Prelude as Diagrams

-- | avoiding the scaleX zero throw
eps :: N [Point V2 Double]
eps = 1e-8

scaleX :: Double -> [Point V2 Double] -> [Point V2 Double]
scaleX s = Diagrams.scaleX (if s==zero then eps else s)

scaleY :: Double -> [Point V2 Double] -> [Point V2 Double]
scaleY s = Diagrams.scaleY (if s==zero then eps else s)

-- * chartlets are recipes for constructing QDiagrams from traversable containers of vectors and a configuration
-- a solid blob (shape) with a colour fill and no border
blob ∷ (Floating (N a), Ord (N a), Typeable (N a), HasStyle a, V a ~ V2) ⇒
    Chart.Types.Color → a → a
blob c = fcA (color c) # lcA (withOpacity black 0) # lw none

-- a line is just a scatter chart rendered with a line
-- (and with a usually stable x-value series)
line1 ∷ (Traversable f, R2 r) => LineConfig → f (r Double) → Chart b
line1 (LineConfig s c) ps = case NumHask.Prelude.head ps of
  Nothing -> mempty
  Just p0 -> stroke (trailFromVertices (toList $ (p2 . unr2) . view _xy <$> ps)
              `at`
              p2 (unr2 (view _xy p0))) # lcA (color c) # lwN s

-- dots on the XY plane
scatter1 ∷ (Traversable f, R2 r) => ScatterConfig → f (r Double) → Chart b
scatter1 (ScatterConfig s c) ps =
  atPoints (toList $ (p2 . unr2) . view _xy <$> ps)
    (repeat $ circle s #
     blob c
    )

-- | rectangles specified using a V4 x y z w where
-- (x,y) is location of lower left corner
-- (z,w) is location of upper right corner
rect1 :: (Traversable f) => RectConfig -> f (Rect Double) -> Chart b
rect1 cfg rs = mconcat $ toList $
    (\(Rect (V2 (Range (x,z)) (Range (y,w)))) ->
       (unitSquare #
        moveTo (p2 (0.5,0.5)) #
        Diagrams.scaleX (if z-x==zero then eps else z-x) #
        Diagrams.scaleY (if w-y==zero then eps else w-y) #
        moveTo (p2 (x,y)) #
        fcA (color $ cfg ^. rectColor) #
        lcA (color $ cfg ^. rectBorderColor) #
        lw 1
       )) <$> rs

arrow1 :: (Traversable f) => ArrowConfig Double -> f (V4 Double) -> Chart b
arrow1 cfg qs =
    fcA (color $ cfg ^. arrowColor) $ position $
    zip
    ((\(V4 x y _ _) -> p2 (x,y)) <$> toList qs)
    (arrowStyle cfg <$> toList qs)

arrowStyle :: ArrowConfig Double -> V4 Double -> Chart b
arrowStyle cfg (V4 _ _ z w) =
    arrowAt' opts (p2 (0, 0)) (sL *^ V2 z w)
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

-- | convert from an XY to a polymorphic qdiagrams rectangle
box ::
    ( Field (N t)
    , N t ~ Double
    , V t ~ V2
    , HasOrigin t
    , Transformable t
    , TrailLike t) =>
    Rect Double -> t
box (Rect (V2 x y)) =
    moveOriginTo (p2 ( -x^.low - (x^.width/2)
                     , -y^.low - (y^.width/2))) $
    Diagrams.scaleX (if x^.width==zero then eps else x^.width) $
    Diagrams.scaleY (if y^.width==zero then eps else y^.width)
    unitSquare

-- | a pixel is a rectangle with a color.
pixel1 :: (Traversable f) => f (Rect Double, Color) -> Chart b
pixel1 rs = mconcat $ toList $
    (\(Rect (V2 (Range (x,z)) (Range (y,w))), c) ->
       (unitSquare #
        moveTo (p2 (0.5,0.5)) #
        Diagrams.scaleX (if z-x==zero then eps else z-x) #
        Diagrams.scaleY (if w-y==zero then eps else w-y) #
        moveTo (p2 (x,y)) #
        fcA (color c) #
        lcA transparent #
        lw 0
       )) <$> rs

-- * charts are recipes for constructing a QDiagram from a specification of the XY plane to be projected on to (XY), a list of traversable vector containers and a list of configurations.  The charts are self-scaling.
-- | a chart of scattered dot points scaled to its own range
scatterChart ::
    (R2 r, Traversable f) =>
    [ScatterConfig] ->
    Aspect ->
    [f (r Double)] ->
    Chart a
scatterChart defs asp xyss =
    scatterChartWithRange (foldMap rangeR2 xyss) defs asp xyss

-- | a chart of scattered dot points with a specific range
scatterChartWithRange ::
    (R2 r, Traversable f) =>
    Rect Double ->
    [ScatterConfig] ->
    Aspect ->
    [f (r Double)] ->
    Chart a
scatterChartWithRange cr defs (Aspect xy) xyss =
    mconcat $ zipWith scatter1 defs (projectR2 cr xy <$> xyss)

-- | a chart of lines scaled to its own range
lineChart ::
    (R2 r, Traversable f) =>
    [LineConfig] ->
    Aspect ->
    [f (r Double)] ->
    Chart a
lineChart defs asp xyss =
    lineChartWithRange (foldMap rangeR2 xyss) defs asp xyss

-- | a chart of lines with a specific range
lineChartWithRange ::
    (R2 r, Traversable f) =>
    Rect Double ->
    [LineConfig] ->
    Aspect ->
    [f (r Double)] ->
    Chart a
lineChartWithRange cr defs (Aspect xy) xyss =
    mconcat $ zipWith line1 defs (projectR2 cr xy <$> xyss)

-- | a chart of histograms scaled to its own range
histChart ::
    (Traversable f) =>
    [RectConfig] ->
    Aspect ->
    [f (Rect Double)] ->
    Chart a
histChart defs asp rs =
    histChartWithRange (fold $ fold <$> rs) defs asp rs

-- | a chart of histograms with a specific range
histChartWithRange ::
    (Traversable f) =>
    Rect Double ->
    [RectConfig] ->
    Aspect ->
    [f (Rect Double)] ->
    Chart a
histChartWithRange cr defs (Aspect xy) rs =
    mconcat . zipWith rect1 defs $ fmap (projectRect cr xy) <$> rs

toPixels :: Rect Double -> (V2 Double -> Double) -> PixelConfig -> [(Rect Double, Color)]
toPixels xy f cfg = zip g cs
    where
      g = grid xy (view pixelGrain cfg)
      xs = f . midRect <$> g
      (Range (lx,ux)) = range xs
      (Range (lc0,uc0)) = view pixelGradient cfg
      cs = uncolor . (\x -> blend ((x - lx)/(ux - lx)) (color lc0) (color uc0)) <$> xs

rescalePixels :: Rect Double -> [(Rect Double, Color)] -> [(Rect Double, Color)]
rescalePixels xy xys = zip vs cs
  where
    vs = projectRect (fold $ fst <$> xys) xy . fst <$> xys
    cs = snd <$> xys

-- | pixels over an XY plane using a function
pixelf ::
    PixelConfig ->
    Aspect ->
    Rect Double ->
    (V2 Double -> Double) ->
    Chart a
pixelf cfg (Aspect asp) xy f =
    pixel1 $ rescalePixels asp (toPixels xy f cfg)

-- | arrow lengths and sizes also need to be scaled, and so arrows doesnt fit as neatly into the whole scaling idea
arrowChartWithRange ::
    (Traversable f) =>
    V4 (Range Double) ->
    ArrowConfig Double ->
    V4 (Range Double) ->
    f (V4 Double) ->
    Chart a
arrowChartWithRange cr cfg xy xs =
    arrow1 cfg $ rescaleV4P cr xy <$> xs

arrowChart ::
    (Traversable f) =>
    ArrowConfig Double ->
    V4 (Range Double) ->
    f (V4 Double) ->
    Chart a
arrowChart cfg xy xs =
    arrow1 cfg $ rescaleV4P (rangeV4 xs) xy <$> xs

-- | rescale a V4 from rold to rnew
rescaleV4P :: V4 (Range Double) -> V4 (Range Double) -> V4 Double -> V4 Double
rescaleV4P rold rnew q =
    over _x (project (rold^._x) (rnew^._x)) $
    over _y (project (rold^._y) (rnew^._y)) $
    over _z (project (rold^._z) (rnew^._z)) $
    over _w (project (rold^._w) (rnew^._w))
    q

-- | rescale a container of V4s
rescaleV4 :: (Functor f) =>
    V4 (Range Double) -> V4 (Range Double) -> f (V4 Double) -> f (V4 Double)
rescaleV4 rold rnew qs = rescaleV4P rold rnew <$> qs

-- | scale a double container of V4s from the current range
scaleV4s :: (Traversable f) =>
    V4 (Range Double) -> f (V4 Double) -> f (V4 Double)
scaleV4s r f = rescaleV4 (rangeV4 f) r f

-- | V4 range of a V4 container
rangeV4 :: (Traversable f) => f (V4 Double) -> V4 (Range Double)
rangeV4 qs = V4 rx ry rz rw
  where
    rx = range $ toList (view _x <$> qs)
    ry = range $ toList (view _y <$> qs)
    rz = range $ toList (view _z <$> qs)
    rw = range $ toList (view _w <$> qs)

rangeV42Rect :: V4 (Range Double) -> Rect Double
rangeV42Rect (V4 x y z w) = Rect (V2 (x<>z) (y<>w))

-- * axis rendering

-- | render with a chart configuration
withChart ::
    ( Traversable f
    , R2 r) =>
    ChartConfig ->
    (Aspect -> [f (r Double)] -> QDiagram a V2 Double Any) ->
    [f (r Double)] ->
    Chart' a
withChart conf renderer d = case conf^.chartRange of
  Nothing ->
      renderer (conf^.chartAspect) d <>
      axes (chartRange .~ Just (foldMap rangeR2 d) $ conf)
  Just axesRange ->
      combine (conf ^. chartAspect)
      [ QChart renderer r d
      , QChart
        (\asp _ ->
           axes
           ( chartAspect.~asp
           $ chartRange .~ Just axesRange
           $ conf))
        r
        []
      ]
    where
      r = foldMap rangeR2 d

axes ::
    ChartConfig ->
    Chart' a
axes (ChartConfig p a r (Aspect asp@(Rect (V2 ax ay))) cc) =
    L.fold (L.Fold step begin (pad p)) a
  where
    begin = box asp # fcA (color cc) # lcA (withOpacity black 0) # lw none
    step x cfg = beside dir x (mo $ axis1 cfg rendr tickr)
      where
        rendr = case view axisOrientation cfg of
              X -> ax
              Y -> ay
        tickr = case view axisOrientation cfg of
              X -> rx
              Y -> ry
        dir   = case view axisPlacement cfg of
              AxisBottom -> r2 (0,-1)
              AxisTop -> r2 (0,1)
              AxisLeft -> r2 (-1,0)
              AxisRight -> r2 (1,0)
        mo    = case view axisOrientation cfg of
              X -> moveOriginTo (p2 ((-ax^.low)-(ax^.width)/2,0))
              Y -> moveOriginTo (p2 (0,(-ay^.low)-(ay^.width)/2))
        (Rect (V2 rx ry)) = fromMaybe one r

axis1 ::
    AxisConfig ->
    Range Double ->
    Range Double ->
    Chart' b
axis1 cfg rendr tickr = pad (cfg ^. axisPad) $ strut2 $ centerXY $
  atPoints
    (t <$> tickLocations)
    ((`mkLabel` cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) rendr
   # blob (cfg ^. axisColor))
  where
    strut2 x = beside dir x $ strut1 (cfg ^. axisInsideStrut)
    dir = case cfg ^. axisPlacement of
      AxisBottom -> r2 (0,1)
      AxisTop -> r2 (0,-1)
      AxisLeft -> r2 (1,0)
      AxisRight -> r2 (-1,0)
    strut1 = case cfg ^. axisOrientation of
      X -> strutY
      Y -> strutX
    t = case cfg ^. axisOrientation of
      X -> \x -> p2 (x, 0)
      Y -> \y -> p2 (-(cfg ^. axisMarkSize), y)
    ticks0 = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> linearSpaceSensible OuterPos tickr n
      TickExact n -> linearSpace OuterPos tickr n
      TickLabels _ -> []
      TickPlaced xs -> fst <$> xs
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound _ -> project tickr rendr <$> ticks0
      TickExact _ -> project tickr rendr <$> ticks0
      TickLabels ls ->
          project
          (Range (0, fromIntegral $ length ls))
          rendr <$>
          ((\x -> x - 0.5) . fromIntegral <$> [1..length ls])
      TickPlaced _ -> project tickr rendr <$> ticks0
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound _ -> tickFormat <$> ticks0
      TickExact _ -> tickFormat <$> ticks0
      TickLabels ls -> ls
      TickPlaced xs -> snd <$> xs
    tickFormat = sformat (prec 2)
    axisRect h (Range (l,u)) = case cfg ^. axisOrientation of
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

{-
text1 ::
    TextConfig ->
    Text ->
    Chart' b
text1 cfg label =
  Diagrams.Prelude.alignedText
    (cfg ^. textRight)
    (cfg ^. textBottom)
    (Text.unpack label) #
  Diagrams.scale (cfg ^. textSize) #
  fcA (color $ cfg ^.textColor)
  where
    dir = case cfg ^. textOrientation of
      X -> r2 (0,-1)
      Y -> r2 (-1,0)
-}

-- * rendering
-- | render a list of qcharts using a common scale
combine :: Aspect -> [QChart a b] -> Chart' a
combine (Aspect xy) qcs = mconcat $
    (\(QChart c xy1 x) -> c
          (Aspect $ xy `times` xy1 `times` recip xysum)
          x) <$> qcs
    where
      xysum = mconcat $ (\(QChart _ xy1 _) -> xy1) <$> qcs

fileSvg ∷ FilePath → (Double, Double) → Chart SVG → IO ()
fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

-- outline of a chart
bubble ∷ ∀ a. (FromInteger (N a), MultiplicativeGroup (N a), RealFloat (N a), Traced a, V a ~ V2) ⇒ [a] → Int → [V a (N a)]
bubble chart' n = bubble'
  where
    bubble' = ps
    ps = catMaybes $ maxRayTraceV (p2 (0,0)) <$>
        ((\x -> view (Diagrams.Prelude.from r2PolarIso) (1, x @@ rad)) .
         (\x -> fromIntegral x/10.0) <$> [0..n]) <*>
        chart'

histCompare :: DealOvers -> Histogram -> Histogram -> Chart' a
histCompare o h1 h2 =
    let h = fromHist o h1
        h' = fromHist o h2
        h'' = zipWith (\(Rect (V2 (Range (x,y)) (Range (z,w)))) (Rect (V2 _ (Range (_,w')))) -> Rect (V2 (Range (x,y)) (Range (z,w-w')))) h h'
        flat = Aspect $ Rect (V2 (Range (-0.75,0.75)) (Range (-0.25,0.25)))
    in
      pad 1.1 $
        beside (r2 (0,-1)) (histChart
        [ def
        , rectBorderColor .~ Color 0 0 0 0
        $ rectColor .~ Color 0.333 0.333 0.333 0.1
        $ def ] sixbyfour [h,h'] <>
        axes (ChartConfig 1.1
              [def]
              (Just (fold $ fold [abs <$> h,abs <$> h']))
              sixbyfour (uncolor transparent)))
        (histChart
        [ rectBorderColor .~ Color 0 0 0 0
        $ rectColor .~ Color 0.888 0.333 0.333 0.8
        $ def ] flat [abs <$> h''] <>
        axes (ChartConfig 1.1
              [ axisAlignedTextBottom .~ 0.65 $
                axisAlignedTextRight .~ 1 $
                axisOrientation .~ Y $
                axisPlacement .~ AxisLeft $
                def
              ]
              (Just (fold $ abs <$> h''))
              flat (uncolor transparent)))
