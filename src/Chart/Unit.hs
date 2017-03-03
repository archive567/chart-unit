{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart.Unit where

import Chart.Range
import Chart.Types

import Control.Lens hiding (beside, none, (#), at)
import Data.Ord (max, min)
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude hiding (width, unit, D, Color, scale, zero, scaleX, scaleY)
import Formatting
import Linear hiding (zero, identity, unit)
import Tower.Prelude hiding (min,max,from,to, (&))

import qualified Control.Foldl as L
import qualified Data.Text as Text
import qualified Diagrams.Prelude as Diagrams
import qualified Diagrams.TwoD.Text


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
line1 (LineConfig s c) ps = case Tower.Prelude.head ps of
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
-- z is width, and
-- w is height
rect1 :: (Traversable f) => RectConfig -> f (V4 Double) -> Chart b
rect1 cfg qs = mconcat $ toList $
    (\(V4 x y z w) ->
       (unitSquare #
        moveTo (p2 (0.5,0.5)) #
        Diagrams.scaleX (if z-x==zero then eps else z-x) #
        Diagrams.scaleY (if w-y==zero then eps else w-y) #
        moveTo (p2 (x,y)) #
        fcA (color $ cfg ^. rectColor) #
        lcA (color $ cfg ^. rectBorderColor) #
        lw 1
       )) <$> qs

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

arrowLength :: ArrowConfig Double -> Double
arrowLength cfg =
    (0.667 * view arrowHeadSize cfg) +
    min
    (view arrowStaffLength cfg + view arrowMinStaffLength cfg)
    (view arrowMaxStaffLength cfg)

-- | convert from an XY to a polymorphic qdiagrams rectangle
box ::
    ( Field (N t)
    , N t ~ Double
    , V t ~ V2
    , HasOrigin t
    , Transformable t
    , TrailLike t) =>
    XY -> t
box a =
    moveOriginTo (p2 ( -a^._x^.low - (a^._x^.width/2)
                     , -a^._y^.low - (a^._y^.width/2))) $
    Diagrams.scaleX (if a^._x^.width==zero then eps else a^._x^.width) $
    Diagrams.scaleY (if a^._y^.width==zero then eps else a^._y^.width)
    unitSquare

-- * charts are recipes for constructing a QDiagram from a specification of the XY plane to be projected on to (XY), a list of traversable vector containers and a list of configurations.

scatter ::
    (Renderable (Path V2 Double) b, R2 r, Traversable f) =>
    [ScatterConfig] ->
    XY ->
    [f (r Double)] ->
    QDiagram b V2 Double Any
scatter defs xy xyss = mconcat $ zipWith scatter1 defs (scaleR2s xy xyss)

line ::
    (Renderable (Path V2 Double) b, R2 r, Traversable f) =>
    [LineConfig] ->
    XY ->
    [f (r Double)] ->
    QDiagram b V2 Double Any
line defs xy xyss = mconcat $ zipWith line1 defs (scaleR2s xy xyss)

hist ::
    (Renderable (Path V2 Double) b, Traversable f) =>
    [RectConfig] ->
    XY ->
    [f (V4 Double)] ->
    QDiagram b V2 Double Any
hist defs xy xs = centerXY . mconcat . zipWith rect1 defs $ scaleRects xy xs

arrow ::
    (Renderable (Path V2 Double) b, Traversable f) =>
    [ArrowConfig Double] ->
    V4 (Range Double) ->
    [f (V4 Double)] ->
    QDiagram b V2 Double Any
arrow defs xy xs =
    centerXY . mconcat . zipWith arrow1 defs $ scaleV4s xy xs

-- * axis rendering

withAxes ::
    (Renderable (Path V2 Double) b
    , (Renderable (Diagrams.TwoD.Text.Text Double) b)) =>
    ChartConfig ->
    XY ->
    (XY -> [f (r Double)] -> QDiagram b V2 Double Any) ->
    XY ->
    [f (r Double)] ->
    QDiagram b V2 Double Any
withAxes cc axesRange renderer xy d = renderer xy d <> axes cc xy [toCorners axesRange]

axes ::
    ( (Renderable (Diagrams.TwoD.Text.Text Double) b)
    , Renderable (Path V2 Double) b
    , R2 r
    , Traversable f) =>
    ChartConfig ->
    XY ->
    [f (r Double)] ->
    QDiagram b V2 Double Any
axes (ChartConfig p a cc) xy xs = L.fold (L.Fold step begin (pad p)) a
  where
    begin = box xy # fcA (color cc) # lcA (withOpacity black 0) # lw none
    step x cfg = beside dir x (mo $ axis1 cfg rendr tickr)
      where
        rendr = case view axisOrientation cfg of
              X -> xy^._x
              Y -> xy^._y
        tickr = case view axisOrientation cfg of
              X -> rangeR2s xs^._x
              Y -> rangeR2s xs^._y
        dir   = case view axisPlacement cfg of
              AxisBottom -> r2 (0,-1)
              AxisTop -> r2 (0,1)
              AxisLeft -> r2 (-1,0)
              AxisRight -> r2 (1,0)
        mo    = case view axisOrientation cfg of
              X -> moveOriginTo (p2 ((-xy^._x^.low)-(xy^._x^.width)/2,0))
              Y -> moveOriginTo (p2 (0,(-xy^._x^.low)-(xy^._x^.width)/2))

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
    ticks = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> ticksRound tickr n
      TickExact n -> ticksExact tickr n
      TickLabels _ -> []
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound _ -> rescaleP tickr rendr <$> ticks
      TickExact _ -> rescaleP tickr rendr <$> ticks
      TickLabels ls ->
          rescaleP
          (Range (0, fromIntegral $ length ls))
          rendr <$>
          ((\x -> x - 0.5) . fromIntegral <$> [1..length ls])
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound _ -> tickFormat <$> ticks
      TickExact _ -> tickFormat <$> ticks
      TickLabels ls -> ls
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


-- * rendering
-- | render a list of qcharts using a common scale
combine :: ((Renderable (Diagrams.TwoD.Text.Text Double) a), Renderable (Path V2 Double) a) => XY -> [QChart a] -> QDiagram a V2 Double Any
combine xy qcs = mconcat $
    (\(QChart c xy1 x) -> c
          (xy `times` xy1 `times` recip xysum)
          x) <$> qcs
    where
      xysum = mconcat $ (\(QChart _ xy1 _) -> xy1) <$> qcs

fileSvg ∷ FilePath → (Double, Double) → Chart SVG → IO ()
fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

-- outline of a chart
bubble ∷ ∀ a. (MultiplicativeGroup (N a), RealFloat (N a), Traced a, V a ~ V2) ⇒ [a] → Int → [V a (N a)]
bubble chart' n = bubble'
  where
    bubble' = ps
    ps = catMaybes $ maxRayTraceV (p2 (0,0)) <$>
        ((\x -> view (Diagrams.Prelude.from r2PolarIso) (1, x @@ rad)) .
         (\x -> fromIntegral x/10.0) <$> [0..n]) <*>
        chart'
