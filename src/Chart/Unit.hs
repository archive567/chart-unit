{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  , arrowStyle'
  , arrow1'
  , normArrowLength
  ) where

import NumHask.Prelude hiding (min,max,from,to,(&))

import NumHask.Range
import NumHask.Rect
import NumHask.Histogram
import NumHask.Pair
import Chart.Types

import Control.Lens hiding (beside, none, (#), at, singular)
import Data.Ord (max, min)
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude hiding (width, unit, D, Color, scale, zero, scaleX, scaleY, aspect, rect, project)
import Formatting
import Linear hiding (zero, identity, unit, project)

import qualified Control.Foldl as L
import qualified Data.Text as Text
import qualified Diagrams.Prelude as Diagrams
import Data.List (zipWith3, zipWith4, zipWith5)
import Data.Functor.Compose

import NumHask.Naperian()

instance R1 Pair where
    _x f (Pair a b) = (`Pair` b) <$> f a

instance R2 Pair where
    _y f (Pair a b) = Pair a <$> f b
    _xy f p = fmap (\(V2 a b) -> Pair a b) . f . (\(Pair a b) -> V2 a b) $ p

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

-- | rectangles specified using a Rect x z y w where
-- (x,y) is location of lower left corner
-- (z,w) is location of upper right corner
rect1 :: (Traversable f) => RectConfig -> f (Rect Double) -> Chart b
rect1 cfg rs = mconcat $ toList $
    (\(Rect x z y w) ->
       (unitSquare #
        moveTo (p2 (0.5,0.5)) #
        Diagrams.scaleX (if z-x==zero then eps else z-x) #
        Diagrams.scaleY (if w-y==zero then eps else w-y) #
        moveTo (p2 (x,y)) #
        fcA (color $ cfg ^. rectColor) #
        lcA (color $ cfg ^. rectBorderColor) #
        lw 1
       )) <$> rs


-- | an arrow specified as a Rect x z y w where
-- Pair x y is the location of the arrow tail
-- Pair z w is the location of the arrow head
arrow1 :: (Traversable f) => ArrowConfig Double -> f (Compose Pair Pair Double) -> Chart b
arrow1 cfg qs =
    fcA (color $ cfg ^. arrowColor) $ position $
    zipWith5 (\xy wz lh lw' ls -> (xy, arrowAt' (opts lh lw') (p2 (0, 0)) (ls *^ wz)))
    ((\(Pair x y) -> p2 (x,y)) <$> ds)
    ((\(Pair x y) -> V2 x y) <$> as)
    habs
    wabs
    sabs
  where
    -- data points
    ds = ((\(Compose (Pair d _)) -> d) <$> toList qs)
    -- arrow vectors
    as = ((\(Compose (Pair _ a)) -> a) <$> toList qs)
    -- width of the data space
    (Pair dx dy) = width ((space ds) :: Rect Double)
    -- norm of arrow vectors relative to the data space metric
    anorm = (\(Pair x y) -> sqrt((x/dx)**2+(y/dy)**2)) <$> as
    -- norm of the data space relative to each arrow vector
    invanorm = (\(Pair x y) -> sqrt((dx/x)**2+(dy/y)**2)) <$> as
    -- the maximum arrow vector norm
    (Range _ anormMax) = space anorm
    -- the overall size of the arrows, as a proportion to the data space
    arel = (\x -> x * (cfg ^. arrowMaxStaffLength) / anormMax) <$> anorm
    -- size of the head (as a proportion of the data space)
    hrel = (\x -> min (cfg ^. arrowMinHeadSize) ((cfg^.arrowHeadSize) * x)) <$> arel
    -- widt of the staff
    wrel = (\x -> min (cfg ^. arrowMinStaffWidth) ((cfg^.arrowStaffWidth) * x)) <$> arel
    -- length of the staff (taking into account the head length)
    srel = zipWith (\la lh -> max 0 (la - lh)) arel hrel
    -- absolute size of the heads
    habs = zipWith (*) hrel invanorm
    -- absolute width of the staff
    wabs = zipWith (*) wrel invanorm
    -- absolute size of the staff
    sabs = zipWith (*) srel invanorm
    -- diagrams arrow options
    opts lh lw'' = with & arrowHead .~ tri &
                 headLength .~ global lh &
                 shaftStyle %~ (lwG lw'' & lcA (color $ cfg ^. arrowColor)) &
                 headStyle %~ (lcA (color $ cfg ^. arrowColor) & fcA (color $ cfg ^. arrowColor))

normArrows :: (Traversable f) => f (Compose Pair Pair Double) -> [Compose Pair Pair Double]
normArrows qs =
    zipWith (\xy wz -> Compose (Pair xy wz))
    xy
    wz'
  where
    -- data points
    xy = ((\(Compose (Pair d _)) -> d) <$> toList qs)
    -- arrow vectors
    wz = ((\(Compose (Pair _ a)) -> a) <$> toList qs)
    wz' = project (space wz :: Rect Double) (space xy) <$> wz

normArrowLength :: ArrowConfig Double -> [Compose Pair Pair Double] -> [Compose Pair Pair Double]
normArrowLength cfg xs = zipWith (\d a -> Compose (Pair d a)) pos v'
  where
    pos = (\(Compose (Pair d _)) -> d) <$> xs
    v = (\(Compose (Pair _ a)) -> a) <$> toList xs
    l = (\(Pair x y) -> sqrt(x**2+y**2)) <$> v
    (Range _ maxL) = space l
    l' = (\x -> (cfg^.arrowStaffLength) * (min (cfg ^. arrowMinStaffLength) x)) <$> (/maxL) <$> l
    r = width (space pos :: Rect Double)
    rpos = width (space v :: Rect Double)
    v' = zipWith (NumHask.Prelude.*.) l' ((\x -> x * (r / rpos)) <$> v)
    opts = with & arrowHead .~ tri &
           headLength .~ global (cfg ^. arrowHeadSize) &
           shaftStyle %~ (lwG (cfg ^. arrowStaffWidth) & lcA (color $ cfg ^. arrowColor)) &
           headStyle %~ (lcA (withOpacity yellow 0.5) & fcA (withOpacity yellow 0.5))


-- | arrow styles from diagrams
arrowStyle :: ArrowConfig Double -> Double -> Pair Double -> Chart b
arrowStyle cfg maxNorm (Pair z w) =
    arrowAt' opts (p2 (0, 0)) (V2 z w)
  where
    trunc minx maxx a = min (max minx a) maxx
    m = norm (V2 z w) / maxNorm
    hs = trunc (cfg ^. arrowMinHeadSize) (cfg ^. arrowMaxHeadSize) (cfg ^. arrowHeadSize * m)
    sW = trunc (cfg ^. arrowMinStaffWidth) (cfg ^. arrowMaxStaffWidth) (cfg ^. arrowStaffWidth * m)
    sL = trunc (cfg ^. arrowMinStaffLength) (cfg ^. arrowMaxStaffLength) (cfg ^. arrowStaffLength * m)
    opts = with & arrowHead .~ tri &
           headLength .~ global (cfg ^. arrowHeadSize) &
           shaftStyle %~ (lwG (cfg ^. arrowStaffWidth) & lcA (color $ cfg ^. arrowColor)) &
           headStyle %~ (lcA (withOpacity yellow 0.5) & fcA (withOpacity yellow 0.5))


arrow1' cfg qs =
    (arrowStyle' cfg maxNorm <$> arr)
  where
    arr = (\(Rect x z y w) -> Pair (z-x) (w-y)) <$> toList qs
    (Range _ maxNorm) = space $ (\(Pair x y) -> sqrt(x**2+y**2)) <$> arr


-- arrowStyle' :: ArrowConfig Double -> Double -> Pair Double -> (Double, Double, Double, Double)
arrowStyle' cfg maxNorm (Pair z w) =
    (m,hs,sW,sL,z,w)
  where
    trunc minx maxx a = min (max minx a) maxx
    m = norm (V2 z w) / maxNorm
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
box (Ranges x y) =
    moveOriginTo (p2 ( -lower x - (width x/2)
                     , -lower y - (width y/2))) $
    Diagrams.scaleX (if singular x then eps else width x) $
    Diagrams.scaleY (if singular y then eps else width y)
    unitSquare

-- | a pixel is a rectangle with a color.
pixel1 :: (Traversable f) => f (Rect Double, Color) -> Chart b
pixel1 rs = mconcat $ toList $
    (\(Rect x z y w, c) ->
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
    (Traversable f, R2 Pair) =>
    [ScatterConfig] ->
    Aspect ->
    [f (Pair Double)] ->
    Chart a
scatterChart defs asp xyss =
    scatterChartWithRange (foldMap rangeR2 xyss) defs asp xyss

-- | a chart of scattered dot points with a specific range
scatterChartWithRange ::
    (Traversable f, R2 Pair) =>
    Rect Double ->
    [ScatterConfig] ->
    Aspect ->
    [f (Pair Double)] ->
    Chart a
scatterChartWithRange cr defs (Aspect xy) xyss =
    mconcat $ zipWith scatter1 defs (projectR2 cr xy <$> xyss)

-- | a chart of lines scaled to its own range
lineChart ::
    (Traversable f) =>
    [LineConfig] ->
    Aspect ->
    [f (Pair Double)] ->
    Chart a
lineChart defs asp xyss =
    lineChartWithRange (foldMap rangeR2 xyss) defs asp xyss

-- | a chart of lines with a specific range
lineChartWithRange ::
    (Traversable f) =>
    Rect Double ->
    [LineConfig] ->
    Aspect ->
    [f (Pair Double)] ->
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

toPixels :: Rect Double -> (Pair Double -> Double) -> PixelConfig -> [(Rect Double, Color)]
toPixels xy f cfg = zip g cs
    where
      g = grid xy (view pixelGrain cfg)
      xs = f . mid <$> g
      (Range lx ux) = space xs
      (Range lc0 uc0) = view pixelGradient cfg
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
    (Pair Double -> Double) ->
    Chart a
pixelf cfg (Aspect asp) xy f =
    pixel1 $ rescalePixels asp (toPixels xy f cfg)

-- | arrow lengths and sizes also need to be scaled, and so arrows doesnt fit as neatly into the whole scaling idea
arrowChartWithRange ::
    (Traversable f) =>
    Rect Double ->
    ArrowConfig Double ->
    Aspect ->
    f (Compose Pair Pair Double) ->
    Chart a
arrowChartWithRange cr cfg (Aspect xy) xs =
    arrow1 cfg $ (\(Compose (Pair d a)) -> Compose (Pair (project cr xy d) ((project cr xy a)))) <$> xs

arrowChart ::
    (Traversable f) =>
    ArrowConfig Double ->
    Aspect ->
    f (Compose Pair Pair Double) ->
    Chart a
arrowChart cfg xy xs =
    arrowChartWithRange
    ((space $ (\(Compose (Pair d _)) -> d) <$> xs) `mappend`
     (space $ (\(Compose (Pair _ a)) -> a) <$> xs))
    cfg xy xs

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
    rx = space $ toList (view _x <$> qs)
    ry = space $ toList (view _y <$> qs)
    rz = space $ toList (view _z <$> qs)
    rw = space $ toList (view _w <$> qs)

rangeV42Rect :: V4 (Range Double) -> Rect Double
rangeV42Rect (V4 x y z w) = Ranges (x `mappend` z) (y `mappend` w)

-- * axis rendering

-- | render with a chart configuration
withChart ::
    ( Traversable f ) =>
    ChartConfig ->
    (Aspect -> [f (Pair Double)] -> QDiagram a V2 Double Any) ->
    [f (Pair Double)] ->
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
axes (ChartConfig p a r (Aspect asp@(Ranges ax ay)) cc) =
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
              X -> moveOriginTo (p2 ((-lower ax)-width ax/2,0))
              Y -> moveOriginTo (p2 (0,(-lower ay)-width ay/2))
        (Ranges rx ry) = fromMaybe one r

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
          (Range 0 (fromIntegral $ length ls))
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
    axisRect h (Range l u) = case cfg ^. axisOrientation of
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
        h'' = zipWith (\(Rect x y z w) (Rect _ _ _ w') -> Rect x y z (w-w')) h h'
        flat = Aspect $ Rect -0.75 0.75 -0.25 0.25
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
