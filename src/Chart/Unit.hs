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
  , Arrow(..)
  , normArrows
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
import NumHask.Pair
import Chart.Types

import Control.Lens hiding (beside, none, (#), at, singular)
import Data.Ord (max)
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude hiding (width, unit, D, Color, scale, zero, scaleX, scaleY, aspect, rect, project)
import Formatting
import Linear hiding (zero, identity, unit, project)

import qualified Control.Foldl as L
import qualified Data.Text as Text
import qualified Diagrams.Prelude as Diagrams
import Data.List (zipWith5)

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

-- | equalize the arrow space width with the data space one.
normArrows :: [Arrow] -> [Arrow]
normArrows xs =
    zipWith Arrow ps as'
  where
    -- data points
    ps = pos <$> xs
    -- arrow vectors
    as = arr <$> xs
    as' = (\x ->
             x *
             width (space $ pos <$> xs :: Rect Double) /
             width (space $ arr <$> xs :: Rect Double)) <$>
          as

-- | data structure for an arrow chart
data Arrow = Arrow
    { pos :: Pair Double -- position of arrow tail
    , arr :: Pair Double -- direction and strength of arrow
    } deriving (Eq, Show)

-- | an arrow chart specified as an `Arrow pos arr` where
-- pos is the location of the arrow tail
-- arr is the location of the arrow head
-- The resulting chart is robust to scale changes across the x and y dimensions, and between the position and arrow coordinates
arrow1 :: ArrowConfig Double -> [Arrow] -> Chart b
arrow1 cfg xs = c
  where
    c = fcA (color $ cfg ^. arrowColor) $ position $
        zipWith5 (\ps' as' hrel' wrel' srel' -> (ps', arrowAt' (opts hrel' wrel') (p2 (0, 0)) ((srel'/norm(as')) *^ as')))
        ((\(Pair x y) -> p2 (x,y)) <$> ps)
        ((\(Pair x y) -> V2 x y) <$> as)
        hrel
        wrel
        srel
    ps = pos <$> xs
    -- arrow vectors
    as = arr <$> xs
    -- width of the data space
    (Pair dx dy) = width ((space ps) :: Rect Double)
    -- norm of arrow vectors relative to the data space metric
    anorm = (\(Pair x y) -> sqrt((x/dx)**2+(y/dy)**2)) <$> as
    -- the maximum arrow vector norm
    (Range _ anormMax) = space anorm
    -- the overall size of the arrows, as a proportion to the data space
    arel = (\x -> (max (anormMax * (cfg ^. arrowMinLength)) (x / anormMax * (cfg ^. arrowMaxLength)))) <$> anorm
    -- size of the head (as a proportion of the data space)
    hrel = (\x -> max (cfg ^. arrowMinHeadLength) ((cfg^.arrowMaxHeadLength) * x)) <$> arel
    -- widt of the staff
    wrel = (\x -> max (cfg ^. arrowMinStaffWidth) ((cfg^.arrowMaxStaffWidth) * x)) <$> arel
    -- length of the staff (taking into account the head length)
    srel = zipWith (\la lh -> max 1e-12 (la - lh)) arel hrel
    -- diagrams arrow options
    opts lh lw'' = with & arrowHead .~ (cfg ^. arrowHeadStyle) &
                 headLength .~ global lh &
                 shaftStyle %~ (lwG lw'' & lcA (color $ cfg ^. arrowColor)) &
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
    Rect Double ->
    ArrowConfig Double ->
    Aspect ->
    [Arrow] ->
    Chart a
arrowChartWithRange cr cfg (Aspect xy) xs =
    arrow1 cfg $ (\(Arrow d a) -> Arrow (project cr xy d) (project cr xy a)) <$> xs

arrowChart ::
    ArrowConfig Double ->
    Aspect ->
    [Arrow] ->
    Chart a
arrowChart cfg xy xs =
    arrowChartWithRange
    (space (pos <$> xs))
    cfg xy xs

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
