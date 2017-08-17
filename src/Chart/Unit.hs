{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
     
module Chart.Unit
  ( glyphs
  , text_
  , textp
  , labelled
  , rect_
  , rects
  , texts
  , lines
  , line_
  , glines
  , pixel1
  , Arrow(..)
  , normArrows
  , arrow1
  , arrow_
  , range
  , scale
  , scatterChart
  , scatterChart_
  , lineChart
  , lineChart_
  , histChart
  , histChart_
  , arrowChart
  , arrowChart_
  , toPixels
  , pixelf
  , textChart
  , textChart_
  , withChart
  , precision
  , axes
  , axis
  , legend1 
  , combine
  , fileSvg
  -- , histCompareChart
  ) where

import Chart.Types
import NumHask.Prelude hiding (min,max,from,to,(&),local)
import NumHask.Space
import NumHask.Range
import NumHask.Rect
import NumHask.Pair

import Control.Lens hiding (beside, none, (#), at, singular)
import Data.Ord (max)
import Diagrams.Prelude hiding (width, unit, D, Color, scale, zero, scaleX, scaleY, aspect, rect, project, lineColor, (*.))
import Formatting
import qualified Control.Foldl as L
import qualified Data.Text as Text
import qualified Diagrams.Prelude as Diagrams
import Diagrams.Backend.SVG (B, SVG, renderSVG)
import Graphics.SVGFonts
import qualified Diagrams.TwoD.Size as D
import Data.List (zipWith5, nub)

glyph_ :: GlyphConfig -> Diagram B
glyph_ cfg =
    ((cfg ^. glyphShape) (cfg ^. glyphSize)) #
    fcA ( cfg ^. glyphColor) #
    lcA ( cfg ^. glyphBorderColor) #
    lw (local $ cfg ^. glyphBorderSize)

glyph :: (R2 r) => GlyphConfig -> r Double -> Diagram B
glyph cfg p =
  moveTo (p_ p) $
    ((cfg ^. glyphShape) (cfg ^. glyphSize)) #
    fcA ( cfg ^. glyphColor) #
    lcA ( cfg ^. glyphBorderColor) #
    lw (local $ cfg ^. glyphBorderSize)

-- | text primitive
text_ ::
    TextConfig ->
    Text ->
    Diagram B
text_ cfg t =
    (moveTo
     (p2
       (alignH (cfg^.textAlignH) * D.width path,
        alignV (cfg^.textAlignV) * D.height path))
    path #
    fcA ( cfg ^.textColor) #
    lw 0 #
    fillRule (cfg ^. textFillRule)) #
    Diagrams.rotate (cfg ^. textRotation @@ deg) #
    pad (cfg^.textPad)
  where
    path = (stroke $ textSVG'
            (TextOpts lin2 INSIDE_H KERN False (cfg ^. textSize) (cfg ^. textSize))
            (Text.unpack t))
 
-- | text anchored in a position.
textp ::
    (R2 r) =>
    TextConfig ->
    Text ->
    (r Double) ->
    Diagram B
textp cfg t p =
    moveTo (p_ p) (text_ cfg t)

-- | texts are rendered svg text and positions.
texts ::
    (Traversable f, R2 r) =>
    TextConfig ->
    f (Text, r Double) ->
    Diagram B
texts cfg xs =
    atPoints (toList $ p_ . snd <$> xs)
    (toList $ text_ cfg . fst <$> xs)

labelled :: LabelConfig -> Diagram B -> Text -> Diagram B
labelled cfg s t =
  beside o
  (beside o s (strut o # Diagrams.scale (cfg ^. labelStrut)))
  (text_ (cfg^.labelText) t) 
  where
    o = r_ (cfg ^. labelOrientation)

-- glyphs on the XY plane
glyphs :: (R2 r, Functor t, Foldable t) => GlyphConfig -> t (r Double) -> Diagram B
glyphs cfg xs = mconcat $ toList $ glyph cfg <$> xs

-- a line connecting a series of points
lines :: (Traversable f, R2 r) => LineConfig -> f (r Double) -> Diagram B
lines cfg xs = case NumHask.Prelude.head xs of
  Nothing -> mempty
  Just p -> trailLike (trailFromVertices (toList $ p_ <$> xs) `at` p_ p) #
    lcA ( cfg ^. lineColor) #
    lwN (cfg ^. lineSize)

-- a single line connecting 2 points
-- line :: (R2 r) => LineConfig -> (Pair (r Double)) -> Diagram B b
line_ :: (R2 r) => LineConfig -> Pair (r Double) -> Diagram B
line_ cfg (Pair x0 x1) =
    stroke (trailFromVertices ([p_ x0, p_ x1]) `at` p_ x0) #
    lcA ((cfg ^. lineColor)) #
    lwN (cfg ^. lineSize)

-- | lines with glyphs atop
-- glines ∷ (Semigroup a, Monoid a, Transformable a, TrailLike a, HasOrigin a, HasStyle a, Traversable f, R2 r, N a ~ Double, V a ~ V2) => LineConfig -> GlyphConfig -> (Double -> a) → f (r Double) → a
glines :: (Traversable f, R2 r) => LineConfig -> GlyphConfig -> f (r Double) -> Diagram B
glines cfg gcfg xs = glyphs gcfg xs <> lines cfg xs

-- | a single rectangle specified using a Rect x z y w where
-- (x,y) is location of lower left corner
-- (z,w) is location of upper right corner
rect_ :: (N b ~ Double, V b ~ V2, Transformable b, HasOrigin b, TrailLike b, HasStyle b) => RectConfig -> Rect Double -> b
rect_ cfg (Rect x z y w) = 
    unitSquare #
    moveTo (p2 (0.5,0.5)) #
    scaleX (z-x) #
    scaleY (w-y) #
    moveTo (p2 (x,y)) #
    fcA ( cfg ^. rectColor) #
    lcA ( cfg ^. rectBorderColor) #
    lw (local $ cfg ^. rectBorderWidth)

-- | rectangles with the same configuration
rects :: (V a ~ V2, N a ~ Double, Functor t, HasStyle a, TrailLike a, HasOrigin a, Transformable a, Foldable t, Monoid a) => RectConfig -> t (Rect Double) -> a
rects cfg xs = mconcat $ toList $ rect_ cfg <$> xs

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
arrow1 :: ArrowConfig Double -> [Arrow] -> Diagram B
arrow1 cfg xs = c
  where
    c = fcA ( cfg ^. arrowColor) $ position $
        zipWith5 (\ps' as' hrel' wrel' srel' ->
                    (ps',
                     arrowAt' (opts hrel' wrel') (p2 (0, 0))
                     ((srel'/norm(as')) *^ as')))
        (p_ <$> ps)
        (r_ <$> as)
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
    arel = (\x -> (max (anormMax * (cfg ^. arrowMinLength))
                  (x / anormMax * (cfg ^. arrowMaxLength)))) <$> anorm
    -- size of the head (as a proportion of the data space)
    hrel = (\x -> max (cfg ^. arrowMinHeadLength) ((cfg^.arrowMaxHeadLength) * x)) <$>
        arel
    -- widt of the staff
    wrel = (\x -> max (cfg ^. arrowMinStaffWidth) ((cfg^.arrowMaxStaffWidth) * x)) <$>
        arel
    -- length of the staff (taking into account the head length)
    srel = zipWith (\la lh -> max 1e-12 (la - lh)) arel hrel
    -- diagrams arrow options
    opts lh lw'' = with & arrowHead .~ (cfg ^. arrowHeadStyle) &
                 headLength .~ global lh &
                 shaftStyle %~ (lwG lw'' & lcA ( cfg ^. arrowColor)) &
                 headStyle %~ (lcA ( cfg ^. arrowColor) & fcA ( cfg ^. arrowColor))

-- | single arrow, represented as a Rect
-- The basic arrow technology requires non-singleton ranges in both the x and y dimensions for arrows and positions, and this is a horrible hack to get that (and still fit in with the ArrowConfig).
arrow_ :: ArrowConfig Double -> Rect Double -> Diagram B
arrow_ cfg (Rect x w y z) = arrow1' cfg [Arrow (Pair x y) (Pair w z), Arrow (Pair (x+1.0) (y+1)) (Pair (w+1) (z+1))]
  where
    arrow1' cfg' xs = fcA ( cfg' ^. arrowColor) $ position $ take 1 $
        zipWith5 (\ps' as' hrel' wrel' srel' ->
                    (ps',
                     arrowAt' (opts hrel' wrel') (p2 (0, 0))
                     ((srel'/norm(as')) *^ as')))
        (p_ <$> ps)
        (r_ <$> as)
        hrel
        wrel
        srel
      where
        ps = pos <$> xs
        -- arrow vectors
        as = arr <$> xs
        -- width of the data space
        (Pair dx dy) = width ((space ps) :: Rect Double)
        -- norm of arrow vectors relative to the data space metric
        anorm = (\(Pair x' y') -> sqrt((x'/dx)**2+(y'/dy)**2)) <$> as
        -- the maximum arrow vector norm
        (Range _ anormMax) = space anorm
        -- the overall size of the arrows, as a proportion to the data space
        arel = (\x' -> (max (anormMax * (cfg ^. arrowMinLength))
                      (x' / anormMax * (cfg ^. arrowMaxLength)))) <$> anorm
        -- size of the head (as a proportion of the data space)
        hrel = (\x' -> max (cfg ^. arrowMinHeadLength)
                 ((cfg^.arrowMaxHeadLength) * x')) <$>
               arel
        -- widt of the staff
        wrel = (\x' -> max (cfg ^. arrowMinStaffWidth)
                 ((cfg^.arrowMaxStaffWidth) * x')) <$>
            arel
        -- length of the staff (taking into account the head length)
        srel = zipWith (\la lh -> max 1e-12 (la - lh)) arel hrel
        -- diagrams arrow options
        opts lh lw'' = with & arrowHead .~ (cfg ^. arrowHeadStyle) &
                 headLength .~ global lh &
                 shaftStyle %~ (lwG lw'' & lcA ( cfg ^. arrowColor)) &
                 headStyle %~ (lcA ( cfg ^. arrowColor) & fcA ( cfg ^. arrowColor))

-- | a pixel is a rectangle with a color.
pixel1 :: (Traversable f) => f (Rect Double, AlphaColour Double) -> Diagram B
pixel1 rs = mconcat $ toList $
    (\(Rect x z y w, c) ->
       (unitSquare #
        moveTo (p2 (0.5,0.5)) #
        scaleX (z-x) #
        scaleY (w-y) #
        moveTo (p2 (x,y)) #
        fcA c #
        lcA transparent #
        lw 0
       )) <$> rs

scale ::
    (Functor f, Functor f1) =>
    Rect Double ->
    Rect Double ->
    f1 (f (Pair Double)) ->
    f1 (f (Pair Double))
scale r0 r1 xyss = fmap (project r0 r1) <$> xyss

range :: (Foldable f, Foldable f1) => f1 (f (Pair Double)) -> Rect Double
range xyss = foldMap space xyss

-- * charts are recipes for constructing a Diagram B from a specification of the XY plane to be projected on to (XY), a list of traversable vector containers and a list of configurations.  The charts are self-scaling.
 
-- | a chart of lines
lineChart ::
    (Traversable f) =>
    [LineConfig] ->
    Aspect ->
    Rect Double ->
    [f (Pair Double)] ->
    Diagram B
lineChart defs (Aspect asp) r xyss =
    mconcat $ zipWith lines defs (scale r asp xyss)

-- | a chart of lines scaled to its own range
lineChart_ ::
    (Traversable f) =>
    [LineConfig] ->
    Aspect ->
    [f (Pair Double)] ->
    Diagram B
lineChart_ defs asp xyss =
    lineChart defs asp (range xyss) xyss

-- | a chart of scattered dot points
-- scatterChart :: (HasOrigin a, HasStyle a, InSpace V2 Double a, Monoid a, Traversable f ) => [GlyphConfig] -> [Double -> a] -> Aspect -> Rect Double -> [f (Pair Double)] -> a

scatterChart ::
    (Traversable f) =>
    [GlyphConfig] -> Aspect -> Rect Double -> [f (Pair Double)] -> Diagram B
scatterChart defs (Aspect asp) r xyss =
    mconcat $ zipWith glyphs defs (scale r asp xyss)

-- | a chart of scattered dot points scaled to its own range
scatterChart_ :: (Traversable f) =>
                [GlyphConfig] -> Aspect -> [f (Pair Double)] -> Diagram B
scatterChart_ defs asp xyss =
    scatterChart defs asp (range xyss) xyss

-- | a chart of histograms
histChart ::
    (Traversable f) =>
    [RectConfig] ->
    Aspect ->
    Rect Double ->
    [f (Rect Double)] ->
    Diagram B
histChart defs (Aspect asp) r rs =
    mconcat . zipWith rects defs $ fmap (projectRect r asp) <$> rs

-- | a chart of histograms scaled to its own range
histChart_ ::
    (Traversable f) =>
    [RectConfig] ->
    Aspect ->
    [f (Rect Double)] ->
    Diagram B
histChart_ defs asp rs =
    histChart defs asp (fold $ fold <$> rs) rs

toPixels ::
    Rect Double ->
    (Pair Double -> Double) ->
    PixelConfig ->
    [(Rect Double, AlphaColour Double)]
toPixels xy f cfg = zip g cs
    where
      g = gridSpace xy (view pixelGrain cfg)
      xs = f . mid <$> g
      (Range lx ux) = space xs
      (Range lc0 uc0) = view pixelGradient cfg
      cs = (\x -> blend ((x - lx)/(ux - lx)) (lc0) (uc0)) <$> xs

projectPixels ::
    Rect Double ->
    [(Rect Double, AlphaColour Double)] ->
    [(Rect Double, AlphaColour Double)]
projectPixels xy xys = zip vs cs
  where
    vs = projectRect (fold $ fst <$> xys) xy . fst <$> xys
    cs = snd <$> xys

-- | pixels over an XY plane using a function
pixelf ::
    PixelConfig ->
    Aspect ->
    Rect Double ->
    (Pair Double -> Double) ->
    Diagram B
pixelf cfg (Aspect asp) xy f =
    pixel1 $ projectPixels asp (toPixels xy f cfg)

-- | a chart of text
textChart ::
    (Traversable f) =>
    [TextConfig] ->
    Aspect ->
    Rect Double ->
    [f (Text, Pair Double)] ->
    Diagram B
textChart defs (Aspect asp) r xyss =
    mconcat $
    zipWith texts defs $
    zipWith zip
    (fmap fst . toList <$> xyss)
    (scale r asp (fmap snd . toList <$> xyss))

-- | a chart of text scaled to its own range
textChart_ ::
    (Traversable f) =>
    [TextConfig] ->
    Aspect ->
    [f (Text, Pair Double)] ->
    Diagram B
textChart_ defs asp xyss =
    textChart defs asp (range $ fmap snd . toList <$> xyss) xyss

-- | A chart of arrows representing vectors at various positions
arrowChart ::
    ArrowConfig Double ->
    Aspect ->
    Rect Double ->
    [Arrow] ->
    Diagram B
arrowChart cfg (Aspect asp) r xs =
    arrow1 cfg $
    (\(Arrow d arr) ->
        Arrow (project r asp d) (project r asp arr)) <$> xs

-- | an arrow chart scaled to its own range
arrowChart_ ::
    ArrowConfig Double ->
    Aspect ->
    [Arrow] ->
    Diagram B
arrowChart_ cfg asp xs =
    arrowChart cfg asp (space (pos <$> xs)) xs

-- * axis rendering
-- | render with a chart configuration
withChart :: ( Foldable f ) =>
    ChartConfig ->
    (Aspect -> Rect Double -> [f (Pair Double)] -> Diagram B) ->
    [f (Pair Double)] ->
    Diagram B
withChart cfg renderer d = case cfg^.chartRange of
  Nothing -> 
      renderer (cfg^.chartAspect) (foldMap space d) d <>
      axes (chartRange .~ Just (foldMap space d) $ cfg)
  Just r ->
      combine (cfg ^. chartAspect)
      [ Chart renderer r d
      , Chart
        (\asp _ _ ->
           axes
           ( chartAspect .~ asp
           $ chartRange .~ Just r
           $ cfg))
        r
        []
      ]
 
-- | render a list of charts, taking into account each of their ranges
combine :: Aspect -> [Chart a] -> Diagram B
combine asp qcs = mconcat $
    (\(Chart c _ x) -> c asp rall x) <$> qcs
    where
      rall = fold $ (\(Chart _ r1 _) -> r1) <$> qcs

-- | render a set of axes based upon a ChartConfig
axes :: ( ) =>
    ChartConfig ->
    Diagram B
axes cfg =
    L.fold (L.Fold addTitle uptoLegend (pad $ cfg ^. chartPad)) (cfg ^. chartTitles)
  where
    addTitle x (tcfg, t) =
        beside (placeOutside (tcfg^.titlePlace)) x
          (title1 (cfg^.chartAspect) tcfg t)
    addLegend x lcfg = beside (placeOutside (lcfg^.legendPlace)) x $
        if (0 == length (lcfg^.legendChartType))
        then mempty
        else legend1 lcfg
    uptoLegend = L.fold (L.Fold addLegend uptoAxes identity) (cfg ^. chartLegends)
    uptoAxes = L.fold (L.Fold addAxis canvas identity) (cfg ^. chartAxes)
    canvas = rect_ (cfg ^. chartCanvas) asp
    (Aspect asp@(Ranges aspx aspy)) = cfg ^. chartAspect
    addAxis x acfg =
        beside (placeOutside (acfg^.axisPlace)) x (axis' (acfg ^. axisOrientation))
      where
        (Ranges rx ry) = fromMaybe one (cfg ^. chartRange)
        axis' X = (moveOriginTo (p2 ((-lower aspx)-width aspx/2,0)) $
                   axis acfg aspx rx)
        axis' Y = (moveOriginTo (p2 ((-lower aspy)-width aspy/2,0)) $
                   axis acfg aspy ry)

-- | Create a legend for a chart
legend1 :: LegendConfig -> Diagram B
legend1 cfg =
    pad (cfg^.legendOuterPad) $
    bound (cfg^.legendRect) 1 $
    pad (cfg^.legendInnerPad) $
    centerXY $
    vert (pad (cfg^.legendRowPad))
    (intersperse (strutY (cfg^.legendInnerSep)) $
      legend_ <$> (cfg^.legendChartType))
  where
    legend_ (LegendText c, t) = text_ c t
    legend_ (LegendGlyph c, t) =
        hori identity [ glyph_ c
                      , strutX (cfg^.legendSep) 
                      , text_ (cfg^.legendText) t]
    legend_ (LegendLine c l, t) =
        hori identity [ line_ c (Pair (Pair 0 0) (Pair l 0))
                      , strutX (cfg^.legendSep) 
                      , text_ (cfg^.legendText) t]
    legend_ (LegendGLine gc lcfg l, t) =
        hori identity [ (glyph_ gc `atop` line_ lcfg (Pair (Pair (-l) 0) (Pair l 0)))
                      , strutX (cfg^.legendSep) 
                      , text_ (cfg^.legendText) t]
    legend_ (LegendRect c s, t) =
        hori identity [ rect_ c (s*.one)
                      , strutX (cfg^.legendSep) 
                      , text_ (cfg^.legendText) t]
    legend_ (LegendArrow c s, t) =
        hori identity [ arrow_ c (Rect 0 s 0 0)
                      , strutX (cfg^.legendSep) 
                      , text_ (cfg^.legendText) t]
    legend_ (LegendPixel c s, t) =
        hori identity [ rect_ c (s*.one)
                      , strutX (cfg^.legendSep) 
                      , text_ (cfg^.legendText) t]

-- | Create a title for a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title1 :: Aspect -> TitleConfig -> Text -> Diagram B
title1 (Aspect (Ranges aspx aspy)) (TitleConfig textcfg a p s) t =
    placeGap p
    (textp (textAlignH .~ a $ textcfg) t (pos a p))
    s
  where
    pos AlignCenter _ = Pair 0 0
    pos AlignLeft PlaceTop = Pair (lower aspx) 0
    pos AlignLeft PlaceBottom = Pair (lower aspx) 0
    pos AlignLeft PlaceLeft = Pair 0 (lower aspy)
    pos AlignLeft PlaceRight = Pair 0 (upper aspy)
    pos AlignRight PlaceTop = Pair (upper aspx) 0
    pos AlignRight PlaceBottom = Pair (upper aspx) 0
    pos AlignRight PlaceLeft = Pair 0 (upper aspy)
    pos AlignRight PlaceRight = Pair 0 (lower aspy)

-- | render an axis, based on a chart aspect, and the axis range
axis :: ( ) =>
    AxisConfig ->
    Range Double ->
    Range Double ->
    Diagram B
axis cfg asp r = pad (cfg ^. axisPad) $ strut2 $ centerXY $
  atPoints
    (t <$> tickLocations)
    (labelled
     (cfg^.axisLabel)
     (glyph_ (cfg^.axisMark)) <$>
      tickLabels)
  `atop`
    arect (cfg^.axisOrientation)
  where
    arect X = rect_ (cfg ^. axisRect) (Ranges asp (Range 0 (cfg ^. axisRectHeight)))
    arect Y = rect_ (cfg ^. axisRect) (Ranges (Range 0 (cfg ^. axisRectHeight)) asp)
    strut2 x = beside (placeOutside (cfg^.axisPlace)) x $ strut1 (cfg ^. axisGap)
    strut1 = case cfg ^. axisOrientation of
      X -> strutY
      Y -> strutX
    t = case cfg ^. axisPlace of
      PlaceBottom -> \x -> p2
        (x, (-0.5*(cfg ^. axisMark ^. glyphSize)) +
          (cfg^.axisRectHeight) +
          (cfg^.axisMarkStart) )
      PlaceLeft -> \y -> p2
        ((-0.5*(cfg ^. axisMark ^. glyphSize)) +
         (cfg^.axisRectHeight) +
         (cfg^.axisMarkStart), y)
      PlaceTop -> \x -> p2
        (x, (0.5*(cfg ^. axisMark ^. glyphSize)) + (cfg^.axisMarkStart) )
      PlaceRight -> \y -> p2
        ((0.5*(cfg ^. axisMark ^. glyphSize)) + (cfg^.axisMarkStart), y)
    ticks0 = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> gridSensible OuterPos r n
      TickExact n -> grid OuterPos r n
      TickLabels _ -> []
      TickPlaced xs -> fst <$> xs
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound _ -> project r asp <$> ticks0
      TickExact _ -> project r asp <$> ticks0
      TickLabels ls ->
          project
          (Range 0 (fromIntegral $ length ls))
          asp <$>
          ((\x -> x - 0.5) . fromIntegral <$> [1..length ls])
      TickPlaced _ -> project r asp <$> ticks0
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound _ -> precision 0 ticks0
      TickExact _ -> precision 3 ticks0
      TickLabels ls -> ls
      TickPlaced xs -> snd <$> xs

precision :: Int -> [Double] -> [Text]
precision n0 xs | (foldr max 0 xs) < 0.01 = precLoop (Formatting.expt) n0 xs
                | (foldr max 0 xs) > 100000 = precLoop (Formatting.expt) n0 xs
                | (foldr max 0 xs) > 1000 =
                      precLoopInt (const Formatting.commas) n0 (floor <$> xs)
                | otherwise = precLoop fixed n0 xs
  where
    precLoop f n xs' = let s = sformat (f n) <$> xs' in
     if s == nub s then s else (precLoop f (n+1) xs')
    precLoopInt f n xs' = let s = sformat (f n) <$> xs' in
     if s == nub s then s else (precLoopInt f (n+1) xs')

{-
histCompareChart :: [(Rect Double)] -> [(Rect Double)] -> Diagram B' a
histCompareChart h1 h2 =
    let deltah = zipWith (\(Rect x y z w) (Rect _ _ _ w') -> Rect x y z (w-w')) h1 h2
        mainAspect = Aspect (Rect -0.75 0.75 -0.5 0.5)
        botAspect = Aspect (Rect -0.75 0.75 -0.2 0.2)
    in
      pad 1.1 $
        beside (r2 (0,-1)) (histChart_
        [ def
        , rectBorderColor .~ UColor 0 0 0 0
        $ rectColor .~ UColor 0.333 0.333 0.333 0.1
        $ def ]
        mainAspect [h1,h2] <>
        axes (ChartConfig 1.1
              [def]
              (Just (fold $ fold [abs <$> h1,abs <$> h2]))
              mainAspect (fromColor transparent)
              True))
        (histChart_
        [ rectBorderColor .~ UColor 0 0 0 0
        $ rectColor .~ UColor 0.888 0.333 0.333 0.8
        $ def ] botAspect [abs <$> deltah] <>
        axes (ChartConfig 1.1
              [ axisLabelText .~
                ( textBottom .~ 0.65 $
                  textRight .~ 1 $
                  def) $
                axisOrientation .~ Y $
                axisPlacement .~ AxisLeft $
                def
              ]
              (Just (fold $ abs <$> deltah))
              botAspect (fromColor transparent)
              True))
-}

fileSvg ∷ FilePath → (Double, Double) → Diagram SVG → IO ()
fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

-- | Pair orphans
instance R1 Pair where
    _x f (Pair a b) = (`Pair` b) <$> f a

instance R2 Pair where
    _y f (Pair a b) = Pair a <$> f b
    _xy f p = fmap (\(V2 a b) -> Pair a b) . f . (\(Pair a b) -> V2 a b) $ p
 
p_ :: (R2 r) => r Double -> Point V2 Double
p_ r = curry p2 (r^._x) (r^._y)

r_ :: R2 r => r a -> V2 a
r_ r = V2 (r^._x) (r^._y) 

-- | avoiding the scaleX zero throw
eps :: N [Point V2 Double]
eps = 1e-8

scaleX :: (N t ~ Double, Transformable t, R2 (V t), Diagrams.Additive (V t)) =>
    Double -> t -> t
scaleX s = Diagrams.scaleX (if s==zero then eps else s)

scaleY :: (N t ~ Double, Transformable t, R2 (V t), Diagrams.Additive (V t)) =>
    Double -> t -> t
scaleY s = Diagrams.scaleY (if s==zero then eps else s)
