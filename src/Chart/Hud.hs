{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Hud (Heads up display) is a collective noun for axes, titles & legends
--
-- todo: refactor me please. A hud for a chart uses 'beside' to combine elements, and this restricts the hud to the outside of the chart canvas.  This tends to make hud elements (such as gridlines) harder to implement than they should be.
module Chart.Hud
  ( HudOptions(HudOptions)
  , hud
  , withHud
  , placeOutside
  , placeGap
  , TickStyle(..)
  , precision
  , AxisOptions(AxisOptions)
  , defXAxis
  , defYAxis
  , axis
  , AutoOptions(AutoOptions)
  , adjustAxis
  , axisSane
  , computeTicks
  , TitleOptions(TitleOptions)
  , title
  , LegendType(..)
  , LegendOptions(LegendOptions)
  , legend
  , GridStyle(..)
  , GridOptions(GridOptions)
  , GridPos(..)
  , gridPos
  , defXGrid
  , defYGrid
  , gridl
  ) where

import Chart.Arrow
import Chart.Core
import Chart.Glyph
import Chart.Line (LineOptions(LineOptions), lines, oneline)
import Chart.Rect
import Chart.Text
import qualified Control.Foldl as L
import Data.List (nub)
import Data.Ord (max)
import Diagrams.Prelude
       hiding (Color, D, (*.), (<>), project, width, zero, (<>))
import qualified Diagrams.TwoD.Size as D
import Formatting
import NumHask.Pair
import NumHask.Prelude hiding (max)
import NumHask.Range
import NumHask.Rect
import NumHask.Space
import Data.Generics.Labels()
import Diagrams.Backend.SVG (SVG)

-- | Various options for a hud.
--
-- Defaults to the classical x- and y-axis, a sixbyfour aspect, no titles and no legends
data HudOptions = HudOptions
  { outerPad :: Double
  , axes :: [AxisOptions]
  , grids :: [GridOptions]
  , titles :: [(TitleOptions, Text)]
  , legends :: [LegendOptions]
  , range :: Maybe (Rect Double)
  , aspect :: Rect Double
  , canvas :: RectOptions
  } deriving (Show, Generic)

instance Default HudOptions where
  def = HudOptions 1.1 [defXAxis, defYAxis] [] [] []
      Nothing sixbyfour clear

-- | Create a hud.
--
-- > hud def
--
-- ![hud example](other/hudExample.svg)
--
hud :: () => HudOptions -> Chart b
hud (HudOptions p as gs ts ls mr ar@(Ranges ax ay) can) =
  mconcat ((\x -> gridl x ar r) <$> gs) <>
  L.fold (L.Fold addTitle uptoLegend (pad p)) ts
  where
    r = fromMaybe one mr
    addTitle x (topts, t) =
      beside (placeOutside (topts ^. #place)) x (title ar topts t)
    addLegend x lopts =
      beside (placeOutside (lopts ^. #place)) x $
      if 0 == length (lopts ^. #chartType)
        then mempty
        else (\x' ->
                moveTo (p_ $ pos' (lopts ^. #align) (lopts ^. #place) x') x') $
             legend lopts
    pos' AlignCenter _ _ = Pair 0 0
    pos' AlignLeft PlaceTop x = Pair (lower ax - 0.5 * D.width x) 0
    pos' AlignLeft PlaceBottom x = Pair (lower ax - 0.5 * D.width x) 0
    pos' AlignLeft PlaceLeft x = Pair 0 (lower ay - 0.5 * D.height x)
    pos' AlignLeft PlaceRight x = Pair 0 (upper ay - 0.5 * D.height x)
    pos' AlignRight PlaceTop x = Pair (upper ax + 0.5 * D.width x) 0
    pos' AlignRight PlaceBottom x = Pair (upper ax + 0.5 * D.width x) 0
    pos' AlignRight PlaceLeft x = Pair 0 (upper ay + 0.5 * D.height x)
    pos' AlignRight PlaceRight x = Pair 0 (lower ay + 0.5 * D.height x)
    uptoLegend = L.fold (L.Fold addLegend uptoAxes identity) ls
    uptoAxes = L.fold (L.Fold addAxis canvas' identity) as
    canvas' = rect_ can ar
    addAxis x aopts =
      case aopts ^. #orientation of
        Hori -> beside (placeOutside (aopts ^. #place)) x (axis aopts ax rx)
        Vert -> beside (placeOutside (aopts ^. #place)) x (axis aopts ay ry)
      where
        (Ranges rx ry) = fromMaybe one mr

-- | create a chart with a hud from data (using the data range)
--
-- > withHudExample :: Chart b
-- > withHudExample = withHud hopts (lineChart lopts) ls
-- >     where
-- >       hopts = def &
-- >         #titles .~ [(def,"withHud Example")] &
-- >         #legends .~ [def & #chartType .~ zipWith (\x y ->
-- >                     (LegendLine x 0.05, y)) lopts ["line1", "line2", "line3"]]
--
-- ![withHud example](other/withHudExample.svg)
--
withHud ::
     (Foldable f)
  => HudOptions
  -> (Rect Double -> Rect Double -> [f (Pair Double)] -> Chart b)
  -> [f (Pair Double)]
  -> Chart b
withHud opts renderer d =
  case opts ^. #range of
    Nothing ->
      renderer (opts ^. #aspect) (foldMap space d) d <>
      hud (#range .~ Just (foldMap space d) $ opts)
    Just r ->
      combine
        (opts ^. #aspect)
        [ UChart renderer r d
        , UChart
            (\asp _ _ -> hud (#aspect .~ asp $ #range .~ Just r $ opts))
            r
            []
        ]

-- | Direction to place stuff on the outside of the built-up hud
placeOutside :: Num n => Place -> V2 n
placeOutside pl =
  case pl of
    PlaceBottom -> r2 (0, -1)
    PlaceTop -> r2 (0, 1)
    PlaceLeft -> r2 (-1, 0)
    PlaceRight -> r2 (1, 0)

-- | A gap to add when placing elements.
placeGap ::
     (Monoid m, Semigroup m, Ord n, Floating n)
  => Place
  -> n
  -> QDiagram b V2 n m
  -> QDiagram b V2 n m
placeGap pl s x = beside (placeOutside pl) (strut' pl s) x
  where
    strut' PlaceTop = strutY
    strut' PlaceBottom = strutY
    strut' PlaceLeft = strutX
    strut' PlaceRight = strutX

-- | Axes are somewhat complicated.  For instance, they contain a range within which tick marks need to be supplied or computed.
data AxisOptions = AxisOptions
  { outerPad :: Double
  , orientation :: Orientation
  , place :: Place
  , rect :: RectOptions
  , rectHeight :: Double
  , mark :: GlyphOptions
  , markStart :: Double
  , gap :: Double -- distance of axis from plane
  , label :: LabelOptions
  , tickStyle :: TickStyle
  } deriving (Show, Generic)

-- | default X axis
defXAxis :: AxisOptions
defXAxis =
  AxisOptions
    1
    Hori
    PlaceBottom
    (RectOptions 0 transparent (withOpacity black 0.1))
    0.02
    (GlyphOptions 0.03 transparent (withOpacity black 0.6) 0.005 (VLine 1.0))
    0
    0.04
    (LabelOptions
       (TextOptions 0.08 AlignCenter AlignMid (withOpacity black 0.6) EvenOdd 0 Lin2)
       (Pair 0 -1)
       0.015)
    (TickRound 8)

-- | default Y axis
defYAxis :: AxisOptions
defYAxis =
  AxisOptions
    1
    Vert
    PlaceLeft
    (RectOptions 0 transparent (withOpacity black 0.1))
    0.02
    (GlyphOptions 0.03 transparent (withOpacity black 0.6) 0.005 (HLine 1.0))
    0
    0.04
    (LabelOptions
       (TextOptions 0.08 AlignCenter AlignMid (withOpacity black 0.6) EvenOdd 0 Lin2)
       (Pair -1 0)
       0.015)
    (TickRound 8)

instance Default AxisOptions where
  def = defXAxis

-- | create an axis, based on AxisOptions, a physical aspect, and a range
--
-- Under-the-hood, the axis function has gone through many a refactor, and still has a ways to go.  A high degree of technical debt tends to acrue here.
--
-- > axisExample :: Chart b
-- > axisExample = axis aopts one (Range 0 100000)
-- >   where
-- >     aopts :: AxisOptions b
-- >     aopts =
-- >         #label . #text %~
-- >         ((#rotation .~ -45) .
-- >          (#size .~ 0.06) .
-- >          (#alignH .~ AlignLeft)) $
-- >         #gap .~ 0.0001 $
-- >         def
--
-- ![axis example](other/axisExample.svg)
--
axis :: () => AxisOptions -> Range Double -> Range Double -> Chart b
axis opts asp r =
  mo $
  pad (opts ^. #outerPad) $
  astrut $
  atPoints
    (pl <$> tickLocations)
    ((\x -> labelled (opts ^. #label) x (glyph_ (opts ^. #mark))) <$> tickLabels)
    `atop`
  arect (opts ^. #orientation )
  where
    mo = moveOriginTo (p2 ((-lower asp) - width asp / 2, 0))
    arect Hori =
      rect_ (opts ^. #rect) (Ranges asp (Range 0 (opts ^. #rectHeight)))
    arect Vert =
      rect_ (opts ^. #rect) (Ranges (Range 0 (opts ^. #rectHeight)) asp)
    astrut =
      beside (placeOutside (opts ^. #place))
        (case opts ^. #orientation of
           Hori -> strutY (opts ^. #gap)
           Vert -> strutX (opts ^. #gap))
    pl =
      let gs = (opts ^. #mark . #size)
      in case opts ^. #place of
           PlaceBottom ->
             \x ->
               p2 (x, (-0.5 * gs) + opts ^. #rectHeight + opts ^. #markStart)
           PlaceLeft ->
             \y ->
               p2 ((-0.5 * gs) + opts ^. #rectHeight + opts ^. #markStart, y)
           PlaceTop -> \x -> p2 (x, (0.5 * gs) + opts ^. #markStart)
           PlaceRight -> \y -> p2 ((0.5 * gs) + opts ^. #markStart, y)
    (tickLocations, tickLabels) = computeTicks opts r asp

-- | options for prettifying axis decorations
data AutoOptions =
  AutoOptions
  { maxXRatio :: Double
  , maxYRatio :: Double
  , angledRatio :: Double
  , allowDiagonal :: Bool
  } deriving (Show, Generic)

instance Default AutoOptions where
  def = AutoOptions 0.08 0.06 0.12 True

-- | adjust an axis for sane font sizes etc
adjustAxis :: AutoOptions -> Range Double -> Range Double ->
  AxisOptions -> AxisOptions
adjustAxis (AutoOptions mrx ma mry ad) asp r opts = case opts ^. #orientation of
  Hori -> case ad of
    False -> (#label . #text . #size %~ (/adjustSizeX)) opts
    True ->
        case adjustSizeX > one of
          True -> (#label . #text . #rotation .~ (-45)) . (#label . #text . #alignH .~ AlignLeft) $ (#label . #text . #size %~ (/adjustSizeA)) opts
          False -> (#label . #text . #size %~ (/adjustSizeA)) opts
  Vert -> (#label . #text . #size %~ (/adjustSizeY)) opts

  where
        tickl = snd (computeTicks opts r asp)
        maxWidth =
          maximum $
          (\x ->
              D.width
              (text_ (opts ^. #label . #text) x :: QDiagram SVG V2 Double Any))
              <$> tickl
        maxHeight =
          maximum $
          (\x ->
              D.height
              (text_ (opts ^. #label . #text) x :: QDiagram SVG V2 Double Any))
              <$> tickl
        adjustSizeX = maximum [(maxWidth / (upper asp - lower asp)) / mrx, one]
        adjustSizeY = maximum [(maxHeight / (upper asp - lower asp)) / mry, one]
        adjustSizeA = maximum [(maxHeight / (upper asp - lower asp)) / ma, one]

-- | create an axis, with adjustment to axis options if needed
axisSane :: () => AutoOptions -> AxisOptions -> Range Double -> Range Double -> Chart b
axisSane ao opts asp r =
    axis (adjustAxis ao asp r opts) asp r

-- | compute tick values and labels given options and ranges
computeTicks :: AxisOptions -> Range Double -> Range Double -> ([Double], [Text])
computeTicks opts r asp =
    case opts ^. #tickStyle of
      TickNone -> ([], [])
      TickRound n -> (project r asp <$> ticks0, precision 0 ticks0)
        where ticks0 = gridSensible OuterPos r n
      TickExact n -> (project r asp <$> ticks0, precision 3 ticks0)
        where ticks0 = grid OuterPos r n
      TickLabels ls ->
          ( project (Range 0 (fromIntegral $ length ls)) asp <$>
            ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
          , ls)
      TickPlaced xs -> (project r asp . fst <$> xs, snd <$> xs)

-- | Style of tick marks on an axis.
data TickStyle
  = TickNone -- ^ no ticks on axis
  | TickLabels [Text] -- ^ specific labels
  | TickRound Int -- ^ sensibly rounded ticks and a guide to how many
  | TickExact Int -- ^ exactly n equally spaced ticks
  | TickPlaced [(Double, Text)] -- ^ specific labels and placement
  deriving (Show, Generic)

-- | Provide formatted text for a list of numbers so that they are just distinguished.  'precision 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figues.
precision :: Int -> [Double] -> [Text]
precision n0 xs
  | foldr max 0 xs < 0.01 = precLoop Formatting.expt n0 xs
  | foldr max 0 xs > 100000 = precLoop Formatting.expt n0 xs
  | foldr max 0 xs > 1000 =
    precLoopInt (const Formatting.commas) n0 (floor <$> xs)
  | otherwise = precLoop fixed n0 xs
  where
    precLoop f n xs' =
      let s = sformat (f n) <$> xs'
      in if s == nub s
           then s
           else precLoop f (n + 1) xs'
    precLoopInt f n xs' =
      let s = sformat (f n) <$> xs'
      in if s == nub s
           then s
           else precLoopInt f (n + 1) xs'

-- | Options for titles.  Defaults to center aligned, and placed at Top of the hud
data TitleOptions = TitleOptions
  { text :: TextOptions
  , align :: AlignH
  , place :: Place
  , gap :: Double
  } deriving (Show, Generic)

instance Default TitleOptions where
  def =
    TitleOptions
      (TextOptions 0.12 AlignCenter AlignMid (withOpacity black 0.6) EvenOdd 0 Lin2)
      AlignCenter
      PlaceTop
      0.04

-- | Create a title for a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: Rect Double -> TitleOptions -> Text -> Chart b
title (Ranges aspx aspy) (TitleOptions textopts a p s) t =
  placeGap p s (positioned (pos a p) (text_ ( #alignH .~ a $ textopts) t))
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

-- | LegendType reuses all the various chart option types to help formulate a legend
data LegendType
  = LegendText TextOptions
  | LegendGlyph GlyphOptions
  | LegendLine LineOptions
               Double
  | LegendGLine GlyphOptions
                LineOptions
                Double
  | LegendRect RectOptions
               Double
  | LegendArrow (ArrowOptions Double)
                Double
  | LegendPixel RectOptions
                Double
    deriving (Show, Generic)

-- | Legend options. todo: allow for horizontal concatenation.
data LegendOptions = LegendOptions
  { chartType :: [(LegendType, Text)]
  , innerPad :: Double
  , innerSep :: Double
  , gap :: Double
  , rowPad :: Double
  , place :: Place
  , align :: AlignH
  , sep :: Double
  , canvasRect :: RectOptions
  , text :: TextOptions
  } deriving (Show, Generic)

instance Default LegendOptions where
  def =
    LegendOptions
      []
      1.1
      0.03
      0.05
      1
      PlaceRight
      AlignRight
      0.02
      (RectOptions 0.002 (withOpacity black 0.2) transparent)
      (TextOptions 0.07 AlignCenter AlignMid
       (withOpacity black 0.63) EvenOdd 0 Lin2)

-- | Create a legend based on a LegendOptions
--
-- > legendExample :: Chart b
-- > legendExample = legend $ def {legendChartType=legends}
-- >     where
-- >       legends =
-- >           [ (LegendText def, "legend")] <>
-- >           [ (LegendPixel (blob (withOpacity blue 0.4)) 0.05, "pixel")] <>
-- >           -- [ (LegendArrow (def {arrowMinStaffWidth=0.01,
-- >           --                     arrowMinHeadLength=0.03}) 0.05, "arrow")] <>
-- >           [ (LegendRect def 0.05, "rect")] <>
-- >           [ (LegendGLine def def 0.10, "glyph+line")] <>
-- >           [ (LegendGlyph def, "just a glyph")] <>
-- >           (zipWith (\x y -> (LegendLine x 0.05, y))
-- >            lopts ["short", "much longer name", "line 3"])
--
--
-- ![legend example](other/legendExample.svg)
--
legend :: LegendOptions -> Chart b
legend opts =
  placeGap (opts ^. #place) (opts ^. #gap) $
  bound (opts ^. #canvasRect) 1 $
  pad (opts ^. #innerPad) $
  centerXY $
  vert
    (pad (opts ^. #rowPad))
    (intersperse (strutY (opts ^. #innerSep)) $
     legend__ <$> opts ^. #chartType)
  where
    legend__ (LegendText c, t) = text_ c t
    legend__ (LegendGlyph c, t) =
      hori
        identity
        [glyph_ c, strutX (opts ^. #sep), text_ (opts ^. #text) t]
    legend__ (LegendLine c l, t) =
      hori
        identity
        [ oneline c (Pair (Pair 0 0) (Pair l 0))
        , strutX (opts ^. #sep)
        , text_ (opts ^. #text) t
        ]
    legend__ (LegendGLine gc lopts l, t) =
      hori
        identity
        [ glyph_ gc `atop` oneline lopts (Pair (Pair (-l) 0) (Pair l 0))
        , strutX (opts ^. #sep)
        , text_ (opts ^. #text) t
        ]
    legend__ (LegendRect c s, t) =
      hori
        identity
        [rect_ c (s *. one), strutX (opts ^. #sep), text_ (opts ^. #text) t]
    legend__ (LegendArrow c s, t) =
      hori
        identity
        [ arrows c [Arrow zero (s *. one), Arrow (s *. one) zero]
        , strutX (opts ^. #sep)
        , text_ (opts ^. #text) t
        ]
    legend__ (LegendPixel c s, t) =
      hori
        identity
        [rect_ c (s *. one), strutX (opts ^. #sep), text_ (opts ^. #text) t]

data GridPos = GridOuterPos | GridInnerPos | GridLowerPos | GridUpperPos | GridMidPos deriving (Show, Generic, Eq)

gridPos :: GridPos -> Pos
gridPos GridOuterPos = OuterPos
gridPos GridInnerPos = InnerPos
gridPos GridLowerPos = LowerPos
gridPos GridUpperPos = UpperPos
gridPos GridMidPos = MidPos

-- | Style of grid lines
data GridStyle
  = GridNone -- ^ no ticks on axis
  | GridRound GridPos Int -- ^ sensibly rounded line placement and a guide to how many
  | GridExact GridPos Int -- ^ exactly n lines using Pos
  | GridPlaced [Double] -- ^ specific line placement
  deriving (Show, Generic)

-- | Options for gridlines.
data GridOptions = GridOptions
  { gridOrientation :: Orientation
  , gridStyle :: GridStyle
  , gridLine :: LineOptions
  } deriving (Show, Generic)

defXGrid :: GridOptions
defXGrid =
    GridOptions
    Hori 
    (GridRound GridOuterPos 10)
    (LineOptions 0.002 ublue)

defYGrid :: GridOptions
defYGrid =
    GridOptions
    Vert
    (GridRound GridOuterPos 10)
    (LineOptions 0.002 ublue)

instance Default GridOptions where
  def = defXGrid

-- | Create a grid line for a chart.
gridl :: GridOptions -> Rect Double -> Rect Double -> Chart b
gridl gopt (Ranges aspx aspy) (Ranges rx ry) = ls
  where
    ls = mconcat $ lines (gridLine gopt) <$> (l1d <$> lineLocations)
    lineLocations =
        case gridStyle gopt of
          GridNone -> []
          GridRound p n -> project r0 asp0 <$> gridSensible (gridPos p) r0 n
          GridExact p n -> project r0 asp0 <$> grid (gridPos p) r0 n
          GridPlaced xs -> project r0 asp0 <$> xs
    (asp0, r0) =
        case gridOrientation gopt of
          Vert -> (aspx, rx)
          Hori -> (aspy, ry)
    l1d =
        case gridOrientation gopt of
          Hori -> \y -> [Pair (lower aspx) y, Pair (upper aspx) y]
          Vert -> \x -> [Pair x (lower aspy), Pair x (upper aspy)]
