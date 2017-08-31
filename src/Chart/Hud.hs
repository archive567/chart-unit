{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

-- | collective noun for axes, titles & legends
module Chart.Hud
  ( HudOptions(..)
  , withHud
  , hud
  , Orientation(..)
  , Place(..)
  , placeOutside
  , placeGap
  , TickStyle(..)
  , precision
  , AxisOptions(..)
  , defXAxis
  , defYAxis
  , axis
  , TitleOptions(..)
  , title
  , LegendType(..)
  , LegendOptions(..)
  , legend
  ) where

import Chart.Arrow
import Chart.Core
import Chart.Glyph
import Chart.Line
import Chart.Rect
import Chart.Text
import qualified Control.Foldl as L
import Data.List (nub)
import Data.Ord (max)
import Diagrams.Prelude
       hiding (Color, D, (*.), (<>), project, width, zero)
import qualified Diagrams.TwoD.Size as D
import Formatting
import NumHask.Pair
import NumHask.Prelude hiding (max)
import NumHask.Range
import NumHask.Rect
import NumHask.Space

-- | various options for a hud
-- Defaults to an x- and y-axis, sixbyfour aspect, no titles and no legends
data HudOptions b = HudOptions
  { hudPad :: Double
  , hudAxes :: [AxisOptions b]
  , hudTitles :: [(TitleOptions, Text)]
  , hudLegends :: [LegendOptions b]
  , hudRange :: Maybe (Rect Double)
  , hudAspect :: Aspect
  , hudCanvas :: RectOptions
  }

instance Default (HudOptions b) where
  def = HudOptions 1.1 [defXAxis, defYAxis] [] [] Nothing sixbyfour clear

-- | create a hud based upon HudOptions
--
-- > hud def
--
-- ![hud example](other/hudExample.svg)
--
hud :: () => HudOptions b -> Chart b
hud (HudOptions p axes titles legends mr asp@(Aspect ar@(Ranges ax ay)) can) =
  L.fold (L.Fold addTitle uptoLegend (pad p)) titles
  where
    addTitle x (topts, t) =
      beside (placeOutside (titlePlace topts)) x (title asp topts t)
    addLegend x lopts =
      beside (placeOutside (legendPlace lopts)) x $
      if 0 == length (legendChartType lopts)
        then mempty
        else (\x' ->
                moveTo (p_ $ pos' (legendAlign lopts) (legendPlace lopts) x') x') $
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
    uptoLegend = L.fold (L.Fold addLegend uptoAxes identity) legends
    uptoAxes = L.fold (L.Fold addAxis canvas identity) axes
    canvas = rect_ can ar
    addAxis x aopts =
      case axisOrientation aopts of
        Hori -> beside (placeOutside (axisPlace aopts)) x (axis aopts ax rx)
        Vert -> beside (placeOutside (axisPlace aopts)) x (axis aopts ay ry)
      where
        (Ranges rx ry) = fromMaybe one mr

-- | create a chart with a hud from data (using the data range)
--
-- > withHudExample :: Chart b
-- > withHudExample = withHud hopts (lineChart lopts) ls
-- >     where
-- >       hopts = def {hudTitles=[(def,"withHud Example")],
-- >                    hudLegends=[def {legendChartType=zipWith (\x y -> (LegendLine x 0.05, y)) lopts ["line1", "line2", "line3"]}]}
--
-- ![withHud example](other/withHudExample.svg)
--
withHud ::
     (Foldable f)
  => HudOptions b
  -> (Aspect -> Rect Double -> [f (Pair Double)] -> Chart b)
  -> [f (Pair Double)]
  -> Chart b
withHud opts renderer d =
  case hudRange opts of
    Nothing ->
      renderer (hudAspect opts) (foldMap space d) d <>
      hud (opts {hudRange = Just (foldMap space d)})
    Just r ->
      combine
        (hudAspect opts)
        [ UChart renderer r d
        , UChart
            (\asp _ _ -> hud (opts {hudAspect = asp, hudRange = Just r}))
            r
            []
        ]

-- | placement of hud elements around (what is implicity but maybe shouldn't just be) a rectangular canvas
data Place
  = PlaceLeft
  | PlaceRight
  | PlaceTop
  | PlaceBottom
  deriving (Eq, Show)

-- | direction to place stuff on the outside of the built-up chart
placeOutside :: Num n => Place -> V2 n
placeOutside pl =
  case pl of
    PlaceBottom -> r2 (0, -1)
    PlaceTop -> r2 (0, 1)
    PlaceLeft -> r2 (-1, 0)
    PlaceRight -> r2 (1, 0)

-- | a strut to add to a placement
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

-- | orientation for a hud element.  Watch this space for curvature!
data Orientation
  = Hori
  | Vert

-- | axes are somewhat complicated.  For instance, they contain a range within which tick marks need to be supplied or computed.
data AxisOptions b = AxisOptions
  { axisPad :: Double
  , axisOrientation :: Orientation
  , axisPlace :: Place
  , axisRect :: RectOptions
  , axisRectHeight :: Double
  , axisMark :: GlyphOptions b
  , axisMarkStart :: Double
  , axisGap :: Double -- distance of axis from plane
  , axisLabel :: LabelOptions
  , axisTickStyle :: TickStyle
  }

-- | default X axis
defXAxis :: AxisOptions b
defXAxis =
  AxisOptions
    1
    Hori
    PlaceBottom
    (RectOptions 0 transparent (withOpacity black 0.1))
    0.02
    (GlyphOptions 0.03 transparent (withOpacity black 0.6) 0.005 (vline_ 1))
    0
    0.04
    (LabelOptions
       (TextOptions 0.08 AlignCenter (withOpacity black 0.6) EvenOdd 0)
       (Pair 0 -1)
       0.015)
    (TickRound 8)

-- | default Y axis
defYAxis :: AxisOptions b
defYAxis =
  AxisOptions
    1
    Vert
    PlaceLeft
    (RectOptions 0 transparent (withOpacity black 0.1))
    0.02
    (GlyphOptions 0.03 transparent (withOpacity black 0.6) 0.005 (hline_ 1))
    0
    0.04
    (LabelOptions
       (TextOptions 0.08 AlignCenter (withOpacity black 0.6) EvenOdd 0)
       (Pair -1 0)
       0.015)
    (TickRound 8)

instance Default (AxisOptions b) where
  def = defXAxis

-- | create an axis, based on AxisOptions, a physical aspect, and a range
--
-- Under-the-hood, the axis function has gone through many a re-factor, and still has a ways to go.  A high degree of technical debt tends to acrue here.
--
-- > axisExample :: Chart b
-- > axisExample = axis aopts one (Range 0 100000)
-- >   where
-- >     aopts = def {axisLabel=(axisLabel def) {labelGap=0.0001,labelText=(labelText (axisLabel def)) {textSize=0.06,textAlignH=AlignLeft,textRotation=(-45)}}}
--
-- ![axis example](other/axisExample.svg)
--
axis :: () => AxisOptions b -> Range Double -> Range Double -> Chart b
axis opts asp r =
  mo $
  pad (axisPad opts) $
  astrut $
  centerXY $
  atPoints
    (pl <$> tickLocations)
    ((\x -> labelled (axisLabel opts) x (glyph_ (axisMark opts))) <$> tickLabels) `atop`
  arect (axisOrientation opts)
  where
    mo = moveOriginTo (p2 ((-lower asp) - width asp / 2, 0))
    arect Hori =
      rect_ (axisRect opts) (Ranges asp (Range 0 (axisRectHeight opts)))
    arect Vert =
      rect_ (axisRect opts) (Ranges (Range 0 (axisRectHeight opts)) asp)
    astrut =
      beside (placeOutside (axisPlace opts))
        (case axisOrientation opts of
           Hori -> strutY (axisGap opts)
           Vert -> strutX (axisGap opts))
    pl =
      let gs = glyphSize (axisMark opts)
      in case axisPlace opts of
           PlaceBottom ->
             \x ->
               p2 (x, (-0.5 * gs) + axisRectHeight opts + axisMarkStart opts)
           PlaceLeft ->
             \y ->
               p2 ((-0.5 * gs) + axisRectHeight opts + axisMarkStart opts, y)
           PlaceTop -> \x -> p2 (x, (0.5 * gs) + axisMarkStart opts)
           PlaceRight -> \y -> p2 ((0.5 * gs) + axisMarkStart opts, y)
    (tickLocations, tickLabels) =
      case axisTickStyle opts of
        TickNone -> ([], [])
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
        TickRound n -> (project r asp <$> ticks0, precision 0 ticks0)
          where ticks0 = gridSensible OuterPos r n
        TickExact n -> (project r asp <$> ticks0, precision 3 ticks0)
          where ticks0 = grid OuterPos r n
        TickLabels ls ->
          ( project (Range 0 (fromIntegral $ length ls)) asp <$>
            ((\x -> x - 0.5) . fromIntegral <$> [1 .. length ls])
          , ls)
        TickPlaced xs -> (project r asp . fst <$> xs, snd <$> xs)

-- | style of tick marks on an axis
data TickStyle
  = TickNone -- ^ no ticks on axis
  | TickLabels [Text] -- ^ specific labels
  | TickRound Int -- ^ sensibly rounded ticks and a guide to how many
  | TickExact Int -- ^ exactly n equally spaced ticks
  | TickPlaced [(Double, Text)] -- ^ specific labels and placement

-- | provide a formatted text from a list of numbers so that they are just distinguished.  'precision 2 ticks' means give the tick labels as much precision as is needed for them to be distinguished, but with at least 2 significant figues.
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

-- | options for titles.  Defaults to center aligned, and placed at Top of the hud
data TitleOptions = TitleOptions
  { titleText :: TextOptions
  , titleAlign :: AlignH
  , titlePlace :: Place
  , titleGap :: Double
  }

instance Default TitleOptions where
  def =
    TitleOptions
      (TextOptions 0.12 AlignCenter (withOpacity black 0.6) EvenOdd 0)
      AlignCenter
      PlaceTop
      0.04

-- | Create a title for a chart. The logic used to work out placement is flawed due to being able to freely specify text rotation.  It works for specific rotations (Top, Bottom at 0, Left at 90, Right @ 270)
title :: Aspect -> TitleOptions -> Text -> Chart b
title (Aspect (Ranges aspx aspy)) (TitleOptions textopts a p s) t =
  placeGap p s (positioned (pos a p) (text_ (textopts {textAlignH = a}) t))
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

-- | the LegendType reuses all the various chart option types to help formulate a legend
data LegendType b
  = LegendText TextOptions
  | LegendGlyph (GlyphOptions b)
  | LegendLine LineOptions
               Double
  | LegendGLine (GlyphOptions b)
                LineOptions
                Double
  | LegendRect RectOptions
               Double
  | LegendArrow (ArrowOptions Double)
                Double
  | LegendPixel RectOptions
                Double

-- | legend options. The main missing piece here is to allow for horizontal concatenation.
data LegendOptions b = LegendOptions
  { legendChartType :: [(LegendType b, Text)]
  , legendInnerPad :: Double
  , legendInnerSep :: Double
  , legendGap :: Double
  , legendRowPad :: Double
  , legendPlace :: Place
  , legendAlign :: AlignH
  , legendSep :: Double
  , legendRect :: RectOptions
  , legendText :: TextOptions
  }

instance Default (LegendOptions b) where
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
      (TextOptions 0.07 AlignCenter (withOpacity black 0.63) EvenOdd 0)

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
legend :: LegendOptions b -> Chart b
legend opts =
  placeGap (legendPlace opts) (legendGap opts) $
  bound (legendRect opts) 1 $
  pad (legendInnerPad opts) $
  centerXY $
  vert
    (pad (legendRowPad opts))
    (intersperse (strutY (legendInnerSep opts)) $
     legend__ <$> legendChartType opts)
  where
    legend__ (LegendText c, t) = text_ c t
    legend__ (LegendGlyph c, t) =
      hori
        identity
        [glyph_ c, strutX (legendSep opts), text_ (legendText opts) t]
    legend__ (LegendLine c l, t) =
      hori
        identity
        [ oneline c (Pair (Pair 0 0) (Pair l 0))
        , strutX (legendSep opts)
        , text_ (legendText opts) t
        ]
    legend__ (LegendGLine gc lopts l, t) =
      hori
        identity
        [ glyph_ gc `atop` oneline lopts (Pair (Pair (-l) 0) (Pair l 0))
        , strutX (legendSep opts)
        , text_ (legendText opts) t
        ]
    legend__ (LegendRect c s, t) =
      hori
        identity
        [rect_ c (s *. one), strutX (legendSep opts), text_ (legendText opts) t]
    legend__ (LegendArrow c s, t) =
      hori
        identity
        [ arrows c [Arrow zero (s *. one), Arrow (s *. one) zero]
        , strutX (legendSep opts)
        , text_ (legendText opts) t
        ]
    legend__ (LegendPixel c s, t) =
      hori
        identity
        [rect_ c (s *. one), strutX (legendSep opts), text_ (legendText opts) t]
