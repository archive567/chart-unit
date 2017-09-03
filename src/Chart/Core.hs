{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

-- | In making a chart, there are three main size domains you have to be concerned about:
--
-- - the range of the data being charted. This range is often projected onto chart elements such as axes and labels. A data range in two dimensions is a 'Rect' a.
--
-- - the scale of various chart primitives and elements.  The overall dimensions of the chart canvas - the rectangular shape on which the data is represented - is referred to as an 'Aspect' in the api, and is a wrapped 'Rect' to distinguish aspects from rect ranges.  The default chart options tend to be callibrated to Aspects around widths of one.
--
-- - the size of the chart rendered as an image. Backends tend to shrink charts to fit the rectangle shape specified in the render function, and a loose sympathy is expected between the aspect and a chart's ultimate physical size.
--
-- Jumping ahead a bit, the code snippet below draws vertical lines using a data range of "Rect 0 12 0 0.2" (slightly different to the actual data range), using a widescreen (3:1) aspect, and renders the chart as a 300 by 120 pixel svg:
--
-- > fileSvg "other/scaleExample.svg" (300,120) $
-- >   withHud (hudAspect_ .~ widescreen $ hudRange_ .~ Just (Rect 0 12 0 0.2) $ def)
-- >   (lineChart (repeat def)) ((\x -> [Pair x 0, Pair x (x/100)]) <$> [0..10])
--
-- ![scale example](other/scaleExample.svg)
--
module Chart.Core
  ( -- * Chart types
    Chart
  , UChart(..)
  , combine

    -- * Scaling
  , range
  , projectss
  , Aspect(..)
  , aspect
  , asquare
  , sixbyfour
  , golden
  , widescreen
  , skinny
  , AlignH(..)
  , AlignV(..)
  , alignHU
  , alignHTU
  , alignVU

    -- * Combinators
    --
    -- | The concept of a point on a chart is the polymorphic 'R2' from the 'linear' library.  Diagrams most often uses 'Point', which is a wrapped 'V2'.  The 'Pair' type from 'numhask-range' is often used as a point reference.
  , positioned
  , p_
  , r_
  , stack
  , vert
  , hori
  , sepVert
  , sepHori

    -- * IO
  , fileSvg

    -- * Color
    --
    -- | chart-unit exposes the 'colour' and 'palette' libraries for color combinators
  , ucolor
  , ublue
  , ugrey

    -- * Compatability
  , scaleX
  , scaleY
  , scale
  ) where

import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude
       hiding (Color, D, aspect, project, scale, scaleX, scaleY, zero)
import qualified Diagrams.Prelude as Diagrams
import NumHask.Pair
import NumHask.Prelude
import NumHask.Rect
import NumHask.Space

-- | A Chart is simply a type synonym for a typical Diagrams object.  A close relation to this type is 'Diagram' 'B', but this usage tends to force a single backend (B comes from the backend libraries), so making Chart b's maintains backend polymorphism.
--
-- Just about everything - text, circles, lines, triangles, charts, axes, titles, legends etc - are 'Chart's, which means that most things are amenable to full use of the combinatorially-inclined diagrams-lib.
type Chart b
   = (Renderable (Path V2 Double) b) =>
       QDiagram b V2 Double Any

-- | a UChart provides a late binding of a chart Aspect so multiple charts can be rendered using the same range.
data UChart a b = UChart
  { uchartRenderer :: () =>
                        Aspect -> Rect Double -> a -> Chart b
  , uchartRenderRange :: Rect Double
  , uchartData :: a
  }

-- | render a list of charts, taking into account each of their ranges
combine :: Aspect -> [UChart a b] -> Chart b
combine asp qcs = mconcat $ (\(UChart c _ x) -> c asp rall x) <$> qcs
  where
    rall = fold $ (\(UChart _ r1 _) -> r1) <$> qcs

-- | project a double-containered set of data to a new Rect range
projectss ::
     (Functor f, Functor g)
  => Rect Double
  -> Rect Double
  -> g (f (Pair Double))
  -> g (f (Pair Double))
projectss r0 r1 xyss = map (project r0 r1) <$> xyss

-- | determine the range of a double-containered set of data
range :: (Foldable f, Foldable g) => g (f (Pair Double)) -> Rect Double
range xyss = foldMap space xyss

-- | a wrapped Rect specifying the shape od the chart canvas.
--
-- The Aspect tends to be:
--
-- - independent of the data range
-- - expressed in terms around a width magnitude of one.  chart default options are callibrated to this convention.
newtype Aspect = Aspect (Rect Double)

-- | the rendering aspect of a chart expressed as a ratio of x-plane : y-plane.
aspect :: Double -> Aspect
aspect a = Aspect $ Ranges ((a *) <$> one) one

-- | a 1:1 aspect
asquare :: Aspect
asquare = aspect 1

-- | a 1.5:1 aspect
sixbyfour :: Aspect
sixbyfour = aspect 1.5

-- | golden ratio
golden :: Aspect
golden = aspect 1.61803398875

-- | a 3:1 aspect
widescreen :: Aspect
widescreen = aspect 3

-- | a skinny 5:1 aspect
skinny :: Aspect
skinny = aspect 5

-- | horizontal alignment
data AlignH
  = AlignLeft
  | AlignCenter
  | AlignRight

-- | vertical alignment
data AlignV
  = AlignTop
  | AlignMid
  | AlignBottom

-- | conversion of horizontal alignment to (one :: Range Double) limits
alignHU :: AlignH -> Double
alignHU a =
  case a of
    AlignLeft -> 0.5
    AlignCenter -> 0
    AlignRight -> -0.5

-- | svg text is forced to be lower left (-0.5) by default
alignHTU :: AlignH -> Double
alignHTU a =
  case a of
    AlignLeft -> 0
    AlignCenter -> -0.5
    AlignRight -> -1

-- | conversion of vertical alignment to (one :: Range Double) limits
alignVU :: AlignV -> Double
alignVU a =
  case a of
    AlignTop -> -0.5
    AlignMid -> 0
    AlignBottom -> 0.5

-- | position an element at a point
positioned :: (R2 r) => r Double -> Chart b -> Chart b
positioned p = moveTo (p_ p)

-- | convert an R2 to a diagrams Point
p_ :: (R2 r) => r Double -> Point V2 Double
p_ r = curry p2 (r ^. _x) (r ^. _y)

-- | convert an R2 to a V2
r_ :: R2 r => r a -> V2 a
r_ r = V2 (r ^. _x) (r ^. _y)

-- | foldMap for beside; stacking chart elements in a direction, with a premap
stack ::
     (R2 r, V a ~ V2, Foldable t, Juxtaposable a, Semigroup a, N a ~ Double, Monoid a)
  => r Double
  -> (b -> a)
  -> t b
  -> a
stack dir f xs = foldr (\a x -> beside (r_ dir) (f a) x) mempty xs

-- | combine elements vertically, with a premap
vert ::
     (V a ~ V2, Foldable t, Juxtaposable a, Semigroup a, N a ~ Double, Monoid a)
  => (b -> a)
  -> t b
  -> a
vert = stack (Pair 0 -1)

-- | combine elements horizontally, with a premap
hori ::
     (V a ~ V2, Foldable t, Juxtaposable a, Semigroup a, N a ~ Double, Monoid a)
  => (b -> a)
  -> t b
  -> a
hori = stack (Pair 1 0)

-- | horizontal separator
sepHori :: Double -> Chart b -> Chart b
sepHori s x = beside (r2 (0, -1)) x (strutX s)

-- | vertical separator
sepVert :: Double -> Chart b -> Chart b
sepVert s x = beside (r2 (1, 0)) x (strutY s)

-- | write an svg to file
fileSvg :: FilePath -> (Double, Double) -> Diagram SVG -> IO ()
fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

-- | convert an rgba spec to an AlphaColour
ucolor :: (Floating a, Ord a) => a -> a -> a -> a -> AlphaColour a
ucolor r g b o = withOpacity (sRGB r g b) o

-- | the official chart-unit blue
ublue :: AlphaColour Double
ublue = ucolor 0.365 0.647 0.855 0.5

-- | the official chart-unit grey
ugrey :: AlphaColour Double
ugrey = ucolor 0.4 0.4 0.4 1

-- | These are difficult to avoid
instance R1 Pair where
  _x f (Pair a b) = (`Pair` b) <$> f a

instance R2 Pair where
  _y f (Pair a b) = Pair a <$> f b
  _xy f p = fmap (\(V2 a b) -> Pair a b) . f . (\(Pair a b) -> V2 a b) $ p

eps :: N [Point V2 Double]
eps = 1e-8

-- | the diagrams scaleX with a zero divide guard to avoid error throws
scaleX ::
     (N t ~ Double, Transformable t, R2 (V t), Diagrams.Additive (V t))
  => Double
  -> t
  -> t
scaleX s =
  Diagrams.scaleX
    (if s == zero
       then eps
       else s)

-- | the diagrams scaleY with a zero divide guard to avoid error throws
scaleY ::
     (N t ~ Double, Transformable t, R2 (V t), Diagrams.Additive (V t))
  => Double
  -> t
  -> t
scaleY s =
  Diagrams.scaleY
    (if s == zero
       then eps
       else s)

-- | the diagrams scale with a zero divide guard to avoid error throws
scale ::
     (N t ~ Double, Transformable t, R2 (V t), Diagrams.Additive (V t))
  => Double
  -> t
  -> t
scale s =
  Diagrams.scale
    (if s == zero
       then eps
       else s)
