{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

module Chart.Core
  ( -- * chart
    --
    -- $chart
    Chart
  , UChart(..)
  , combine
    -- * scaling
    --
    -- $scaling
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
    -- * combinator
    --
    -- $combinator
  , positioned
  , p_
  , r_
  , vert
  , hori
  , sepVert
  , sepHori
    -- * IO
    --
    -- $io
  , fileSvg
    -- * color
    --
    -- $color
  , ucolor
  , ublue
  , ugrey
  , ugreen
  , ured
    -- * errata
    --
    -- $errata
  , scaleX
  , scaleY
  , scale
  ) where

import Data.Colour.Palette.ColorSet
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude
       hiding (Color, D, aspect, project, scale, scaleX, scaleY, zero)
import qualified Diagrams.Prelude as Diagrams
import NumHask.Pair
import NumHask.Prelude
import NumHask.Rect
import NumHask.Space

-- $chart
--
-- base chart type
-- | A Chart is simply a type synonym for a typical Diagrams object.  A close relation to this type is 'Diagram' 'B', but this usage tends to force a single backend (B comes from the backend libraries), so this is preferred to maintain backend polymorphism.
-- Just about everything - text, circles, lines, triangles, charts, axes, titles, legends etc - are 'Chart's, which means that most things are amenable to full use of the 'diagrams-lib' library.
type Chart b
   = (Renderable (Path V2 Double) b) =>
       QDiagram b V2 Double Any

-- | a UChart is mostly a late binding of the Chart Aspect that is to be projected on to and the data.
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

-- $scaling
--
-- scaling functionality
projectss ::
     (Functor f, Functor g)
  => Rect Double
  -> Rect Double
  -> g (f (Pair Double))
  -> g (f (Pair Double))
projectss r0 r1 xyss = map (project r0 r1) <$> xyss

range :: (Foldable f, Foldable g) => g (f (Pair Double)) -> Rect Double
range xyss = foldMap space xyss

-- | the rendering plane
newtype Aspect = Aspect
  { aspectRect :: Rect Double
  }

-- | the rendering aspect of a chart expressed as a ratio of x-plane : y-plane.
aspect :: Double -> Aspect
aspect a = Aspect $ Ranges ((a *) <$> one) one

-- | 1:1
asquare :: Aspect
asquare = aspect 1

-- | 1.5:1
sixbyfour :: Aspect
sixbyfour = aspect 1.5

-- | golden ratio
golden :: Aspect
golden = aspect 1.61803398875

-- | 3:1
widescreen :: Aspect
widescreen = aspect 3

-- | 5:1
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

-- | conversion of horizontal alignment to one :: Range Double
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

-- | conversion of vertical alignment to one :: Range Double
alignVU :: AlignV -> Double
alignVU a =
  case a of
    AlignTop -> -0.5
    AlignMid -> 0
    AlignBottom -> 0.5

-- $combinator
--
-- chart combinators
-- | position an element
positioned :: (R2 r) => r Double -> Chart b -> Chart b
positioned p = moveTo (p_ p)

-- | convert an R2 to a diagrams Point
p_ :: (R2 r) => r Double -> Point V2 Double
p_ r = curry p2 (r ^. _x) (r ^. _y)

-- | convert an R2 to a V2
r_ :: R2 r => r a -> V2 a
r_ r = V2 (r ^. _x) (r ^. _y)

-- | combine elements vertically, with a premap
vert ::
     (V a ~ V2, Foldable t, Juxtaposable a, Semigroup a, Num (N a), Monoid a)
  => (a -> a)
  -> t a
  -> a
vert f xs = foldr (\a x -> beside (r2 (0, -1)) (f a) x) mempty xs

-- | combine elements horizontally, with a premap
hori ::
     (V a ~ V2, Foldable t, Juxtaposable a, Semigroup a, Num (N a), Monoid a)
  => (a -> a)
  -> t a
  -> a
hori f xs = foldr (\a x -> beside (r2 (1, 0)) (f a) x) mempty xs

-- | horizontal separator
sepHori :: Double -> Chart b -> Chart b
sepHori s x = beside (r2 (0, -1)) x (strutX s)

-- | vertical separator
sepVert :: Double -> Chart b -> Chart b
sepVert s x = beside (r2 (1, 0)) x (strutY s)

-- $io
--
-- | write an svg to file
fileSvg :: FilePath -> (Double, Double) -> Diagram SVG -> IO ()
fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

-- $color
--
ucolor :: (Floating a, Ord a) => a -> a -> a -> a -> AlphaColour a
ucolor r g b o = withOpacity (sRGB r g b) o

ublue :: AlphaColour Double
ublue = ucolor 0.365 0.647 0.855 0.5

ugreen :: AlphaColour Double
ugreen = rybColor 11 `withOpacity` 0.5

ured :: AlphaColour Double
ured = rybColor 0 `withOpacity` 0.5

ugrey :: AlphaColour Double
ugrey = ucolor 0.5 0.5 0.5 0.5

-- $errata
--
instance R1 Pair where
  _x f (Pair a b) = (`Pair` b) <$> f a

instance R2 Pair where
  _y f (Pair a b) = Pair a <$> f b
  _xy f p = fmap (\(V2 a b) -> Pair a b) . f . (\(Pair a b) -> V2 a b) $ p

-- avoiding the diagrams throw on zero divides
eps :: N [Point V2 Double]
eps = 1e-8

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
