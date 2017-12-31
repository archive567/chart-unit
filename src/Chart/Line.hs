{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Points on a chart connected by lines.
module Chart.Line
  ( LineOptions(LineOptions)
  , oneline
  , lines
  , glines
  , lineChart
  , lineChart_
  , glineChart
  , glineChart_
  ) where

import Chart.Core
import Chart.Glyph
import Data.Default (Default(..))
import Diagrams.Prelude hiding ((<>))
import GHC.Generics
import NumHask.Pair
import NumHask.Prelude
import NumHask.Rect

-- | The main features of a line (that distinguish it from a glyph say) is that:
--
-- - it exists over multiple points (a line can't exist at a single point)
--
-- - line rendering is normalized to the eventual physical chart
--
data LineOptions = LineOptions
  { size :: Double -- ^ normalized
  , color :: AlphaColour Double
  } deriving (Show, Generic)

instance Default LineOptions where
  def = LineOptions 0.02 ublue

-- | A line connecting a series of points
--
-- > linesExample :: Int -> Chart b
-- > linesExample n =
-- >   lines
-- >   (#color .~ red `withOpacity` 0.5 $ def)
-- >   (dataXY cos (Range 0 (4*pi)) n)
--
-- ![lines example](other/linesExample.svg)
--
lines :: (Traversable f, R2 r) => LineOptions -> f (r Double) -> Chart b
lines (LineOptions s c) xs =
  case NumHask.Prelude.head xs of
    Nothing -> mempty
    Just p ->
      trailLike (trailFromVertices (toList $ p_ <$> xs) `at` p_ p) # lcA c #
      lwN s

-- | A single line connecting 2 points
oneline :: (R2 r) => LineOptions -> Pair (r Double) -> Chart b
oneline (LineOptions s c) (Pair x0 x1) =
  stroke (trailFromVertices [p_ x0, p_ x1] `at` p_ x0) # lcA c # lwN s

-- | A chart of lines
lineChart ::
     (Traversable f)
  => [LineOptions]
  -> Rect Double
  -> Rect Double
  -> [f (Pair Double)]
  -> Chart b
lineChart optss asp r xyss =
  mconcat $ zipWith lines optss (projectss r asp xyss)

-- | A chart of lines scaled to its own range
--
-- > ls :: [[Pair Double]]
-- > ls =
-- >   map (uncurry Pair) <$>
-- >   [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
-- >   , [(0.0, 0.0), (3.0, 3.0)]
-- >   , [(0.5, 4.0), (0.5, 0)]
-- >   ]
-- >
-- > lopts :: [LineOptions]
-- > lopts =
-- >   zipWith
-- >   (\x y -> LineOptions x (withOpacity (d3Colors1 y) 0.6))
-- >   [0.01, 0.02, 0.005]
-- >   [0,1,2]
-- >
-- > lineChart_Example :: Chart b
-- > lineChart_Example = lineChart_ lopts sixbyfour ls
--
-- ![lineChart_ example](other/lineChart_Example.svg)
--
lineChart_ ::
     (Traversable f)
  => [LineOptions]
  -> Rect Double
  -> [f (Pair Double)]
  -> Chart b
lineChart_ optss asp xyss = lineChart optss asp (range xyss) xyss

-- | Lines with glyphs atop eack point
glines ::
     (Traversable f, R2 r)
  => LineOptions
  -> GlyphOptions
  -> f (r Double)
  -> Chart b
glines opts gopts xs = glyphs gopts xs <> lines opts xs

-- | A chart of glines
glineChart ::
     (Traversable f)
  => [LineOptions]
  -> [GlyphOptions]
  -> Rect Double
  -> Rect Double
  -> [f (Pair Double)]
  -> Chart b
glineChart ls gs asp r xyss =
  mconcat $
  getZipList $
  glines <$> ZipList ls <*> ZipList gs <*> ZipList (projectss r asp xyss)

-- | A chart of glyphs_lines scaled to its own range
--
-- > gopts3 :: [GlyphOptions]
-- > gopts3 =
-- >   zipWith
-- >   (\x y ->
-- >      #color .~ withOpacity (d3Colors1 x) 0.2 $
-- >      #borderColor .~ withOpacity (d3Colors1 x) 1 $
-- >      #borderSize .~ 0.005 $
-- >      #shape .~ y $
-- >      #size .~ 0.08 $
-- >      def)
-- >   [6,8,2]
-- >   [Triangle, Square, Circle]
-- >
-- > glineChart_Example :: Chart b
-- > glineChart_Example = glineChart_ lopts gopts3 sixbyfour ls
--
-- ![glineChart_ example](other/glineChart_Example.svg)
--
glineChart_ ::
     (Traversable f)
  => [LineOptions]
  -> [GlyphOptions]
  -> Rect Double
  -> [f (Pair Double)]
  -> Chart b
glineChart_ ls gs asp xyss = glineChart ls gs asp (range xyss) xyss
