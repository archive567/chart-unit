{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Glyphs are (typically) small shapes symbolically representing a data point.
module Chart.Glyph
  ( GlyphOptions(GlyphOptions)
  , GlyphShape(..)
  , glyphShape
  , glyph_
  , glyphs
  , lglyphs
  , glyphChart
  , glyphChart_
  , lglyphChart
  , lglyphChart_
  , circle
  , square
  , triangle
  ) where

import Chart.Core
import Chart.Text
import Diagrams.Prelude hiding (Color, D, scaleX, scaleY)
import NumHask.Pair
import NumHask.Prelude
import NumHask.Rect

-- | The actual shape of a glyph can be any Chart element
data GlyphOptions b = GlyphOptions
  { size :: Double -- ^ glyph radius
  , color :: AlphaColour Double
  , borderColor :: AlphaColour Double
  , borderSize :: Double -- ^ normalized
  , shape :: GlyphShape
  } deriving (Show, Generic)

instance Default (GlyphOptions b) where
  def = GlyphOptions 0.03 ublue ugrey 0.015 Circle

data GlyphShape =
    Circle |
    Square |
    Ellipse Double |
    Triangle |
    Pentagon |
    Hexagon |
    Septagon |
    Octagaon |
    RectSharp Double |
    RectRounded Double Double |
    VLine Double |
    HLine Double
    deriving Show

glyphShape :: GlyphShape -> (Double -> Chart b)
glyphShape Circle = \x -> circle (x/2)
glyphShape Square = square
glyphShape (Ellipse a) = ellipseXY a
glyphShape Triangle = triangle
glyphShape Pentagon = pentagon
glyphShape Hexagon = hexagon
glyphShape Septagon = septagon
glyphShape Octagaon = octagon
glyphShape (RectSharp a) = \x -> rect (a*x) x
glyphShape (RectRounded a r) = \x -> roundedRect (a*x) x r
glyphShape (VLine a) = vline_ a
glyphShape (HLine a) = hline_ a

-- | Vertical line glyph shape with a reasonable thickness at "vline_ 1"
vline_ :: Double -> Double -> Chart b
vline_ fatness x = vrule x # scaleX (1.6 / 0.5 * fatness)

-- | Horizontal line glyph shape with a reasonable thickness as "hline_ 1"
hline_ :: Double -> Double -> Chart b
hline_ fatness x = hrule x # scaleY (1.6 / 0.5 * fatness)

-- | Create a glyph.
--
-- > let glyph_Example = glyph_ def
--
-- ![glyph_ example](other/glyph_Example.svg)
--
glyph_ :: GlyphOptions b -> Chart b
glyph_ (GlyphOptions s c bc bs sh) = glyphShape sh s # fcA c # lcA bc # lwN bs

-- | Create positioned glyphs.
--
-- > glyphsExample :: Chart b
-- > glyphsExample = glyphs def (dataXY sin (Range 0 (2*pi)) 30)
--
-- ![glyphs example](other/glyphsExample.svg)
--
glyphs :: (R2 r, Traversable f) => GlyphOptions b -> f (r Double) -> Chart b
glyphs opts xs = mconcat $ toList $ (\x -> positioned x (glyph_ opts)) <$> xs

-- | A chart of glyphs
glyphChart ::
     (Traversable f)
  => [GlyphOptions b]
  -> Aspect
  -> Rect Double
  -> [f (Pair Double)]
  -> Chart b
glyphChart optss (Aspect asp) r xyss =
  mconcat $ zipWith glyphs optss (projectss r asp xyss)

-- | A chart of glyphs scaled to its own range
--
-- > gopts :: [GlyphOptions b]
-- > gopts = [ glyphBorderSize_ .~ 0.001 $ def
-- >         , glyphBorderSize_ .~ 0.001 $
-- >           glyphSize_ .~ 0.1 $
-- >           glyphColor_ .~ rybColor 7 `withOpacity` 0.4 $
-- >           def {glyphShape = triangle}
-- >         ]
-- > 
-- > gdata :: [[Pair Double]]
-- > gdata = [ dataXY sin (Range 0 (2*pi)) 30
-- >         , dataXY cos (Range 0 (2*pi)) 30
-- >         ]
-- > 
-- > glyphChart_Example :: Chart b
-- > glyphChart_Example = glyphChart_ gopts widescreen gdata
--
-- ![glyphChart_ example](other/glyphChart_Example.svg)
--
glyphChart_ ::
     (Traversable f)
  => [GlyphOptions b]
  -> Aspect
  -> [f (Pair Double)]
  -> Chart b
glyphChart_ optss asp xyss = glyphChart optss asp (range xyss) xyss

-- | Create labelled, positioned glyphs.
--
-- > lglyphs def def $ zip (show <$> [0..]) ps
--
-- ![lglyphs example](other/lglyphsExample.svg)
--
lglyphs ::
     (R2 r, Traversable f)
  => LabelOptions
  -> GlyphOptions b
  -> f (Text, r Double)
  -> Chart b
lglyphs lopts gopts xs =
  mconcat $
  toList $ (\(t, x) -> moveTo (p_ x) $ labelled lopts t (glyph_ gopts)) <$> xs

-- | A chart of labelled glyphs
lglyphChart ::
     (Traversable f)
  => [LabelOptions]
  -> [GlyphOptions b]
  -> Aspect
  -> Rect Double
  -> [f (Text, Pair Double)]
  -> Chart b
lglyphChart ls gs (Aspect asp) r xyss =
  mconcat $
  getZipList $
  lglyphs <$> ZipList ls <*> ZipList gs <*>
  ZipList
    (zipWith
       zip
       (map fst . toList <$> xyss)
       (projectss r asp (map snd . toList <$> xyss)))

-- | A chart of labelled glyphs scaled to its own range
--
-- > let g = Pair <$> [0..5] <*> [0..5] :: [Pair Int]
-- > let xs = [(\(p@(Pair x y)) -> ((show x <> "," <> show y), fromIntegral <$> p)) <$> g]
-- > lglyphChart_ [def {labelGap=0.01}] [def] sixbyfour xs
--
-- ![lglyphChart_ example](other/lglyphChart_Example.svg)
--
lglyphChart_ ::
     (Traversable f)
  => [LabelOptions]
  -> [GlyphOptions b]
  -> Aspect
  -> [f (Text, Pair Double)]
  -> Chart b
lglyphChart_ ls gs asp xyss =
  lglyphChart ls gs asp (range (map snd . toList <$> xyss)) xyss
