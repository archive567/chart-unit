{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Glyphs are (typically) small shapes symbolically representing a data point.
module Chart.Glyph
  ( GlyphOptions(..)
  , GlyphShape(..)
  , glyphShape
  , glyph_
  , glyphs
  , lglyphs
  , glyphChart
  , glyphChart_
  , lglyphChart
  , lglyphChart_
  ) where

import Chart.Core
import Chart.Text
import Diagrams.Prelude hiding (Color, D, scaleX, scaleY)
import NumHask.Pair
import NumHask.Prelude
import NumHask.Rect

-- | The actual shape of a glyph can be any Chart element
data GlyphOptions = GlyphOptions
  { size :: Double -- ^ glyph radius
  , color :: UColor Double
  , borderColor :: UColor Double
  , borderSize :: Double -- ^ normalized
  , shape :: GlyphShape
  } deriving (Show, Eq, Generic)

instance Default GlyphOptions where
  def = GlyphOptions 0.03 ublue ugrey 0.015 Circle

-- | shape of the glyph expressed in diagrams terms
data GlyphShape
  = Circle
  | Square
  | Ellipse Double
  | Triangle
  | Pentagon
  | Hexagon
  | Septagon
  | Octagaon
  | RectSharp Double
  | RectRounded Double
                Double
  | VLine Double
  | HLine Double
  deriving (Show, Eq, Generic)

-- | convert from chart-unit to diagrams shapes
glyphShape :: GlyphShape -> (Double -> Chart b)
glyphShape Circle = \x -> circle (x / 2)
glyphShape Square = square
glyphShape (Ellipse a) = ellipseXY a
glyphShape Triangle = triangle
glyphShape Pentagon = pentagon
glyphShape Hexagon = hexagon
glyphShape Septagon = septagon
glyphShape Octagaon = octagon
glyphShape (RectSharp a) = \x -> rect (a * x) x
glyphShape (RectRounded a r) = \x -> roundedRect (a * x) x r
glyphShape (VLine a) = \x -> vrule x # scaleX (1.6 / 0.5 * a)
glyphShape (HLine a) = \x -> hrule x # scaleY (1.6 / 0.5 * a)

-- | Create a glyph.
--
-- > glyph_ def
--
-- ![glyph_ example](other/glyph_Example.svg)
--
glyph_ :: GlyphOptions -> Chart b
glyph_ (GlyphOptions s c bc bs sh) = glyphShape sh s # fcA (acolor c) # lcA (acolor bc) # lwN bs

-- | Create positioned glyphs.
--
-- > glyphs def (dataXY sin (Range 0 (2*pi)) 30)
--
-- ![glyphs example](other/glyphsExample.svg)
--
glyphs :: (R2 r, Traversable f) => GlyphOptions -> f (r Double) -> Chart b
glyphs opts xs = mconcat $ toList $ (\x -> positioned x (glyph_ opts)) <$> xs

-- | A chart of glyphs
glyphChart ::
     (Traversable f)
  => [GlyphOptions]
  -> Rect Double
  -> Rect Double
  -> [f (Pair Double)]
  -> Chart b
glyphChart optss asp r xyss =
  mconcat $ zipWith glyphs optss (projectss r asp xyss)

-- | A chart of glyphs scaled to its own range
--
-- > gopts :: [GlyphOptions]
-- > gopts =
-- >   [ #borderSize .~ 0.001 $ def
-- >   , #borderSize .~ 0.001 $
-- >     #size .~ 0.1 $
-- >     #color .~ rybColor 7 `withOpacity` 0.4 $
-- >     #shape .~ Triangle $ def
-- >   ]
-- > 
-- > gdata :: [[Pair Double]]
-- > gdata =
-- >   [ dataXY sin (Range 0 (2*pi)) 30
-- >   , dataXY cos (Range 0 (2*pi)) 30
-- >   ]
-- > 
-- > glyphChart_Example :: Chart b
-- > glyphChart_Example = glyphChart_ gopts widescreen gdata
--
-- ![glyphChart_ example](other/glyphChart_Example.svg)
--
glyphChart_ ::
     (Traversable f)
  => [GlyphOptions]
  -> Rect Double
  -> [f (Pair Double)]
  -> Chart b
glyphChart_ optss asp xyss = glyphChart optss asp (range xyss) xyss

-- | Create labelled, positioned glyphs.
--
-- > lglyphsExample :: Chart b
-- > lglyphsExample =
-- >   lglyphs def def $
-- >   zip (show <$> [0 ..]) [Pair (x / 10) (sin x / 10) | x <- [0 .. 10]]
--
-- ![lglyphs example](other/lglyphsExample.svg)
--
lglyphs ::
     (R2 r, Traversable f)
  => LabelOptions
  -> GlyphOptions
  -> f (Text, r Double)
  -> Chart b
lglyphs lopts gopts xs =
  mconcat $
  toList $ (\(t, x) -> moveTo (p_ x) $ labelled lopts t (glyph_ gopts)) <$> xs

-- | A chart of labelled glyphs
lglyphChart ::
     (Traversable f)
  => [LabelOptions]
  -> [GlyphOptions]
  -> Rect Double
  -> Rect Double
  -> [f (Text, Pair Double)]
  -> Chart b
lglyphChart ls gs asp r xyss =
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
-- > lgdata :: [(Text, Pair Double)]
-- > lgdata =
-- >   [(\(p@(Pair x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$>
-- >     (Pair <$> [0 .. 5] <*> [0 .. 5] :: [Pair Int])
-- >   ]
-- > 
-- > lglyphChart_Example :: Rect Double -> Chart b
-- > lglyphChart_Example a =
-- >   lglyphChart_
-- >   [#gap .~ 0.015 $ #text . #size .~ 0.12 $ def]
-- >   [#color .~ black `withOpacity` 1 $
-- >    #borderSize .~ 0 $
-- >    #size .~ 0.01 $
-- >    def]
-- >   a
-- >   [lgdata]
--
-- ![lglyphChart_ example](other/lglyphChart_Example.svg)
--
lglyphChart_ ::
     (Traversable f)
  => [LabelOptions]
  -> [GlyphOptions]
  -> Rect Double
  -> [f (Text, Pair Double)]
  -> Chart b
lglyphChart_ ls gs asp xyss =
  lglyphChart ls gs asp (range (map snd . toList <$> xyss)) xyss
