{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Chart.Glyph
  ( GlyphOptions(..)
  , hline_
  , vline_
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

import NumHask.Prelude hiding (min,max,from,to,(&),local,size,rotate)
import NumHask.Rect
import NumHask.Pair

import Diagrams.Prelude hiding (width, unit, D, Color, scale, zero, scaleX, scaleY, aspect, rect, project, lineColor, (*.), size)

-- | The actual shape of a glyph can be any Chart element
data GlyphOptions b = GlyphOptions
    { glyphSize :: Double  -- ^ glyph radius
    , glyphColor :: AlphaColour Double
    , glyphBorderColor :: AlphaColour Double
    , glyphBorderSize :: Double -- ^ normalized
    , glyphShape :: Double -> Chart b
    }

instance Default (GlyphOptions b) where
    def = GlyphOptions 0.03 ublue ugrey 0.015 circle

-- | vertical line glyph shape
vline_ :: Double -> Double -> Chart b
vline_ fatness x = vrule x # scaleX (1.6/0.5*fatness)

-- | horizontal line glyph shape
hline_ :: Double -> Double -> Chart b
hline_ fatness x = hrule x # scaleY (1.6/0.5*fatness)

-- | create a glyph from a configuration
--
-- > let glyph_Example = glyph_ def
--
-- ![glyph_ example](other/glyph_Example.svg)
--
glyph_ :: GlyphOptions b -> Chart b
glyph_ (GlyphOptions s c bc bs shape) =
    (shape s) # fcA c # lcA bc # lwN bs

-- | positioned glyphs
--
-- > let ps = [Pair (x/10) ((sin x)/10) | x<-[0..10]]
-- > glyphs def ps
--
-- ![glyphs example](other/glyphsExample.svg)
--
glyphs :: (R2 r, Traversable f) => GlyphOptions b -> f (r Double) -> Chart b
glyphs opts xs = mconcat $ toList $ (\x -> positioned x (glyph_ opts)) <$> xs

-- | a chart of glyphs
glyphChart ::
    (Traversable f) =>
    [GlyphOptions b] -> Aspect -> Rect Double -> [f (Pair Double)] -> Chart b
glyphChart optss (Aspect asp) r xyss =
    mconcat $ zipWith glyphs optss (projectss r asp xyss)

-- | a chart of glyphs scaled to its own range
--
-- > let gopts = [def,def {glyphBorderColor=withOpacity red 0.2, glyphShape=triangle}]
-- > let p_1 = [Pair x (sin (x/10)) | x<-[0..100]]
-- > let p_2 = [Pair x (cos (x/10)) | x<-[0..100]]
-- > glyphChart_ gopts widescreen [p_1,p_2]
--
-- ![glyphChart_ example](other/glyphChart_Example.svg)
--
glyphChart_ :: (Traversable f) =>
                [GlyphOptions b] -> Aspect -> [f (Pair Double)] -> Chart b
glyphChart_ optss asp xyss =
    glyphChart optss asp (range xyss) xyss

-- | labelled, positioned glyphs
--
-- > lglyphs def def $ zip (show <$> [0..]) ps
--
-- ![lglyphs example](other/lglyphsExample.svg)
--
lglyphs :: (R2 r, Traversable f) =>
    LabelOptions -> GlyphOptions b -> f (Text, r Double) -> Chart b
lglyphs lopts gopts xs =
    mconcat $ toList $
    (\(t, x) -> moveTo (p_ x) $ labelled lopts t (glyph_ gopts)) <$> xs

-- | a chart of labelled glyphs
lglyphChart ::
    (Traversable f) =>
    [LabelOptions] -> [GlyphOptions b] -> Aspect -> Rect Double ->
    [f (Text, Pair Double)] -> Chart b
lglyphChart ls gs (Aspect asp) r xyss =
    mconcat $ getZipList $ lglyphs <$> ZipList ls <*> ZipList gs <*> ZipList
    (zipWith zip
     (map fst . toList <$> xyss)
     (projectss r asp (map snd . toList <$> xyss)))

-- | a chart of labelled glyphs scaled to its own range
--
-- > let g = Pair <$> [0..5] <*> [0..5] :: [Pair Int]
-- > let xs = [(\(p@(Pair x y)) -> ((show x <> "," <> show y), fromIntegral <$> p)) <$> g]
-- > lglyphChart_ [def {labelGap=0.01}] [def] sixbyfour xs
--
-- ![lglyphChart_ example](other/lglyphChart_Example.svg)
--
lglyphChart_ ::
    (Traversable f) =>
    [LabelOptions] -> [GlyphOptions b] -> Aspect ->
    [f (Text, Pair Double)] -> Chart b
lglyphChart_ ls gs asp xyss =
    lglyphChart ls gs asp (range (map snd . toList <$> xyss)) xyss

