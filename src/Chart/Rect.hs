{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
-- | rectangular chart elements
module Chart.Rect
  ( RectOptions(..)
  , blob
  , box
  , clear
  , bound
  , rect_
  , rects
  , rectChart
  , rectChart_
  , Pixel(..)
  , pixel_
  , pixels
  , pixelChart
  , pixelChart_
  , PixelationOptions(..)
  , pixelate
  , pixelateChart
  ) where

import Chart.Core
import Diagrams.Prelude hiding (Color, D, scaleX, scaleY)
import NumHask.Pair
import NumHask.Prelude
import NumHask.Range
import NumHask.Rect
import NumHask.Space

data RectOptions = RectOptions
  { rectBorderSize :: Double
  , rectBorderColor :: AlphaColour Double
  , rectColor :: AlphaColour Double
  }

instance Default RectOptions where
  def = RectOptions 0.005 (ucolor 0.4 0.4 0.4 1) ublue

-- | solid rect, no border
blob :: AlphaColour Double -> RectOptions
blob c = RectOptions 0 transparent c

-- | clear and transparent rect
clear :: RectOptions
clear = RectOptions 0 transparent transparent

-- | clear rect, with border
box :: AlphaColour Double -> RectOptions
box c = RectOptions 0.015 c transparent

-- | place a rect around an Chart
bound :: RectOptions -> Double -> Chart b -> Chart b
bound (RectOptions bs bc c) p x =
  (boundingRect x' # lcA bc # lwN bs # fcA c) <> x'
  where
    x' = pad p x

-- | a single rectangle specified using a Rect x z y w where
-- (x,y) is location of lower left corner
-- (z,w) is location of upper right corner
--
-- > let opts o = def {labelText = (labelText def) {textColor=withOpacity black 0.8, textSize = 0.3}, labelOrientation=o}
-- > labelled (opts (Pair 2 1)) ("z,w") $ labelled (opts (Pair -2 -1)) ("x,y") (rect_ def (Ranges (2*.one) one))
--
-- ![rect_ example](other/rect_Example.svg)
--
rect_ ::
     ( N b ~ Double
     , V b ~ V2
     , Transformable b
     , HasOrigin b
     , TrailLike b
     , HasStyle b
     )
  => RectOptions
  -> Rect Double
  -> b
rect_ (RectOptions bs bc c) (Rect x z y w) =
  unitSquare # moveTo (p2 (0.5, 0.5)) # scaleX (z - x) # scaleY (w - y) #
  moveTo (p2 (x, y)) #
  fcA c #
  lcA bc #
  lwN bs

-- | rectangles with the same configuration
--
-- > rects def $ zipWith (\x y -> Rect x (x+1) 0 y) [0..] [1,2,3,5,8,0,-2,11,2,1]
--
-- ![rects example](other/rectsExample.svg)
--
rects ::
     ( V a ~ V2
     , N a ~ Double
     , Functor t
     , HasStyle a
     , TrailLike a
     , HasOrigin a
     , Transformable a
     , Foldable t
     , Monoid a
     )
  => RectOptions
  -> t (Rect Double)
  -> a
rects opts xs = mconcat $ toList $ rect_ opts <$> xs

-- | a chart of rects
rectChart ::
     (Traversable f)
  => [RectOptions]
  -> Aspect
  -> Rect Double
  -> [f (Rect Double)]
  -> Chart b
rectChart optss (Aspect asp) r rs =
  mconcat . zipWith rects optss $ fmap (projectRect r asp) <$> rs

-- | a chart of histograms scaled to its own range
--
-- > let ropts = [def {rectBorderSize=0}, def {rectBorderSize=0,rectColor=ucolor 0.3 0.3 0.3 0.2}]
-- > let pss = transpose [[exp (-(x**2)/2), 0.5 * exp (-(x**2)/8)] | x <- grid LowerPos (Range -5 5) 1000]
-- > let rss = (zipWith (\x y -> Rect x (x+1) 0 y) [0..]) <$> pss
-- > rectChart_ ropts widescreen rss
--
-- ![rectChart_ example](other/rectChart_Example.svg)
--
rectChart_ ::
     (Traversable f) => [RectOptions] -> Aspect -> [f (Rect Double)] -> Chart b
rectChart_ optss asp rs = rectChart optss asp (fold $ fold <$> rs) rs

-- | at some point, a color of a rect becomes more about data than stylistic option, hence the pixel.  Echewing rect border leaves a Pixel with no stylistic options to choose.
data Pixel = Pixel
  { pixelRect :: Rect Double
  , pixelColor :: AlphaColour Double
  }

-- | a pixel is a rectangle with a color.
--
-- > let opt = def {textColor=withOpacity black 0.8, textSize = 0.2}
-- > text_ opt "I'm a pixel!" <> pixel_ (Pixel one ublue)
--
-- ![pixel_ example](other/pixel_Example.svg)
--
pixel_ :: Pixel -> Chart b
pixel_ (Pixel (Rect x z y w) c) =
  unitSquare # moveTo (p2 (0.5, 0.5)) # scaleX (z - x) # scaleY (w - y) #
  moveTo (p2 (x, y)) #
  fcA c #
  lcA transparent #
  lw 0

-- | render multiple pixels
--
-- > pixels $ [Pixel (Rect (5*x) (5*x+0.1) (sin (10*x)) (sin (10*x) + 0.1)) (dissolve (2*x) ublue) | x <- grid OuterPos (Range 0 1) 100]
--
-- ![pixels example](other/pixelsExample.svg)
--
pixels :: (Traversable f) => f Pixel -> Chart b
pixels ps = mconcat $ toList $ pixel_ <$> ps

-- | a chart of pixels
pixelChart :: (Traversable f) => Aspect -> Rect Double -> [f Pixel] -> Chart b
pixelChart (Aspect asp) r pss =
  mconcat $ pixels . projectPixels r asp . toList <$> pss
  where
    projectPixels r0 r1 ps =
      zipWith Pixel (projectRect r0 r1 . pixelRect <$> ps) (pixelColor <$> ps)

-- | a chart of pixels scaled to its own range
--
-- > pixelChart_ asquare [[Pixel (Rect x (x+0.05) y (y+0.05)) (blend  (x*y+x*x) ugrey ublue) | x <- grid OuterPos (one::Range Double) 20, y <- grid OuterPos (one::Range Double) 20]]
--
-- ![pixelChart_ example](other/pixelChart_Example.svg)
--
pixelChart_ :: (Traversable f) => Aspect -> [f Pixel] -> Chart b
pixelChart_ asp ps = pixelChart asp (fold $ fold . map pixelRect <$> ps) ps

-- | options to pixelate a Rect using a function
data PixelationOptions = PixelationOptions
  { pixelationGradient :: Range (AlphaColour Double)
  , pixelationGrain :: Pair Int
  }

instance Default PixelationOptions where
  def = PixelationOptions (Range ugrey ublue) (Pair 20 20)

-- | transform a Rect into Pixels using a function over a Pair
pixelate ::
     PixelationOptions -> Rect Double -> (Pair Double -> Double) -> [Pixel]
pixelate (PixelationOptions (Range lc0 uc0) grain) xy f = zipWith Pixel g cs
  where
    g = gridSpace xy grain
    xs = f . mid <$> g
    (Range lx ux) = space xs
    cs = (\x -> blend ((x - lx) / (ux - lx)) lc0 uc0) <$> xs

-- | chart pixels using a function
-- This is a convenience function, and the example below is equivalent to the pixelChart_ example
--
-- > pixelateChart def asquare one (\(Pair x y) -> x*y+x*x)
--
pixelateChart ::
     PixelationOptions
  -> Aspect
  -> Rect Double
  -> (Pair Double -> Double)
  -> Chart b
pixelateChart opts asp xy f = pixelChart asp xy [pixelate opts xy f]
