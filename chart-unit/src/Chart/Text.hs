{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | textual chart elements
module Chart.Text
  ( TextOptions(..)
  , defaultTextOptions
  , TextPathOptions(..)
  , defaultTextPathOptions
  , TextSvgOptions(..)
  , defaultTextSvgOptions
  , TextType(..)
  , TextFont(..)
  , UFillRule(..)
  , textFont
  , text_
  , texts
  , textChart
  , textChart_
  , LabelOptions(..)
  , defaultLabelOptions
  , labelled
  ) where

import Chart.Core
import Chart.Rect
import Diagrams.Prelude hiding (Color, D, scale, (<>), (*.))
import Graphics.SVGFonts hiding (textFont)
import Graphics.SVGFonts.ReadFont
import NumHask.Pair
import NumHask.Prelude hiding (rotate)
import NumHask.Rect
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as Text
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.TwoD.Text as D

-- | options specific to text as an SVG path
newtype TextPathOptions = TextPathOptions
  { font :: TextFont
  } deriving (Show, Eq, Generic)

defaultTextPathOptions :: TextPathOptions
defaultTextPathOptions = TextPathOptions Lin2

-- | ADT of fonts
data TextFont
  = Lin2
  | FromFontFile Text
  deriving (Show, Eq, Generic)

-- | transform from chart-unit to SVGFonts rep of font
textFont :: TextFont -> PreparedFont Double
textFont Lin2 = lin2
textFont (FromFontFile f) = unsafePerformIO (loadFont (Text.unpack f))

-- | options specific to text as SVG text
data TextSvgOptions = TextSvgOptions
  { nudgeSize :: Double
  , nudgeBottom :: Double
  , nudgeMid :: Double
  , nudgeTop :: Double
  , svgFont :: Maybe Text
  , sizeVert :: Double -- ^ approximate divisor of vertical size
  , sizeHori :: Double -- ^ approximate divisor of horizontal size per character
  , textBox :: RectOptions -- ^ bounding box 
  } deriving (Show, Eq, Generic)

defaultTextSvgOptions :: TextSvgOptions
defaultTextSvgOptions = TextSvgOptions 0.78 0.25 -0.10 0.25 Nothing 1.1 0.55 clear

-- | text as a path or as svg text
data TextType
  = TextPath TextPathOptions
  | TextSvg TextSvgOptions
  deriving (Show, Eq, Generic)

data UFillRule = UWinding | UEvenOdd deriving (Show, Eq, Generic)

uFillRule :: UFillRule -> FillRule
uFillRule UWinding = Winding
uFillRule UEvenOdd = EvenOdd

-- | text options
data TextOptions = TextOptions
  { size :: Double  -- ^ size as ratio to overall chart size (default: 0.08)
  , alignH :: AlignH -- ^ horizontal alignment (default: 'AlignCenter')
  , alignV :: AlignV -- ^ vertical alignment (default: 'AlignMid')
  , color :: UColor Double -- ^ default: greyish
  , textFillRule :: UFillRule -- ^ default: 'EvenOdd'
  , rotation :: Double -- ^ in degrees from the horozontal (default: 0 degrees)
  , textType :: TextType -- ^ default: 'TextPath' def
  } deriving (Show, Eq, Generic)

defaultTextOptions :: TextOptions
defaultTextOptions =
    TextOptions
      0.08
      AlignCenter
      AlignMid
      (UColor 0 0 0 0.33)
      UEvenOdd
      0
      (TextSvg defaultTextSvgOptions)

-- | Create a textual chart element
--
-- > text_ def "Welcome to chart-unit!"
--
-- ![text_ example](other/text_Example.svg)
--
-- Text can be either SVG text or text rendered as an SVG path.  Text as SVG can be overridden by an opinionated browser.
-- SVG Text not have a size, according to diagrams, and according to the svg standards for all I know.
-- textSvg corrects for this by adding an approximately bounding rectangle so that size is forced.
--
-- > text_SvgExample :: Chart b
-- > text_SvgExample = text_
-- >   (#textType .~ TextSvg (#textBox .~ def $ #svgFont .~ Just "Comic Sans MS" $ def) $
-- >   #size .~ 0.2 $
-- >   def)
-- >   "abc & 0123 & POW!"
--
-- ![text_Svg example](other/text_SvgExample.svg)
--
-- Text as an SVG path can use the fonts supplied in [SVGFonts](https://hackage.haskell.org/package/SVGFonts), follow the instructions there to make your own, or use the [Hasklig](https://github.com/i-tu/Hasklig) font supplied in chart-unit.
--
-- > text_PathExample :: Chart b
-- > text_PathExample = text_
-- >   (#textType .~ TextPath (#font .~ FromFontFile "other/Hasklig-Regular.svg" $ def) $
-- >    #size .~ 0.2 $
-- >    def)
-- >    "0123 <*> <$> <| |> <> <- -> => ::"
--
-- ![text_Path example](other/text_PathExample.svg)
--
text_ :: TextOptions -> Text -> Chart b
text_ (TextOptions s ah av c fr rot (TextPath (TextPathOptions f))) t =
  moveTo (p_ (Pair (alignHTU ah * D.width path) (av' * D.height path))) $
  path # fcA (acolor c) # lw 0 # fillRule (uFillRule fr) # rotate (rot @@ deg)
  where
    path =
      textSVG_ (TextOpts (textFont f) INSIDE_H KERN False s s) (Text.unpack t)
    av' = case av of
      AlignBottom -> 0
      AlignMid -> -0.25
      AlignTop -> -0.5
text_ (TextOptions s ah av c fr rot (TextSvg (TextSvgOptions ns nb nm nt f v h bx))) t =
  txt #
  moveTo (p_ (Pair 0 mv)) #
  Chart.Core.scaleX (s * ns) #
  Chart.Core.scaleY (s * ns) #
  maybe identity (D.font . Text.unpack) f #
  fcA (acolor c) #
  lw 0 #
  fillRule (uFillRule fr) #
  rotate (rot @@ deg)
  where
    txt =
      D.alignedText ah'' av'' (Text.unpack t) <>
      Chart.Core.scaleX (h*fromIntegral(Text.length t))
      (Chart.Core.scaleY v $
       moveOriginTo (p_ (Pair boxh boxv)) $
       rect_ bx one)
    (ah'', boxh) = case ah of
      AlignLeft -> (0, -0.5)
      AlignCenter -> (0.5, 0)
      AlignRight -> (1, 0.5)
    (av'', mv, boxv) = case av of
      AlignBottom -> (0.5, nb, 0)
      AlignMid -> (0.5, nm, 0)
      AlignTop -> (1, nt, 0.5)

-- | Create positioned text from a list
texts :: (R2 r) =>
  TextOptions -> [(Text, r Double)] -> Chart b
texts opts ts = mconcat $ (\(t, p) -> positioned p (text_ opts t)) <$> ts

-- | A chart of text
textChart ::
    (Traversable f)
  => [TextOptions]
  -> Rect Double
  -> Rect Double
  -> [f (Text, Pair Double)]
  -> Chart b
textChart optss asp r xyss =
  mconcat $ getZipList $ texts <$> ZipList optss <*> ZipList (zipWith zip ts ps)
  where
    ts = toList . fmap fst <$> xyss
    ps = projectss r asp $ toList . fmap snd <$> xyss

-- | A chart of text scaled to its own range
--
-- > ts :: [(Text, Pair Double)]
-- > ts = zip
-- >   (map Text.singleton ['a' .. 'z'])
-- >   [Pair (sin (x * 0.1)) x | x <- [0 .. 25]]
-- >
-- > textChart_Example :: Chart b
-- > textChart_Example =
-- >   textChart_ [#size .~ 0.33 $ def] widescreen [ts]
--
-- ![textChart_ example](other/textChart_Example.svg)
--
textChart_ ::
  [TextOptions] -> Rect Double -> [[(Text, Pair Double)]] -> Chart b
textChart_ optss asp xyss =
  textChart optss asp (range $ fmap snd . toList <$> xyss) xyss

-- | A label is a text element attached to a chart element
data LabelOptions = LabelOptions
  { text :: TextOptions
  , orientation :: Pair Double -- ^ direction of label
  , gap :: Double -- ^ distance to label
  } deriving (Show, Eq, Generic)

defaultLabelOptions :: LabelOptions
defaultLabelOptions = LabelOptions defaultTextOptions (Pair 0 1) 0.05

-- | Label a chart element with some text
--
-- > labelledExample :: Chart b
-- > labelledExample = D.pad 1.1 $
-- >   labelled (LabelOptions
-- >     (#alignH .~ AlignLeft $ #rotation .~ 45 $ def) (Pair 1 1) 0.02)
-- >   "a label"
-- >   (glyph_ def)
--
-- ![labelled example](other/labelledExample.svg)
--
labelled :: LabelOptions -> Text -> Chart b -> Chart b
labelled (LabelOptions texto o g) t ch =
  beside (r_ o) (beside (r_ o) ch (strut (r_ o) # scale g)) (text_ texto t)

