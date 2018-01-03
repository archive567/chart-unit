{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | textual chart elements
module Chart.Text
  ( TextOptions(TextOptions)
  , TextFont(..)
  , textFont
  , text_
  , texts
  , textChart
  , textChart_
  , LabelOptions(LabelOptions)
  , labelled
  ) where

import Chart.Core
import qualified Data.Text as Text
import Diagrams.Prelude hiding (Color, D, scale)
import qualified Diagrams.TwoD.Size as D
import Graphics.SVGFonts hiding (textFont)
import Graphics.SVGFonts.ReadFont
import NumHask.Pair
import NumHask.Prelude hiding (rotate)
import NumHask.Rect

-- | text options
data TextOptions = TextOptions
  { size :: Double
  , alignH :: AlignH
  , alignV :: AlignV
  , color :: AlphaColour Double
  , textFillRule :: FillRule
  , rotation :: Double
  , font :: TextFont
  } deriving (Show, Generic)

instance Default TextOptions where
  def =
    TextOptions
      0.08
      AlignCenter
      AlignMid
      (withOpacity black 0.33)
      EvenOdd
      0
      Lin2

-- | ADT of fonts
data TextFont
  = Lin2
  | Lin
  deriving (Show)

-- | transform from chart-unit to SVGFonts rep of font
textFont :: TextFont -> PreparedFont Double
textFont Lin = lin
textFont Lin2 = lin2

-- | Create a textual chart element
--
-- > text_ def "Welcome to chart-unit!"
--
-- ![text_ example](other/text_Example.svg)
--
text_ :: TextOptions -> Text -> Chart b
text_ (TextOptions s ah av c fr rot f) t =
  moveTo (p_ (Pair (alignHTU ah * D.width path) (alignVTU av * D.height path))) $
  path # fcA c # lw 0 # fillRule fr # rotate (rot @@ deg)
  where
    path =
      textSVG_ (TextOpts (textFont f) INSIDE_H KERN False s s) (Text.unpack t)

-- | Create positioned text from a list
--
-- > ts :: [(Text, Pair Double)]
-- > ts = zip
-- >   (map Text.singleton ['a' .. 'z'])
-- >   [Pair (sin (x * 0.1)) x | x <- [0 .. 25]]
-- >
-- > textsExample :: Chart b
-- > textsExample = texts def ts
--
-- ![texts example](other/textsExample.svg)
--
texts :: (R2 r) => TextOptions -> [(Text, r Double)] -> Chart b
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
-- > textChart_Example :: Chart b
-- > textChart_Example =
-- >   textChart_ [#size .~ 0.33 $ def] widescreen [ts]
--
-- ![textChart_ example](other/textChart_Example.svg)
--
textChart_ :: [TextOptions] -> Rect Double -> [[(Text, Pair Double)]] -> Chart b
textChart_ optss asp xyss =
  textChart optss asp (range $ fmap snd . toList <$> xyss) xyss

-- | A label is a text element attached to a chart element
data LabelOptions = LabelOptions
  { text :: TextOptions
  , orientation :: Pair Double -- ^ direction of label
  , gap :: Double -- ^ distance to label
  } deriving (Show, Generic)

instance Default LabelOptions where
  def = LabelOptions def (Pair 0 1) 0.05

-- | Label a chart element with some text
--
-- > labelledExample :: Chart b
-- > labelledExample =
-- >   labelled
-- >     (LabelOptions
-- >        (#alignH .~ AlignLeft $ #rotation .~ 45 $ def)
-- >        (Pair 1 1)
-- >        0.05)
-- >     "a label"
-- >     (glyph_ def)
--
-- ![labelled example](other/labelledExample.svg)
--
labelled :: LabelOptions -> Text -> Chart b -> Chart b
labelled (LabelOptions texto o g) t ch =
  beside (r_ o) (beside (r_ o) ch (strut (r_ o) # scale g)) (text_ texto t)
