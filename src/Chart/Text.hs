{-# OPTIONS_GHC -Wall #-}
-- | textual chart elements
module Chart.Text
  ( TextOptions(..)
  , text_
  , texts
  , textChart
  , textChart_
  , LabelOptions(..)
  , labelled
  ) where

import Chart.Core
import qualified Data.Text as Text
import Diagrams.Prelude hiding (Color, D, scale)
import qualified Diagrams.TwoD.Size as D
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont
import NumHask.Pair
import NumHask.Prelude hiding (rotate)
import NumHask.Rect

-- | text options
data TextOptions = TextOptions
  { textSize :: Double
  , textAlignH :: AlignH
  , textColor :: AlphaColour Double
  , textFillRule :: FillRule
  , textRotation :: Double
  , textFont :: PreparedFont Double
  }

instance Default TextOptions where
  def = TextOptions 0.08 AlignCenter (withOpacity black 0.33) EvenOdd 0 lin2

-- | Create a textual chart element
--
-- > let text_Example = text_ def "Welcome to chart-unit!"
--
-- ![text_ example](other/text_Example.svg)
--
text_ :: TextOptions -> Text -> Chart b
text_ (TextOptions s a c fr rot f) t =
  moveTo (p_ (Pair (alignHTU a * D.width path) 0)) $
  path # fcA c # lw 0 # fillRule fr # rotate (rot @@ deg)
  where
    path = textSVG_ (TextOpts f INSIDE_H KERN False s s) (Text.unpack t)

-- | Creatye positioned text from a list
--
-- > let ts = map (Text.singleton) ['a'..'z']
-- > texts def ts [Pair (0.05*x) 0 |x <- [0..5]]
--
-- ![texts example](other/textsExample.svg)
--
texts :: (R2 r) => TextOptions -> [Text] -> [r Double] -> Chart b
texts opts ts ps = mconcat $ zipWith (\p t -> positioned p (text_ opts t)) ps ts

-- | A chart of text
textChart ::
     (Traversable f)
  => [TextOptions]
  -> Aspect
  -> Rect Double
  -> [f (Text, Pair Double)]
  -> Chart b
textChart optss (Aspect asp) r xyss =
  mconcat $
  getZipList $
  texts <$> ZipList optss <*> ZipList (map fst . toList <$> xyss) <*>
  ZipList (projectss r asp (fmap snd . toList <$> xyss))

-- | A chart of text scaled to its own range
--
-- > import qualified Data.Text as Text
-- > let ps = [Pair (sin (x*0.1)) x | x<-[0..25]]
-- > textChart_ (repeat $ def {textSize=0.33}) widescreen [zip ts ps]
--
-- ![textChart_ example](other/textChart_Example.svg)
--
textChart_ :: [TextOptions] -> Aspect -> [[(Text, Pair Double)]] -> Chart b
textChart_ optss asp xyss =
  textChart optss asp (range $ fmap snd . toList <$> xyss) xyss

-- | A label is a text element attached to a chart element
data LabelOptions = LabelOptions
  { labelText :: TextOptions
  , labelOrientation :: Pair Double -- ^ direction of label
  , labelGap :: Double -- ^ distance to label
  }

instance Default LabelOptions where
  def = LabelOptions def (Pair 0 1) 0.05

-- | Label a chart element with some text
--
-- > let lopts = def {textAlignH = AlignLeft, textRotation=45}
-- > labelled (LabelOptions lopts (Pair 1 1) 0.05) "a label" (glyph_ def)
--
-- ![labelled example](other/labelledExample.svg)
--
labelled :: LabelOptions -> Text -> Chart b -> Chart b
labelled (LabelOptions texto o g) t ch =
  beside (r_ o) (beside (r_ o) ch (strut (r_ o) # scale g)) (text_ texto t)
