{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | textual chart elements
module Chart.Text
  ( TextOptions(TextOptions)
  , TextFont(..)
  , textFont
  , text_
  , textPath
  , textSvg
  , texts
  , textChart
  , textChart_
  , LabelOptions(LabelOptions)
  , labelled
  ) where

import Chart.Core
import qualified Data.Text as Text
import Diagrams.Prelude hiding (Color, D, scale, (<>))
import qualified Diagrams.TwoD.Size as D
import qualified Diagrams.TwoD.Text as D
import Data.Generics.Product
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
  , usePath :: Bool
  , nudgeSize :: Double
  , nudgeBottom :: Double
  , nudgeMid :: Double
  , nudgeTop :: Double
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
      True
      0.78
      0.25
      -0.10
      0.25

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
text_ opts t = bool textSvg textPath (opts ^. field @"usePath") opts t

textPath :: TextOptions -> Text -> Chart b
textPath (TextOptions s ah av c fr rot f _ _ _ _ _) t =
  moveTo (p_ (Pair (alignHTU ah * D.width path) (av' * D.height path))) $
  path # fcA c # lw 0 # fillRule fr # rotate (rot @@ deg)
  where
    path =
      textSVG_ (TextOpts (textFont f) INSIDE_H KERN False s s) (Text.unpack t)
    av' = case av of
      AlignBottom -> 0
      AlignMid -> -0.25
      AlignTop -> -0.5

textSvg :: TextOptions -> Text -> Chart b
textSvg (TextOptions s ah av c fr rot _ _ nudge nb nm nt) t =
  txt #
  moveTo (p_ (Pair 0 mv)) #
  Chart.Core.scaleX (s * nudge) #
  Chart.Core.scaleY (s * nudge) #
  fcA c # lw 0 # fillRule fr # rotate (rot @@ deg)
  where
    txt = D.alignedText ah'' av'' (Text.unpack t)
    ah'' = case ah of
      AlignLeft -> 0
      AlignCenter -> 0.5
      AlignRight -> 1
    (av'',mv) = case av of
      AlignBottom -> (0.5, nb)
      AlignMid -> (0.5, nm)
      AlignTop -> (1, nt)

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
