{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
 
-- | Experimental Chart ADT
--
module Chart.Svg
  ( SvgOptions(..)
  , renderSvg
  , toFile
  , fileSvg
    -- * scratch pad
  , scratch
  ) where

import Chart.Core
import Data.Default
import Diagrams.Backend.SVG
import Graphics.Svg
import NumHask.Pair
import NumHask.Prelude
import qualified Data.Text.Lazy.IO as Text
import qualified Diagrams.Prelude as D

-- | Svg options
data SvgOptions = SvgOptions
  { size :: Pair Double
  , includeMathjax :: Bool
  , svgId :: Text
  , attributes :: [Attribute]
  , includeDocType :: Bool
  } deriving (Show, Generic)

instance Default SvgOptions where
  def = SvgOptions (Pair 600 400) False "" [] True

renderSvg :: SvgOptions -> Chart SVG -> Element
renderSvg (SvgOptions (Pair x y) math svgid atts dt) ch =
  D.renderDia
  SVG
  (SVGOptions
   (D.mkSizeSpec (D.V2 (Just x) (Just y))) defs svgid atts dt)
  ch
  where
    defs = case math of
      False -> Nothing
      True -> Just (script_ [Type_ <<- "text/x-mathjax-config", makeAttribute "src" "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=default", makeAttribute "async" "absurd"] "\n    MathJax.Hub.Config({\n        extensions: [\"tex2jax.js\", \"TeX/AMSmath.js\"],\n        jax: [\"input/TeX\", \"output/SVG\"],\n    })")

      -- script type="text/javascript" async src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.1/MathJax.js?config=TeX-MML-AM_CHTML"

-- https://stackoverflow.com/questions/15962325/mathjax-inside-svg

-- | write an svg to file
toFile :: FilePath -> Element -> IO ()
toFile f = Text.writeFile f . prettyText

-- | write a chart to an svg file
fileSvg :: FilePath -> SvgOptions -> Chart SVG -> IO ()
fileSvg f opts ch = toFile f $ renderSvg opts ch

-- | a scratch pad
scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratch.svg" def
