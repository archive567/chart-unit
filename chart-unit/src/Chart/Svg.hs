{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Svg rendering
module Chart.Svg
  ( SVG
  , SvgOptions(..)
  , defaultSvgOptions
  , renderSvg
  , toText
  , toFile
  , fileSvg
    -- * scratch pad
  , scratch
  ) where

import Chart.Core
import Diagrams.Backend.SVG
import Graphics.Svg hiding (toText)
import NumHask.Pair
import NumHask.Prelude
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Text
import qualified Diagrams.Prelude as D

{- | ToDo: Mathjax capability would be awesome but the html that mathjax generates is not valid svg unless it is wrapped in <foreignObject> instead of <text>.  <foreignObject> also cant be a sub-element of a text element.  This means extensive digging into Diagrams innards etc

https://stackoverflow.com/questions/15962325/mathjax-inside-svg

-}

-- | Svg options
data SvgOptions = SvgOptions
  { size :: Pair Double
  , svgId :: Text
  , attributes :: [Attribute]
  , includeDocType :: Bool
  } deriving (Show, Eq, Generic)

defaultSvgOptions :: SvgOptions
defaultSvgOptions = SvgOptions (Pair 600 400) "" [] True

renderSvg :: SvgOptions -> Chart SVG -> Element
renderSvg (SvgOptions (Pair x y) svgid atts dt) ch =
  D.renderDia
  SVG
  (SVGOptions
   (D.mkSizeSpec (D.V2 (Just x) (Just y))) Nothing svgid atts dt)
  ch

-- | render svg to text
toText :: SvgOptions -> Chart SVG -> Lazy.Text
toText opts ch = prettyText $ renderSvg opts ch

-- | write an svg to file
toFile :: FilePath -> Element -> IO ()
toFile f = Text.writeFile f . prettyText

-- | write a chart to an svg file
fileSvg :: FilePath -> SvgOptions -> Chart SVG -> IO ()
fileSvg f opts ch = toFile f $ renderSvg opts ch

-- | a scratch pad
scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratch.svg" defaultSvgOptions
