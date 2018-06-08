{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | examples used in haddock docs
import Chart
import Control.Lens
import NumHask.Prelude
import qualified Data.Text as Text
import qualified Diagrams.Prelude as D
import Data.Generics.Product

-- * Chart.Core examples 
scaleExample :: IO ()
scaleExample =
  fileSvg (coreDir <> "scaleExample.svg")
  (field @"size" .~ Pair 300 120 $ defaultSvgOptions) $
  withHud
  defaultHudOptions
  widescreen
  (Rect 0 12 0 0.2)
  (lineChart (repeat defaultLineOptions))
  (vlineOneD ((0.01*) <$> [0..10]))

-- * example charts look n feel
hudbits :: Text -> Maybe Text -> [Text] -> [LegendType] -> HudOptions -> HudOptions
hudbits t subt ts ls x =
  field @"titles" .~
  [(field @"place" .~ PlaceLeft $
    field @"align" .~ AlignLeft $
    field @"text" . field @"rotation" .~ 90 $
    field @"text" . field @"size" .~ 0.2 $
    field @"text" . field @"color" .~ ucolor (d3Colors1 0 `withOpacity` 1) $
    defaultTitleOptions, t)] <>
  (case subt of
     Nothing -> []
     Just subt' -> 
       [(field @"place" .~ PlaceBottom $
         field @"align" .~ AlignRight $
         field @"text" . field @"rotation" .~ 0 $
         field @"text" . field @"size" .~ 0.12 $
         field @"text" . field @"color" .~ ucolor (d3Colors1 0 `withOpacity` 1) $
         defaultTitleOptions, subt')]) $ 
  field @"legends" .~
  [field @"chartType" .~ zip ls ts $
   field @"align" .~ AlignRight $
   field @"text" . field @"size" .~ 0.2 $
   defaultLegendOptions ] $
  field @"axes" . each . field @"gap" .~ 0.1 $
  x

-- * Chart.Text examples
text_Example :: Chart b
text_Example =
  text_ (field @"textType" .~ TextPath defaultTextPathOptions $ defaultTextOptions)
  "Welcome to chart-unit!"

text_SvgExample :: Chart b
text_SvgExample = text_
  (field @"textType" .~
   TextSvg
   ( field @"textBox" .~ defaultRectOptions $
     field @"svgFont" .~ Just "Comic Sans MS" $
     defaultTextSvgOptions) $
   field @"size" .~ 0.2 $
   defaultTextOptions)
  "abc & 0123 & POW!"

text_PathExample :: Chart b
text_PathExample = text_
  (field @"textType" .~
   TextPath
   (field @"font" .~ FromFontFile ("./chart-unit/other/Hasklig-Regular.svg") $
    defaultTextPathOptions) $
   field @"size" .~ 0.2 $
   defaultTextOptions)
   "0123 <*> <$> <| |> <> <- -> => ::"

ts :: [(Text, Pair Double)]
ts = zip
  (map Text.singleton ['a' .. 'z'])
  [Pair (sin (x * 0.1)) x | x <- [0 .. 25]]

textChart_Example :: Chart b
textChart_Example =
  D.pad 1.1 $ textChart_ [field @"size" .~ 0.33 $ defaultTextOptions] widescreen [ts]

labelledExample :: Chart b
labelledExample = D.pad 1.1 $
  labelled (LabelOptions
    (field @"alignH" .~ AlignLeft $ field @"rotation" .~ 45 $ defaultTextOptions) (Pair 1 1) 0.02)
  "a label"
  (glyph_ defaultGlyphOptions)

textHudExample :: Chart b
textHudExample =
  hud
  (hudbits "Text Chart" (Just "text and glyphs have a similar feel") [] []
   defaultHudOptions)
  widescreen
  (range ts)

-- * Chart.Glyph examples
glyph_Example :: Chart b
glyph_Example = glyph_ defaultGlyphOptions

glyphsExample :: Chart b
glyphsExample = glyphs defaultGlyphOptions (dataXY sin (Range 0 (2*pi)) 30)

gopts :: [GlyphOptions]
gopts =
  [ field @"borderSize" .~ 0.001 $
    field @"size" .~ 0.1 $
    defaultGlyphOptions
  , field @"borderSize" .~ 0.001 $
    field @"size" .~ 0.1 $
    field @"color" .~ ucolor (rybColor 7 `withOpacity` 0.4) $
    field @"shape" .~ Triangle $
    defaultGlyphOptions
  ]

gdata :: [[Pair Double]]
gdata =
  [ dataXY sin (Range 0 (2*pi)) 30
  , dataXY cos (Range 0 (2*pi)) 30
  ]

glyphChart_Example :: Chart b
glyphChart_Example = glyphChart_ gopts widescreen gdata

lglyphsExample :: Chart b
lglyphsExample =
  lglyphs defaultLabelOptions defaultGlyphOptions $
  zip (show <$> [0 ..]) [Pair (x / 10) (sin x / 10) | x <- [0 .. 10]]

lgdata :: [(Text, Pair Double)]
lgdata =
  (\(p@(Pair x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$>
    (Pair <$> [0 .. 5] <*> [0 .. 5] :: [Pair Int])

lglyphChart_Example :: Rect Double -> Chart b
lglyphChart_Example a =
  lglyphChart_
  [field @"gap" .~ 0.015 $ field @"text" . field @"size" .~ 0.12 $
   defaultLabelOptions]
  [field @"color" .~ ublack $
   field @"borderSize" .~ 0 $
   field @"size" .~ 0.01 $
   defaultGlyphOptions]
  a
  [lgdata]

glyphHudExample :: Chart b
glyphHudExample = 
  hud
  (field @"legends" . each . field @"align" .~ AlignLeft $
   hudbits "Glyph Chart" (Just "text elements are paths not svg text")
   ["sin", "cos"]
   (LegendGlyph <$> gopts) $
   field @"axes" .~
    [ field @"label" . field @"text" . field @"size" .~ 0.3 $
      field @"tickStyle" .~ TickPlaced pis $
      field @"label" . field @"text" . field @"textType" .~
      TextPath (TextPathOptions (FromFontFile "./chart-unit-examples/other/SourceCodePro-Regular.svg")) $
      defXAxis
    ] $ defaultHudOptions)
  widescreen
  (range gdata)
  where
    pis =
      [ (0,"zero")
      , (pi/2, "π/2")
      , (pi, "π")
      , (3 * pi / 2, "3π/2")
      , (2*pi,"2π")
      ]

lglyphHudExample :: Chart b
lglyphHudExample = hud
  (field @"titles" . each . _1 . field @"gap" .~ 0.2 $
   hudbits "LGlyph Chart" (Just "Glyphs with text labels are very useful") [] [] $
   field @"axes" .~ [] $
   defaultHudOptions)
  widescreen
  (range (fmap snd <$> [lgdata]))

-- * Chart.Lines examples
linesExample :: Int -> Chart b
linesExample n =
  lines
  (field @"color" .~ ucolor (red `withOpacity` 0.5) $ defaultLineOptions)
  (dataXY cos (Range 0 (4*pi)) n)

ls :: [[Pair Double]]
ls =
  map (uncurry Pair) <$>
  [ [(0.0, 1.0), (1.0, 1.0), (2.0, 5.0)]
  , [(0.0, 0.0), (3.0, 3.0)]
  , [(0.5, 4.0), (0.5, 0)]
  ]

lopts :: [LineOptions]
lopts =
  zipWith
  (\x y -> LineOptions x (ucolor $ withOpacity (d3Colors1 y) 0.6))
  [0.01, 0.02, 0.005]
  [0,1,2]

lineChart_Example :: Chart b
lineChart_Example = lineChart_ lopts sixbyfour ls

gopts3 :: [GlyphOptions]
gopts3 =
  zipWith
  (\x y ->
     field @"color" .~ ucolor (withOpacity (d3Colors1 x) 0.2) $
     field @"borderColor" .~ ucolor (withOpacity (d3Colors1 x) 1) $
     field @"borderSize" .~ 0.005 $
     field @"shape" .~ y $
     field @"size" .~ 0.08 $
     defaultGlyphOptions)
  [6,8,2]
  [Triangle, Square, Circle]

glineChart_Example :: Chart b
glineChart_Example = glineChart_ lopts gopts3 sixbyfour ls

lineHudExample :: Chart b
lineHudExample = 
  hud
  (hudbits "Line Chart" Nothing ["hockey stick", "slope", "vertical"]
   ((`LegendLine` 0.05) <$> lopts) defaultHudOptions)
  sixbyfour
  (range ls)

glineHudExample :: Chart b
glineHudExample = renderChart
  (ChartOptions (Just (Rect 0 5 0 5)) sixbyfour
  [ GlineChart
    (getZipList $
      (\x y z -> ((x,y),z)) <$> ZipList
      lopts <*> ZipList
      gopts3 <*> ZipList
      ls)
  , HudChart
    (field @"legends" . each . field @"gap" .~ 0.2 $
     field @"titles" . each . _1 . field @"gap" .~ 0.2 $
     hudbits "Gline Chart" Nothing ["triangle", "square", "circle"]
     (zipWith (\x y -> LegendGLine x y 0.1) gopts3 lopts) $
     field @"axes" .~ [] $
     defaultHudOptions)
  , LGlyphChart
    [ ( (field @"gap" .~ 0.015 $ field @"text" . field @"size" .~ 0.12 $ defaultLabelOptions
      , field @"color" .~ ublack $
        field @"borderSize" .~ 0 $
        field @"size" .~ 0.01 $
        defaultGlyphOptions)
      , lgdata)]
  ])

-- * Chart.Rect examples
rect_Example :: Double -> Chart b
rect_Example n =
  labelled (opts (Pair n 1)) "z,w" $
  labelled (opts (Pair n -1)) "z,y" $
  labelled (opts (Pair (-n) 1)) "x,w" $
  labelled (opts (Pair (-n) -1)) "x,y" $
  rect_ defaultRectOptions (Ranges (n *. one) one)
  where
    opts :: Pair Double -> LabelOptions
    opts o =
      field @"text" %~
        ( (field @"color" .~ UColor 0 0 0 0.8) .
          (field @"size" .~ 0.3)) $
      field @"orientation" .~ o $
      defaultLabelOptions

rectsExample :: Chart b
rectsExample =
  rects defaultRectOptions (rectBars 0.1 [1, 2, 3, 5, 8, 0, -2, 11, 2, 1])

ropts :: [RectOptions]
ropts =
  [ field @"borderSize" .~ 0 $ defaultRectOptions
  , field @"borderSize" .~ 0 $ field @"color" .~ UColor 0.3 0.3 0.3 0.2 $ defaultRectOptions
  ]
 
rss :: [[Rect Double]]
rss =
  [ rectXY (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50
  , rectXY (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
  ]

rectChart_Example :: Chart b
rectChart_Example = rectChart_ ropts widescreen rss

pixel_Example :: Chart b
pixel_Example = text_ opt "I'm a pixel!" <> pixel_ (Pixel one ublue)
  where
    opt =
      field @"color" .~ UColor 0 0 0 0.8 $
      field @"size" .~ 0.2 $
      defaultTextOptions

pixelsExample :: Chart b
pixelsExample =
  pixels
    [ Pixel
      (Rect (5 * x) (5 * x + 0.1) (sin (10 * x)) (sin (10 * x) + 0.1))
      (ucolor $ dissolve (2 * x) (acolor ublue))
    | x <- grid OuterPos (Range 0 1) 100
    ]

pixelChart_Example :: Chart b
pixelChart_Example =
  pixelChart_ asquare
  [(\(r,c) ->
      Pixel r
      (ucolor $ blend c
       (acolor $ UColor 0.47 0.73 0.86 1)
       (acolor $ UColor 0.01 0.06 0.22 1)
      )) <$>
   rectF (\(Pair x y) -> (x+y)*(x+y))
   one (Pair 40 40)]

pixelateChartExample :: Chart b
pixelateChartExample =
  pixelateChart defaultPixelationOptions asquare one (\(Pair x y) -> (x+y)*(x+y))

rectHudExample :: Chart b
rectHudExample =
  hud
  (field @"legends" . each . field @"place" .~ PlaceBottom $
   field @"legends" . each . field @"align" .~ AlignCenter $
   hudbits "Rect Chart" Nothing ["blue gaussian", "grey wider distribution"]
   ((`LegendRect` 0.05) <$> ropts) $
   field @"axes" .~ [defXAxis] $
   defaultHudOptions)
  widescreen
  (fold $ fold rss)

pixelHudExample :: Chart b
pixelHudExample =
  hud
  (hudbits "Pixel Chart" Nothing ["red", "blue"]
   ((`LegendPixel` 0.05) <$> ropts) defaultHudOptions)
  asquare
  one

-- * Chart.Arrow examples
arrowsExample :: Chart b
arrowsExample =
  arrows
  (field @"maxLength" .~ 0.5 $
   field @"maxHeadLength" .~ 0.2 $
   field @"maxStaffWidth" .~ 0.01 $
   defaultArrowOptions)
  [ Arrow (Pair x (sin (5 * x))) (Pair x (cos x))
  | x <- grid MidPos (one :: Range Double) 100
  ]

arrowChart_Example :: Chart b
arrowChart_Example = arrowChart_ [defaultArrowOptions] asquare [as]
  where
    as =
      normArrows
      [ Arrow (Pair x y) (Pair (sin 1 / x + 0.0001) (cos 1 / y + 0.0001))
      | x <- grid MidPos (one :: Range Double) 20
      , y <- grid MidPos (one :: Range Double) 20
      ]

arrowHudExample :: Chart b
arrowHudExample = 
  hud
  (hudbits "Arrow Chart" Nothing ["this way up"] []
-- ((`LegendArrow` 0.05) <$> [def]) $
   defaultHudOptions)
  asquare
  one

-- * Chart.Hud examples
hudExample :: Chart b
hudExample = hud defaultHudOptions sixbyfour one

withHudExample :: Chart b
withHudExample = withHud_ hopts sixbyfour (lineChart lopts) ls
  where
    hopts =
      field @"titles" .~ [(defaultTitleOptions, "withHud Example")] $
      field @"legends" .~
      [ field @"chartType" .~ zipWith
        (\x y -> (LegendLine x 0.05, y))
        lopts
        ["line1", "line2", "line3"]
        $ defaultLegendOptions
      ] $ defaultHudOptions

axisExample :: Chart b
axisExample = axis aopts one (Range 0 100000)
  where
    aopts :: AxisOptions
    aopts =
      field @"label" . field @"text" %~
      ((field @"rotation" .~ -45) .
       (field @"size" .~ 0.06) .
       (field @"alignH" .~ AlignLeft)) $
      field @"gap" .~ 0.0001 $
      defXAxis

legends' :: [(LegendType, Text)]
legends' =
  [(LegendText defaultTextOptions, "legend")] <>
  [(LegendPixel (blob ublue) 0.05, "pixel")] <>
  -- [(LegendArrow (def & field @"minStaffWidth" .~ 0.01 & field @"minHeadLength" .~ 0.03) 0.05, "arrow")] <>
  [(LegendRect defaultRectOptions 0.05, "rect")] <>
  [(LegendGLine defaultGlyphOptions defaultLineOptions 0.10, "glyph+line")] <>
  [(LegendGlyph defaultGlyphOptions, "just a glyph")] <>
  zipWith
    (\x y -> (LegendLine x 0.05, y))
    lopts
    ["short", "much longer name", "line 3"]

legendExample :: Chart b
legendExample = legend $ field @"chartType" .~ legends' $ defaultLegendOptions

-- * Chart.Bar examples
barExample :: Chart b
barExample  =
  barChart defaultBarOptions (BarData [ys] Nothing Nothing) <>
  hud
  ( field @"titles" .~ [(defaultTitleOptions, "Bar Chart")] $
    field @"axes" .~
    [ field @"tickStyle" .~
      TickLabels labels' $
      defXAxis
    ] $
    defaultHudOptions)
  sixbyfour
  (fold (abs <$> rs))
  where
    labels' = fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..]
    rs = rectBars 0.1 ys
    ys = [1,2,3,5,8,0,-2,11,2,1]

-- * difference between svg text and path text
testTextDiffs :: Double -> Double -> Text -> (Double, Double, Double) -> Maybe Text -> Chart b
testTextDiffs s ns txt (nb, nm, nt) fnt =
  D.pad 1.1 $ vert identity $ D.centerXY .
  (\(ah,av,txt) ->
     D.showOrigin' (D.OriginOpts red 0.001 0.001)
      (text_
      (field @"alignH" .~ ah $
       field @"alignV" .~ av $
       field @"size" .~ s $
       field @"color" .~ ucolor (red `withOpacity` 1) $
       defaultTextOptions) txt) <>
     D.showOrigin' (D.OriginOpts blue 0.003 0.003)
      (text_
      (field @"alignV" .~ av $
       field @"alignH" .~ ah $
       field @"size" .~ s $
       field @"textType" .~ TextSvg (TextSvgOptions ns nb nm nt fnt 1.1 0.55 clear) $
       defaultTextOptions) txt)) <$>
  ((\x y -> (x,y,txt)) <$>
   [AlignLeft, AlignCenter, AlignRight] <*>
   [AlignBottom, AlignMid, AlignTop])

coreDir :: FilePath
coreDir = "./chart-unit/other/"

exampleDir :: FilePath
exampleDir = "./chart-unit-examples/other/"

coreSvg :: FilePath -> Pair Double -> Chart SVG -> IO ()
coreSvg t s c =
  fileSvg
  (coreDir <> t)
  (field @"size" .~ s $ defaultSvgOptions)
  c

exampleSvg :: FilePath -> Pair Double -> Chart SVG -> IO ()
exampleSvg t s c =
  fileSvg
  (exampleDir <> t)
  (field @"size" .~ s $ defaultSvgOptions)
  c

sThin :: Pair Double
sThin = Pair 400 100

sStandard :: Pair Double
sStandard = Pair 300 200

sSmall :: Pair Double
sSmall = Pair 100 100

main :: IO ()
main = do
  scaleExample
  coreSvg "text_Example.svg" sThin text_Example
  coreSvg "text_SvgExample.svg" sThin text_SvgExample
  coreSvg "text_PathExample.svg" sThin text_PathExample
  coreSvg "textChart_Example.svg" sThin textChart_Example
  coreSvg "labelledExample.svg" sSmall labelledExample
  exampleSvg "textHudExample.svg" sThin
    (textHudExample <> textChart_Example)
  coreSvg"glyph_Example.svg" sThin glyph_Example
  coreSvg "glyphsExample.svg" sThin glyphsExample
  coreSvg "glyphChart_Example.svg" (Pair 450 150) glyphChart_Example
  coreSvg "lglyphsExample.svg" sThin lglyphsExample
  coreSvg "lglyphChart_Example.svg" (Pair 600 200) (lglyphChart_Example widescreen)
  exampleSvg "glyphHudExample.svg" sStandard
    (glyphHudExample <> glyphChart_Example)
  exampleSvg "lglyphHudExample.svg" sStandard
    (lglyphHudExample <> lglyphChart_Example widescreen)
  coreSvg "linesExample.svg" sThin (linesExample 100)
  coreSvg "lineChart_Example.svg" sStandard lineChart_Example
  coreSvg "glineChart_Example.svg" sStandard glineChart_Example
  exampleSvg "lineHudExample.svg" sStandard
    (lineHudExample <> lineChart_Example)
  exampleSvg "glineHudExample.svg" sStandard glineHudExample
  coreSvg "rect_Example.svg" sStandard (rect_Example 2)
  coreSvg "rectsExample.svg" sStandard rectsExample
  coreSvg "rectChart_Example.svg" (Pair 300 100) rectChart_Example
  coreSvg "pixel_Example.svg" sSmall pixel_Example
  coreSvg "pixelsExample.svg" (Pair 300 100) pixelsExample
  coreSvg "pixelChart_Example.svg" (Pair 300 300) pixelChart_Example
  exampleSvg "rectHudExample.svg" sStandard
    (rectHudExample <> rectChart_Example)
  exampleSvg "pixelHudExample.svg" sStandard
    (pixelHudExample <> pixelateChartExample)
  coreSvg "arrowsExample.svg" (Pair 100 300) arrowsExample
  coreSvg "arrowChart_Example.svg" (Pair 300 300) arrowChart_Example
  exampleSvg "arrowHudExample.svg" sStandard
    (arrowHudExample <> arrowChart_Example)
  coreSvg "hudExample.svg" (Pair 300 300) hudExample
  coreSvg "withHudExample.svg" sStandard withHudExample
  coreSvg "axisExample.svg" sThin axisExample
  coreSvg "legendExample.svg" (Pair 300 300) legendExample
  exampleSvg "smallHudExample.svg" sSmall
    (D.showOrigin $ hud defaultHudOptions one one)
  coreSvg "barExample.svg" sStandard barExample
  exampleSvg "testTextDiffs.svg" (Pair 400 600)
    (testTextDiffs 1 0.77 "abcdefghij012345" (0.25,-0.1,0.25) (Just "san-serif"))
