{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | examples used in haddock docs
import Chart
import Control.Lens
import Data.Generics.Labels()
import NumHask.Prelude
import qualified Data.Text as Text
import qualified Diagrams.Prelude as D

coreDir :: FilePath
coreDir = "./chart-unit/other/"

examplesDir :: FilePath
examplesDir = "./chart-unit-examples/other/"

-- * Chart.Core examples 
scaleExample :: IO ()
scaleExample =
  fileSvg (coreDir <> "scaleExample.svg") (#size .~ Pair 300 120 $ def) $
  withHud
  def
  widescreen
  (Rect 0 12 0 0.2)
  (lineChart (repeat def))
  (vlineOneD ((0.01*) <$> [0..10]))

-- * example charts look n feel
hudbits :: Text -> Maybe Text -> [Text] -> [LegendType] -> HudOptions -> HudOptions
hudbits t subt ts ls x =
  #titles .~
  [(#place .~ PlaceLeft $
    #align .~ AlignLeft $
    #text . #rotation .~ 90 $
    #text . #size .~ 0.2 $
    #text . #color .~ ucolor (d3Colors1 0 `withOpacity` 1) $
    def, t)] <>
  (case subt of
     Nothing -> []
     Just subt' -> 
       [(#place .~ PlaceBottom $
         #align .~ AlignRight $
         #text . #rotation .~ 0 $
         #text . #size .~ 0.12 $
         #text . #color .~ ucolor (d3Colors1 0 `withOpacity` 1) $
         def, subt')]) $ 
  #legends .~
  [#chartType .~ zip ls ts $
   #align .~ AlignRight $
   #text . #size .~ 0.2 $
   def ] $
  #axes . each . #gap .~ 0.1 $
  x

-- * Chart.Text examples
text_Example :: Chart b
text_Example = text_ (#textType .~ TextPath def $ def) "Welcome to chart-unit!"

text_SvgExample :: Chart b
text_SvgExample = text_
  (#textType .~ TextSvg (#textBox .~ def $ #svgFont .~ Just "Comic Sans MS" $ def) $
   #size .~ 0.2 $
   def)
  "abc & 0123 & POW!"

text_PathExample :: Chart b
text_PathExample = text_
  (#textType .~
   TextPath (#font .~ FromFontFile ("./chart-unit/other/Hasklig-Regular.svg") $ def) $
   #size .~ 0.2 $
   def)
   "0123 <*> <$> <| |> <> <- -> => ::"

ts :: [(Text, Pair Double)]
ts = zip
  (map Text.singleton ['a' .. 'z'])
  [Pair (sin (x * 0.1)) x | x <- [0 .. 25]]

textChart_Example :: Chart b
textChart_Example =
  D.pad 1.1 $ textChart_ [#size .~ 0.33 $ def] widescreen [ts]

labelledExample :: Chart b
labelledExample = D.pad 1.1 $
  labelled (LabelOptions
    (#alignH .~ AlignLeft $ #rotation .~ 45 $ def) (Pair 1 1) 0.02)
  "a label"
  (glyph_ def)

textHudExample :: Chart b
textHudExample =
  hud
  (hudbits "Text Chart" (Just "text and glyphs have a similar feel") [] [] def)
  widescreen
  (range ts)

-- * Chart.Glyph examples
glyph_Example :: Chart b
glyph_Example = glyph_ def

glyphsExample :: Chart b
glyphsExample = glyphs def (dataXY sin (Range 0 (2*pi)) 30)

gopts :: [GlyphOptions]
gopts =
  [ #borderSize .~ 0.001 $
    #size .~ 0.1 $
    def
  , #borderSize .~ 0.001 $
    #size .~ 0.1 $
    #color .~ ucolor (rybColor 7 `withOpacity` 0.4) $
    #shape .~ Triangle $ def
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
  lglyphs def def $
  zip (show <$> [0 ..]) [Pair (x / 10) (sin x / 10) | x <- [0 .. 10]]

lgdata :: [(Text, Pair Double)]
lgdata =
  (\(p@(Pair x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$>
    (Pair <$> [0 .. 5] <*> [0 .. 5] :: [Pair Int])

lglyphChart_Example :: Rect Double -> Chart b
lglyphChart_Example a =
  lglyphChart_
  [#gap .~ 0.015 $ #text . #size .~ 0.12 $ def]
  [#color .~ ublack $
   #borderSize .~ 0 $
   #size .~ 0.01 $
   def]
  a
  [lgdata]

glyphHudExample :: Chart b
glyphHudExample = 
  hud
  (#legends . each . #align .~ AlignLeft $
   hudbits "Glyph Chart" (Just "text elements are paths not svg text")
   ["sin", "cos"]
   (LegendGlyph <$> gopts) $
   #axes .~
    [ #label . #text . #size .~ 0.3 $
      #tickStyle .~ TickPlaced pis $
      #label . #text . #textType .~
      TextPath (TextPathOptions (FromFontFile "./chart-unit-examples/other/SourceCodePro-Regular.svg")) $
      defXAxis
    ] $ def)
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
  (#titles . each . _1 . #gap .~ 0.2 $
   hudbits "LGlyph Chart" (Just "Glyphs with text labels are very useful") [] [] $
   #axes .~ [] $
   def)
  widescreen
  (range (fmap snd <$> [lgdata]))

-- * Chart.Lines examples
linesExample :: Int -> Chart b
linesExample n =
  lines
  (#color .~ ucolor (red `withOpacity` 0.5) $ def)
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
     #color .~ ucolor (withOpacity (d3Colors1 x) 0.2) $
     #borderColor .~ ucolor (withOpacity (d3Colors1 x) 1) $
     #borderSize .~ 0.005 $
     #shape .~ y $
     #size .~ 0.08 $
     def)
  [6,8,2]
  [Triangle, Square, Circle]

glineChart_Example :: Chart b
glineChart_Example = glineChart_ lopts gopts3 sixbyfour ls

lineHudExample :: Chart b
lineHudExample = 
  hud
  (hudbits "Line Chart" Nothing ["hockey stick", "slope", "vertical"]
   ((`LegendLine` 0.05) <$> lopts) def)
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
    (#legends . each . #gap .~ 0.2 $
     #titles . each . _1 . #gap .~ 0.2 $
     hudbits "Gline Chart" Nothing ["triangle", "square", "circle"]
     (zipWith (\x y -> LegendGLine x y 0.1) gopts3 lopts) $
     #axes .~ [] $
     def)
  , LGlyphChart
    [ ( (#gap .~ 0.015 $ #text . #size .~ 0.12 $ def
      , #color .~ ublack $
        #borderSize .~ 0 $
        #size .~ 0.01 $
        def)
      , lgdata)]
  ])

-- * Chart.Rect examples
rect_Example :: Double -> Chart b
rect_Example n =
  labelled (opts (Pair n 1)) "z,w" $
  labelled (opts (Pair n -1)) "z,y" $
  labelled (opts (Pair (-n) 1)) "x,w" $
  labelled (opts (Pair (-n) -1)) "x,y" $
  rect_ def (Ranges (n *. one) one)
  where
    opts :: Pair Double -> LabelOptions
    opts o =
      #text %~
        ( (#color .~ UColor 0 0 0 0.8) .
          (#size .~ 0.3)) $
      #orientation .~ o $
      def

rectsExample :: Chart b
rectsExample =
  rects def (rectBars 0.1 [1, 2, 3, 5, 8, 0, -2, 11, 2, 1])

ropts :: [RectOptions]
ropts =
  [ #borderSize .~ 0 $ def
  , #borderSize .~ 0 $ #color .~ UColor 0.3 0.3 0.3 0.2 $ def
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
      #color .~ UColor 0 0 0 0.8 $
      #size .~ 0.2 $
      def

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
pixelateChartExample = pixelateChart def asquare one (\(Pair x y) -> (x+y)*(x+y))

rectHudExample :: Chart b
rectHudExample =
  hud
  (#legends . each . #place .~ PlaceBottom $
   #legends . each . #align .~ AlignCenter $
   hudbits "Rect Chart" Nothing ["blue gaussian", "grey wider distribution"]
   ((`LegendRect` 0.05) <$> ropts) $
   #axes .~ [defXAxis] $
   def)
  widescreen
  (fold $ fold rss)

pixelHudExample :: Chart b
pixelHudExample =
  hud
  (hudbits "Pixel Chart" Nothing ["red", "blue"]
   ((`LegendPixel` 0.05) <$> ropts) def)
  asquare
  one

-- * Chart.Arrow examples
arrowsExample :: Chart b
arrowsExample =
  arrows
  (#maxLength .~ 0.5 $
   #maxHeadLength .~ 0.2 $
   #maxStaffWidth .~ 0.01 $ def)
  [ Arrow (Pair x (sin (5 * x))) (Pair x (cos x))
  | x <- grid MidPos (one :: Range Double) 100
  ]

arrowChart_Example :: Chart b
arrowChart_Example = arrowChart_ [def] asquare [as]
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
   def)
  asquare
  one

-- * Chart.Hud examples
hudExample :: Chart b
hudExample = hud def sixbyfour one

withHudExample :: Chart b
withHudExample = withHud_ hopts sixbyfour (lineChart lopts) ls
  where
    hopts =
      #titles .~ [(def, "withHud Example")] $
      #legends .~
      [ #chartType .~ zipWith
        (\x y -> (LegendLine x 0.05, y))
        lopts
        ["line1", "line2", "line3"]
        $ def
      ] $ def

axisExample :: Chart b
axisExample = axis aopts one (Range 0 100000)
  where
    aopts :: AxisOptions
    aopts =
      #label . #text %~
      ((#rotation .~ -45) .
       (#size .~ 0.06) .
       (#alignH .~ AlignLeft)) $
      #gap .~ 0.0001 $ def

legends' :: [(LegendType, Text)]
legends' =
  [(LegendText def, "legend")] <> [(LegendPixel (blob ublue) 0.05, "pixel")] <>
  -- [(LegendArrow (def & #minStaffWidth .~ 0.01 & #minHeadLength .~ 0.03) 0.05, "arrow")] <>
  [(LegendRect def 0.05, "rect")] <>
  [(LegendGLine def def 0.10, "glyph+line")] <>
  [(LegendGlyph def, "just a glyph")] <>
  zipWith
    (\x y -> (LegendLine x 0.05, y))
    lopts
    ["short", "much longer name", "line 3"]

legendExample :: Chart b
legendExample = legend $ #chartType .~ legends' $ def

-- * Chart.Bar examples
barExample :: Chart b
barExample  =
  barChart def (BarData [ys] Nothing Nothing) <>
  hud
  ( #titles .~ [(def,"Bar Chart")] $
    #axes .~
    [ #tickStyle .~
      TickLabels labels' $
      def
    ] $
    def)
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
      (#alignH .~ ah $
       #alignV .~ av $
       #size .~ s $
       #color .~ ucolor (red `withOpacity` 1) $
       def) txt) <>
     D.showOrigin' (D.OriginOpts blue 0.003 0.003)
      (text_
      (#alignV .~ av $
       #alignH .~ ah $
       #size .~ s $
       #textType .~ TextSvg (TextSvgOptions ns nb nm nt fnt 1.1 0.55 clear) $
       def) txt)) <$>
  ((\x y -> (x,y,txt)) <$>
   [AlignLeft, AlignCenter, AlignRight] <*>
   [AlignBottom, AlignMid, AlignTop])

main :: IO ()
main = do
  scaleExample
  fileSvg (coreDir <> "text_Example.svg") (#size .~ Pair 400 100 $ def)
    text_Example
  fileSvg (coreDir <> "text_SvgExample.svg") (#size .~ Pair 400 100 $ def)
    text_SvgExample
  fileSvg (coreDir <> "text_PathExample.svg") (#size .~ Pair 400 100 $ def)
    text_PathExample
  fileSvg (coreDir <> "textChart_Example.svg") (#size .~ Pair 400 100 $ def)
    textChart_Example
  fileSvg (coreDir <> "labelledExample.svg") (#size .~ Pair 100 100 $ def)
    labelledExample
  fileSvg (examplesDir <> "textHudExample.svg") def $
    textHudExample <> textChart_Example
  fileSvg (coreDir <> "glyph_Example.svg") (#size .~ Pair 400 100 $ def)
    glyph_Example
  fileSvg (coreDir <> "glyphsExample.svg") (#size .~ Pair 400 100 $ def)
    glyphsExample
  fileSvg (coreDir <> "glyphChart_Example.svg") (#size .~ Pair 450 150 $ def)
    glyphChart_Example
  fileSvg (coreDir <> "lglyphsExample.svg") (#size .~ Pair 400 100 $ def)
    lglyphsExample
  fileSvg
    (coreDir <> "lglyphChart_Example.svg")
    (#size .~ Pair 600 200 $ def)
    (lglyphChart_Example widescreen)
  fileSvg (examplesDir <> "glyphHudExample.svg") def $
    glyphHudExample <> glyphChart_Example
  fileSvg (examplesDir <> "lglyphHudExample.svg") def $
    lglyphHudExample <> lglyphChart_Example widescreen
  fileSvg (coreDir <> "linesExample.svg") (#size .~ Pair 400 100 $ def)
    (linesExample 100)
  fileSvg (coreDir <> "lineChart_Example.svg") (#size .~ Pair 300 200 $ def)
    lineChart_Example
  fileSvg (coreDir <> "glineChart_Example.svg") (#size .~ Pair 300 200 $ def)
    glineChart_Example
  fileSvg (examplesDir <> "lineHudExample.svg") def $
    lineHudExample <> lineChart_Example
  fileSvg (examplesDir <> "glineHudExample.svg") def glineHudExample
  fileSvg (coreDir <> "rect_Example.svg") (#size .~ Pair 300 200 $ def) $
    rect_Example 2
  fileSvg (coreDir <> "rectsExample.svg") (#size .~ Pair 300 200 $ def) rectsExample
  fileSvg (coreDir <> "rectChart_Example.svg") (#size .~ Pair 300 100 $ def)
    rectChart_Example
  fileSvg (coreDir <> "pixel_Example.svg") (#size .~ Pair 100 100 $ def)
    pixel_Example
  fileSvg (coreDir <> "pixelsExample.svg") (#size .~ Pair 300 100 $ def)
    pixelsExample
  fileSvg (coreDir <> "pixelChart_Example.svg") (#size .~ Pair 300 300 $ def)
    pixelChart_Example
  fileSvg (examplesDir <> "rectHudExample.svg") def $
    rectHudExample <> rectChart_Example
  fileSvg (examplesDir <> "pixelHudExample.svg") def $
    pixelHudExample <> pixelateChartExample
  fileSvg (coreDir <> "arrowsExample.svg") (#size .~ Pair 100 300 $ def)
    arrowsExample
  fileSvg (coreDir <> "arrowChart_Example.svg") (#size .~ Pair 300 300 $ def)
    arrowChart_Example
  fileSvg (examplesDir <> "arrowHudExample.svg") def $
    arrowHudExample <> arrowChart_Example
  fileSvg (coreDir <> "hudExample.svg") (#size .~ Pair 300 300 $ def) hudExample
  fileSvg (coreDir <> "withHudExample.svg") (#size .~ Pair 300 200 $ def)
    withHudExample
  fileSvg (coreDir <> "axisExample.svg") (#size .~ Pair 400 100 $ def) axisExample
  fileSvg (coreDir <> "legendExample.svg") (#size .~ Pair 300 300 $ def)
    legendExample
  fileSvg (examplesDir <> "smallHudExample.svg") (#size .~ Pair 100 100 $ def)
    (D.showOrigin $ hud def one one)
  fileSvg (coreDir <> "barExample.svg") def barExample
  fileSvg (examplesDir <> "testTextDiffs.svg") (#size .~ Pair 400 600 $ def) $
    testTextDiffs 1 0.77 "abcdefghij012345" (0.25,-0.1,0.25) (Just "san-serif")
