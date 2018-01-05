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
import qualified Diagrams.Prelude as D
import NumHask.Prelude
import qualified Data.Text as Text

-- * Chart.Core examples 
scaleExample :: IO ()
scaleExample =
    fileSvg "other/scaleExample.svg" (300,120) $ withHud
      ( #aspect .~ widescreen $
        #range .~ Just (Rect 0 12 0 0.2) $
        def)
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
    #text . #color .~ d3Colors1 0 `withOpacity` 1 $
    def, t)] <>
  (case subt of
     Nothing -> []
     Just subt' -> 
       [(#place .~ PlaceBottom $
         #align .~ AlignRight $
         #text . #rotation .~ 0 $
         #text . #size .~ 0.12 $
         #text . #color .~ d3Colors1 0 `withOpacity` 1 $
         def, subt')]) $ 
  #legends .~
  [#chartType .~ zip ls ts $
   #align .~ AlignRight $ def ] $
  #axes . each . #gap .~ 0.1 $
  x

-- * Chart.Text examples
text_Example :: Chart b
text_Example = text_ def "Welcome to chart-unit!"

ts :: [(Text, Pair Double)]
ts = zip
  (map Text.singleton ['a' .. 'z'])
  [Pair (sin (x * 0.1)) x | x <- [0 .. 25]]

textsExample :: Chart b
textsExample = texts def ts

textChart_Example :: Chart b
textChart_Example =
  textChart_ [#size .~ 0.33 $ def] widescreen [ts]

labelledExample :: Chart b
labelledExample =
  labelled
  (LabelOptions
    (#alignH .~ AlignLeft $ #rotation .~ 45 $ def)
    (Pair 1 1)
    0.05)
  "a label"
  (glyph_ def)

textHudExample :: Chart b
textHudExample = hud
  (hudbits "Text Chart" (Just "text and glyphs have a similar feel") [] [] $
   #range .~ Just (range ts) $
   #aspect .~ widescreen $
   def)

-- * Chart.Glyph examples
glyph_Example :: Chart b
glyph_Example = glyph_ def

glyphsExample :: Chart b
glyphsExample = glyphs def (dataXY sin (Range 0 (2*pi)) 30)

gopts :: [GlyphOptions]
gopts =
  [ #borderSize .~ 0.001 $ def
  , #borderSize .~ 0.001 $
    #size .~ 0.1 $
    #color .~ rybColor 7 `withOpacity` 0.4 $
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

lgdata :: [[(Text, Pair Double)]]
lgdata =
  [(\(p@(Pair x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$>
    (Pair <$> [0 .. 5] <*> [0 .. 5] :: [Pair Int])
  ]

lglyphChart_Example :: Rect Double -> Chart b
lglyphChart_Example a =
  lglyphChart_
  [#gap .~ 0.015 $ #text . #size .~ 0.12 $ def]
  [#color .~ black `withOpacity` 1 $
   #borderSize .~ 0 $
   #size .~ 0.01 $
   def]
  a
  lgdata

glyphHudExample :: Chart b
glyphHudExample = 
  hud
  (#legends . each . #align .~ AlignLeft $
   hudbits "Glyph Chart" (Just "text elements are paths not svg text")
   ["sin", "cos"]
   (LegendGlyph <$> gopts) $
   #range .~ Just (range gdata) $
   #aspect .~ widescreen $
   #axes .~
    [ #label . #text . #size .~ 0.2 $
      #tickStyle .~ TickPlaced pis $
      #label . #text . #textType .~ TextPath Lin $
      defXAxis
    , defYAxis] $ def)
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
   #range .~ Just (range (fmap snd <$> lgdata)) $
   #aspect .~ widescreen $
   def)

-- * Chart.Lines examples
linesExample :: Int -> Chart b
linesExample n =
  lines
  (#color .~ D.red `withOpacity` 0.5 $ def)
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
  (\x y -> LineOptions x (withOpacity (d3Colors1 y) 0.6))
  [0.01, 0.02, 0.005]
  [0,1,2]

lineChart_Example :: Chart b
lineChart_Example = lineChart_ lopts sixbyfour ls

gopts3 :: [GlyphOptions]
gopts3 =
  zipWith
  (\x y ->
     #color .~ withOpacity (d3Colors1 x) 0.2 $
     #borderColor .~ withOpacity (d3Colors1 x) 1 $
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
   ((`LegendLine` 0.05) <$> lopts) $
   #range .~ Just (range ls) $
   def)

glineHudExample :: Chart b
glineHudExample = 
  hud
  (#legends . each . #gap .~ 0.2 $
   #titles . each . _1 . #gap .~ 0.2 $
   hudbits "Gline Chart" Nothing ["triangle", "square", "circle"]
   (zipWith (\x y -> LegendGLine x y 0.1) gopts3 lopts) $
   #axes .~ [] $
   #range .~ Just (Rect 0 5 0 5) $
   def) <>
  glineChart lopts gopts3 sixbyfour (Rect 0 5 0 5) ls <>
  lglyphChart_Example sixbyfour


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
        ( (#color .~ black `withOpacity` 0.8) .
          (#size .~ 0.3)) $
      #orientation .~ o $
      def

rectsExample :: Chart b
rectsExample =
  rects def (rectBars 0.1 [1, 2, 3, 5, 8, 0, -2, 11, 2, 1])

ropts :: [RectOptions]
ropts =
  [ #borderSize .~ 0 $ def
  , #borderSize .~ 0 $ #color .~ ucolor 0.3 0.3 0.3 0.2 $ def
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
      #color .~ withOpacity black 0.8 $
      #size .~ 0.2 $
      def

pixelsExample :: Chart b
pixelsExample =
  pixels
    [ Pixel
      (Rect (5 * x) (5 * x + 0.1) (sin (10 * x)) (sin (10 * x) + 0.1))
      (dissolve (2 * x) ublue)
    | x <- grid OuterPos (Range 0 1) 100
    ]

pixelChart_Example :: Chart b
pixelChart_Example =
  pixelChart_ asquare
  [(\(r,c) ->
      Pixel r
      (blend c
       (ucolor 0.47 0.73 0.86 1)
       (ucolor 0.01 0.06 0.22 1)
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
   #range .~ Just (fold $ fold rss) $
   #aspect .~ widescreen $
   #axes .~ [defXAxis] $
   def)

pixelHudExample :: Chart b
pixelHudExample =
  hud
  (hudbits "Pixel Chart" Nothing ["red", "blue"]
   ((`LegendPixel` 0.05) <$> ropts) $
   #range .~ Just one $
   #aspect .~ asquare $
   def)

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
  (hudbits "Arrow Chart" Nothing ["this way up"] [] $
-- ((`LegendArrow` 0.05) <$> [def]) $
   #range .~ Nothing $
   #aspect .~ asquare $
   def)

-- * Chart.Hud examples
hudExample :: Chart b
hudExample = hud def

withHudExample :: Chart b
withHudExample = withHud hopts (lineChart lopts) ls
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

legends :: [(LegendType, Text)]
legends =
  [(LegendText def, "legend")] <> [(LegendPixel (blob ublue) 0.05, "pixel")] <>
    -- [ (LegendArrow (def & #minStaffWidth .~ 0.01 & #minHeadLength .~ 0.03) 0.05, "arrow")] <>
  [(LegendRect def 0.05, "rect")] <>
  [(LegendGLine def def 0.10, "glyph+line")] <>
  [(LegendGlyph def, "just a glyph")] <>
  zipWith
    (\x y -> (LegendLine x 0.05, y))
    lopts
    ["short", "much longer name", "line 3"]

legendExample :: Chart b
legendExample = legend $ #chartType .~ legends $ def

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
    #range .~ Just (fold (abs <$> rs)) $
    def)
  where
    labels' = fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..]
    rs = rectBars 0.1 ys
    ys = [1,2,3,5,8,0,-2,11,2,1]

-- * difference between svg text and path text
testTextDiffs :: Double -> Double -> Text -> (Double, Double, Double) -> Chart b
testTextDiffs s ns txt (nb, nm, nt) =
  D.pad 1.1 $ vert identity $ D.centerXY .
  (\(ah,av,txt) ->
     D.showOrigin' (D.OriginOpts D.red 0.001 0.001)
      (text_
      (#alignH .~ ah $
       #alignV .~ av $
       #size .~ s $
       #color .~ D.red `withOpacity` 1 $
       def) txt) <>
     D.showOrigin' (D.OriginOpts D.blue 0.003 0.003)
      (text_
      (#alignV .~ av $
       #alignH .~ ah $
       #size .~ s $
       #textType .~ TextSvg (TextSvgOptions ns nb nm nt) $
       def) txt)) <$>
  ((\x y -> (x,y,txt)) <$>
   [AlignLeft, AlignCenter, AlignRight] <*>
   [AlignBottom, AlignMid, AlignTop])

main :: IO ()
main = do
  scaleExample
  fileSvg "other/text_Example.svg" (400, 100) text_Example
  fileSvg "other/textsExample.svg" (400, 100) textsExample
  fileSvg "other/textChart_Example.svg" (300, 100) textChart_Example
  fileSvg "other/labelledExample.svg" (300, 100) labelledExample
  fileSvg "other/textHudExample.svg" (600, 400) $
    textHudExample <> textChart_Example
  fileSvg "other/glyph_Example.svg" (400, 100) glyph_Example
  fileSvg "other/glyphsExample.svg" (400, 100) glyphsExample
  fileSvg "other/glyphChart_Example.svg" (450, 150) glyphChart_Example
  fileSvg "other/lglyphsExample.svg" (400, 100) lglyphsExample
  fileSvg
    "other/lglyphChart_Example.svg"
    (600, 200)
    (lglyphChart_Example widescreen)
  fileSvg "other/glyphHudExample.svg" (600, 400) $
    glyphHudExample <> glyphChart_Example
  fileSvg "other/lglyphHudExample.svg" (600, 400) $
    lglyphHudExample <> lglyphChart_Example widescreen
  fileSvg "other/linesExample.svg" (400, 100) (linesExample 100)
  fileSvg "other/lineChart_Example.svg" (300, 200) lineChart_Example
  fileSvg "other/glineChart_Example.svg" (300, 200) glineChart_Example
  fileSvg "other/lineHudExample.svg" (600, 400) $
    lineHudExample <> lineChart_Example
  fileSvg "other/glineHudExample.svg" (600, 400) glineHudExample
  fileSvg "other/rect_Example.svg" (300, 200) $ rect_Example 2
  fileSvg "other/rectsExample.svg" (300, 200) rectsExample
  fileSvg "other/rectChart_Example.svg" (300, 100) rectChart_Example
  fileSvg "other/pixel_Example.svg" (100, 100) pixel_Example
  fileSvg "other/pixelsExample.svg" (300, 100) pixelsExample
  fileSvg "other/pixelChart_Example.svg" (300, 300) pixelChart_Example
  fileSvg "other/rectHudExample.svg" (600, 400) $
    rectHudExample <> rectChart_Example
  fileSvg "other/pixelHudExample.svg" (600, 400) $
    pixelHudExample <> pixelateChartExample
  fileSvg "other/arrowsExample.svg" (100, 300) arrowsExample
  fileSvg "other/arrowChart_Example.svg" (300, 300) arrowChart_Example
  fileSvg "other/arrowHudExample.svg" (600, 400) $
    arrowHudExample <> arrowChart_Example
  fileSvg "other/hudExample.svg" (300, 300) hudExample
  fileSvg "other/withHudExample.svg" (300, 200) withHudExample
  fileSvg "other/axisExample.svg" (400, 100) axisExample
  fileSvg "other/legendExample.svg" (300, 300) legendExample
  -- small hud examples
  fileSvg "other/hud.svg" (100, 100) (D.showOrigin $ hud def)
  putStrLn ("barExample" :: Text)
  fileSvg "other/barExample.svg" (600, 400) barExample

  -- tests
  putStrLn ("testing text differences" :: Text)
  fileSvg "other/testTextDiffs.svg" (400, 600) $
    testTextDiffs 1 0.77 "abcdefghij012345" (0.25,-0.1,0.25)
