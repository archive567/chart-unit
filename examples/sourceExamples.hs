{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Chart 
import NumHask.Prelude
import Control.Lens hiding (over)
import qualified Data.Text as Text

scratch :: Diagram SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600,400)

text_Example :: Chart b
text_Example = text_ def "Welcome to chart-unit!"

textsExample :: Chart b
textsExample = texts def ts [Pair (0.05*x) 0 | x <- [0..5]]
  where
    ts = map (Text.singleton) ['a'..'z']

textChart_Example :: Chart b
textChart_Example = textChart_ (repeat $ def {textSize=0.33}) widescreen [zip ts ps]
  where
    ts = map (Text.singleton) ['a'..'z']
    ps = [Pair (sin (x*0.1)) x | x<-[0..25]]

labelledExample :: Chart b
labelledExample = labelled (LabelOptions (def {textAlignH = AlignLeft, textRotation=45}) (Pair 1 1) 0.05) "a label" (glyph_ def)

glyph_Example :: Chart b
glyph_Example = glyph_ def

glyphsExample :: Chart b
glyphsExample = glyphs def [Pair (x/10) ((sin x)/10) | x<-[0..10]]

glyphChart_Example :: Chart b
glyphChart_Example = glyphChart_ gopts widescreen [p_1,p_2]
  where
    gopts = [def,def {glyphBorderColor=ured, glyphShape=triangle}]
    p_1 = [Pair x (sin (x/10)) | x<-[0..100]]
    p_2 = [Pair x (cos (x/10)) | x<-[0..100]]

lglyphsExample :: Chart b
lglyphsExample = lglyphs def def $
    zip (show <$> [0..]) [Pair (x/10) ((sin x)/10) | x<-[0..10]]

lglyphChart_Example :: Chart b
lglyphChart_Example =
    lglyphChart_ [def {labelGap=0.01}] [def {glyphBorderSize=0.002}] widescreen xs
  where
    xs = [(\(p@(Pair x y)) -> ((show x <> "," <> show y), fromIntegral <$> p)) <$> g]
    g = Pair <$> [0..5] <*> [0..5] :: [Pair Int]

linesExample :: Int -> Chart b
linesExample n = lines def
    [ Pair (10*x/fromIntegral n) (cos (x * (10 / fromIntegral n))) |
      x <- fromIntegral <$> [0..n]]

ls :: [[Pair Double]]
ls = map (uncurry Pair) <$>
    [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
    , [(0.0,0.0),(3.0,3.0)]
    , [(0.5,4.0),(0.5,0)]
    ]

lopts :: [LineOptions]
lopts =
    zipWith (\x y -> LineOptions x (withOpacity y 0.6)) [0.01,0.02,0.005] (tetrad (ublue `over` black))

lineChart_Example :: Chart b
lineChart_Example = lineChart_ lopts sixbyfour ls

glineChart_Example :: Chart b
glineChart_Example = glineChart_ lopts gopts sixbyfour ls
  where
    gopts =
        zipWith (\x y -> def {glyphColor=transparent, glyphBorderColor=withOpacity x 0.6, glyphShape=y})
        (tetrad (rybColor 4))
        [triangle, square, circle]

rect_Example :: Chart b
rect_Example = labelled (opts (Pair 2 1)) ("z,w") $ labelled (opts (Pair -2 -1)) ("x,y") (rect_ def (Ranges (2*.one) one))
  where
    opts o = def {labelText = (labelText def) {textColor=withOpacity black 0.8, textSize = 0.3}, labelOrientation=o}

rectsExample :: Chart b
rectsExample = rects def $ zipWith (\x y -> Rect x (x+1) 0 y) [0..] [1,2,3,5,8,0,-2,11,2,1]

rectChart_Example :: Chart b
rectChart_Example = rectChart_ ropts widescreen rss
  where
    ropts = [def {rectBorderSize=0}, def {rectBorderSize=0,rectColor=ucolor 0.3 0.3 0.3 0.2}]
    rss = (zipWith (\x y -> Rect x (x+1) 0 y) [0..]) <$> pss
    pss = transpose [[exp (-(x**2)/2), 0.5 * exp (-(x**2)/8)] |
                     x <- grid LowerPos (Range -5 5) 1000]

pixel_Example :: Chart b
pixel_Example = text_ opt "I'm a pixel!" <> pixel_ (Pixel one ublue)
  where
    opt = def {textColor=withOpacity black 0.8, textSize = 0.2}

pixelsExample :: Chart b
pixelsExample = pixels $ [Pixel (Rect (5*x) (5*x+0.1) (sin (10*x)) (sin (10*x) + 0.1)) (dissolve (2*x) ublue) | x <- grid OuterPos (Range 0 1) 100]

pixelChart_Example :: Chart b
pixelChart_Example = pixelChart_ asquare [[Pixel (Rect x (x+0.05) y (y+0.05)) (blend  (x*y+x*x) ugrey ublue) | x <- grid MidPos (one::Range Double) 20, y <- grid MidPos (one::Range Double) 20]]

-- pixelateChartExample :: Chart b
-- pixelateChartExample = pixelateChart def asquare one (\(Pair x y) -> x*y+x*x)
 
arrowsExample :: Chart b
arrowsExample = arrows (def {arrowMaxLength=0.5,arrowMaxHeadLength=0.2,arrowMaxStaffWidth=0.01}) [Arrow (Pair x (sin (5*x))) (Pair x (cos x)) | x<-grid MidPos (one::Range Double) 100]

arrowChart_Example :: Chart b
arrowChart_Example = arrowChart_ [def] asquare [as]
  where
    as = normArrows [Arrow (Pair x y) (Pair (sin 1/x+0.0001) (cos 1/y+0.0001)) | x<-grid MidPos (one::Range Double) 20, y<-grid MidPos (one::Range Double) 20]

hudExample :: Chart b
hudExample = hud def

withHudExample :: Chart b
withHudExample = withHud hopts (lineChart lopts) ls
    where
      hopts = def {hudTitles=[(def,"withHud Example")],
                   hudLegends=[def {legendChartType=zipWith (\x y -> (LegendLine x 0.05, y)) lopts ["line1", "line2", "line3"]}]
                  }

axisExample :: Chart b
axisExample = axis aopts one (Range 0 100000)
  where
    aopts = def {axisLabel=(axisLabel def) {labelGap=0.0001,labelText=(labelText (axisLabel def)) {textSize=0.06,textAlignH=AlignLeft,textRotation=(-45)}}}

legends :: [(LegendType b, Text)]
legends =
    [ (LegendText def, "legend")] <>
    [ (LegendPixel (blob ublue) 0.05, "pixel")] <>
    -- [ (LegendArrow (def {arrowMinStaffWidth=0.01,
    --                     arrowMinHeadLength=0.03}) 0.05, "arrow")] <>
    [ (LegendRect def 0.05, "rect")] <>
    [ (LegendGLine def def 0.10, "glyph+line")] <>
    [ (LegendGlyph def, "just a glyph")] <>
    (zipWith (\x y -> (LegendLine x 0.05, y))
      lopts ["short", "much longer name", "line 3"])

legendExample :: Chart b
legendExample = legend $ def {legendChartType=legends}

mainExample :: Diagram B
mainExample = withHud opts (lineChart lopts) ls
  where
    opts =
        (hudTitles_ .~ titles $
         hudAxes_ .~
          [ defXAxis,
            defYAxis,
            axisLabel_ . labelOrientation_ .~ Pair 0 1 $
            axisPlace_ .~ PlaceTop $ defXAxis,
            axisLabel_ . labelOrientation_ .~ Pair 1 0 $
            axisPlace_ .~ PlaceRight $ defYAxis] $
          hudAxes_ %~ (map (axisPad_ .~ 1)) $
          hudLegends_ .~ [legendChartType_ .~ legends $ def] $
          def)

titles :: [(TitleOptions, Text)]
titles =
     [ (def, "Example Chart")
     , ((titleAlign_ .~ AlignCenter $
         titleText_ . textRotation_ .~ 90 $
         titleText_ . textSize_ .~ 0.12 $
         titlePlace_ .~ PlaceLeft $ def), "left axis title")
     , ((titleText_ . textColor_ .~ ublue $
         titleText_ . textSize_ .~ 0.08 $
         titleAlign_ .~ AlignRight $
         titlePlace_ .~ PlaceBottom $ def, "bottom right, non-essential note"))
     ]

main :: IO ()
main = do
    fileSvg "other/text_Example.svg" (400,100) text_Example
    fileSvg "other/textsExample.svg" (400,100) textsExample
    fileSvg "other/textChart_Example.svg" (300,100) textChart_Example
    fileSvg "other/labelledExample.svg" (300,100) labelledExample
    fileSvg "other/glyph_Example.svg" (400,100) glyph_Example
    fileSvg "other/glyphsExample.svg" (400,100) glyphsExample
    fileSvg "other/glyphChart_Example.svg" (450,150) glyphChart_Example
    fileSvg "other/lglyphsExample.svg" (400,100) lglyphsExample
    fileSvg "other/lglyphChart_Example.svg" (600,200) lglyphChart_Example
    fileSvg "other/linesExample.svg" (400,100) (linesExample 100)
    fileSvg "other/lineChart_Example.svg" (300,200) lineChart_Example
    fileSvg "other/glineChart_Example.svg" (300,200) glineChart_Example
    fileSvg "other/rect_Example.svg" (300,200) rect_Example
    fileSvg "other/rectsExample.svg" (300,200) rectsExample
    fileSvg "other/rectChart_Example.svg" (300,100) rectChart_Example
    fileSvg "other/pixel_Example.svg" (100,100) pixel_Example
    fileSvg "other/pixelsExample.svg" (300,100) pixelsExample
    fileSvg "other/pixelChart_Example.svg" (300,300) pixelChart_Example
    fileSvg "other/arrowsExample.svg" (100,300) arrowsExample
    fileSvg "other/arrowChart_Example.svg" (300,300) arrowChart_Example
    fileSvg "other/hudExample.svg" (300,300) hudExample
    fileSvg "other/withHudExample.svg" (300,200) withHudExample
    fileSvg "other/axisExample.svg" (400,100) axisExample
    fileSvg "other/legendExample.svg" (300,300) legendExample
    fileSvg "other/mainExample.svg" (600,400) mainExample
