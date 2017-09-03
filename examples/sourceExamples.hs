{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import Chart
import Control.Lens hiding (beside)
import qualified Data.Text as Text
import NumHask.Prelude
import Data.List (zipWith3, zipWith4)
import Diagrams.Prelude hiding ((*.), scaleX, scaleY, (<>))
import FakeData
import Diagrams.Backend.Rasterific (renderRasterific, Rasterific, animatedGif, GifLooping(..))
import Diagrams.Backend.SVG (B)

hudbits :: Text -> Maybe Text -> [Text] -> [LegendType b] -> HudOptions b -> HudOptions b
hudbits t subt ts ls x =
    hudTitles_ .~
    [ (titlePlace_ .~ PlaceLeft $
       titleAlign_ .~ AlignLeft $
       titleText_ . textRotation_ .~ 90 $
       titleText_ . textSize_ .~ 0.25 $
       titleText_ . textColor_ .~ d3Colors1 0 `withOpacity` 1 $
       def, t)] <>
    (case subt of
      Nothing -> []
      Just subt' -> 
        [(titlePlace_ .~ PlaceBottom $
          titleAlign_ .~ AlignRight $
          titleText_ . textRotation_ .~ 0 $
          titleText_ . textSize_ .~ 0.14 $
          titleText_ . textColor_ .~ d3Colors1 0 `withOpacity` 1 $
          def, subt')]) $ 
    hudLegends_ .~
    [ legendChartType_ .~
      zip ls ts $
      legendAlign_ .~ AlignRight $ def
    ] $
    hudAxes_ . each . axisGap_ .~ 0.1 $
    x

text_Example :: Chart b
text_Example = text_ def "Welcome to chart-unit!"

textsExample :: Chart b
textsExample = texts def ts [Pair (0.05 * x) 0 | x <- [0 .. 5]]
  where
    ts = map Text.singleton ['a' .. 'z']

ts :: [(Text, Pair Double)]
ts =
    zip
    (map Text.singleton ['a' .. 'z'])
    [Pair (sin (x * 0.1)) x | x <- [0 .. 25]]

textChart_Example :: Chart b
textChart_Example =
  textChart_ (repeat $ def {textSize = 0.33}) widescreen [ts]

textHudExample :: Chart b
textHudExample = hud
    ( hudbits "Text Chart" (Just "text and glyphs have a similar feel") [] [] $
      hudRange_ .~ Just (range ts) $
      hudAspect_ .~ widescreen $
      def)

labelledExample :: Chart b
labelledExample =
  labelled
    (LabelOptions
       (def {textAlignH = AlignLeft, textRotation = 45})
       (Pair 1 1)
       0.05)
    "a label"
    (glyph_ def)

glyph_Example :: Chart b
glyph_Example = glyph_ def

glyphsExample :: Chart b
glyphsExample = glyphs def [Pair (x / 10) (sin x / 10) | x <- [0 .. 10]]

gopts :: [GlyphOptions b]
gopts = [def, def { glyphBorderColor = rybColor 0 `withOpacity` 0.5
                  , glyphShape = triangle}]

gdata :: [[Pair Double]]
gdata = [p_1, p_2]
  where
    p_1 = [Pair x (sin (x / 10)) | x <- [0 .. 100]]
    p_2 = [Pair x (cos (x / 10)) | x <- [0 .. 100]]

glyphChart_Example :: Chart b
glyphChart_Example = glyphChart_ gopts widescreen gdata

glyphHudExample :: Chart b
glyphHudExample = 
    hud (hudLegends_ . each . legendAlign_ .~ AlignLeft $
         hudbits "Glyph Chart" Nothing ["sin", "cos"]
          (LegendGlyph <$> gopts) $
          hudRange_ .~ Just (range gdata) $
          hudAspect_ .~ widescreen $
          def)

lglyphsExample :: Chart b
lglyphsExample =
  lglyphs def def $
  zip (show <$> [0 ..]) [Pair (x / 10) (sin x / 10) | x <- [0 .. 10]]


lgdata :: [[(Text, Pair Double)]]
lgdata =
    [(\(p@(Pair x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$> g]
  where
    g = Pair <$> [0 .. 5] <*> [0 .. 5] :: [Pair Int]

lglyphChart_Example :: Aspect -> Chart b
lglyphChart_Example a =
  lglyphChart_
    [labelGap_ .~ 0.015 $ labelText_ . textSize_ .~ 0.12 $ def]
    [glyphColor_ .~ black `withOpacity` 1 $
     glyphBorderSize_ .~ 0 $
     glyphSize_ .~ 0.01 $
     def]
    a
    lgdata

lglyphHudExample :: Chart b
lglyphHudExample = hud
    ( hudTitles_ . each . _1 . titleGap_ .~ 0.2 $
      hudbits "LGlyph Chart" (Just "Glyphs with text labels are very useful") [] [] $
      hudAxes_ .~ [] $
      hudRange_ .~ Just (range (fmap snd <$> lgdata)) $
      hudAspect_ .~ widescreen $
      def)

linesExample :: Int -> Chart b
linesExample n =
  lines
    def
    [ Pair (10 * x / fromIntegral n) (cos (x * (10 / fromIntegral n)))
    | x <- fromIntegral <$> [0 .. n]
    ]

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

lineHudExample :: Chart b
lineHudExample = 
    hud (hudbits "Line Chart" Nothing ["hockey stick", "slope", "vertical"]
          ((`LegendLine` 0.05) <$> lopts) $
          hudRange_ .~ Just (range ls) $
          def)

gopts3 :: (Renderable (Path V2 Double) b) => [GlyphOptions b]
gopts3 =
      zipWith
        (\x y ->
           def
           { glyphColor = withOpacity (d3Colors1 x) 0.2
           , glyphBorderColor = withOpacity (d3Colors1 x) 1
           , glyphBorderSize = 0.005
           , glyphShape = y
           , glyphSize = 0.08
           })
        [6,8,2]
        [triangle, square, circle . (0.5*)]

glineChart_Example :: Chart b
glineChart_Example = glineChart_ lopts gopts3 sixbyfour ls

glineHudExample :: Chart b
glineHudExample = 
    hud ( hudLegends_ . each . legendGap_ .~ 0.2 $
          hudTitles_ . each . _1 . titleGap_ .~ 0.2 $
          hudbits "Gline Chart" Nothing ["triangle", "square", "circle"]
          (zipWith3 LegendGLine gopts3 lopts (repeat 0.1)) $
          hudAxes_ .~ [] $
          hudRange_ .~ Just (Rect 0 5 0 5) $
          def) <>
    glineChart lopts gopts3 sixbyfour (Rect 0 5 0 5) ls <>
    lglyphChart_Example sixbyfour

rect_Example :: Double -> Chart b
rect_Example n =
  labelled (opts (Pair n 1)) "z,w" $
  labelled (opts (Pair n -1)) "z,y" $
  labelled (opts (Pair (-n) 1)) "x,w" $
  labelled (opts (Pair (-n) -1)) "x,y" $
  rect_ def (Ranges (n *. one) one)
  where
    opts o =
      def
      { labelText =
          (labelText def) {textColor = withOpacity black 0.8, textSize = 0.3}
      , labelOrientation = o
      }

rectsExample :: Chart b
rectsExample =
  rects def $
  zipWith (\x y -> Rect x (x + 1) 0 y) [0 ..] [1, 2, 3, 5, 8, 0, -2, 11, 2, 1]

ropts :: [RectOptions]
ropts =
      [ def {rectBorderSize = 0}
      , def {rectBorderSize = 0, rectColor = ucolor 0.3 0.3 0.3 0.2}
      ]

rss :: [[Rect Double]]
rss = zipWith (\x y -> Rect x (x + 1) 0 y) [0 ..] <$> pss
  where
    pss =
      transpose
        [ [exp (-(x ** 2) / 2), 0.5 * exp (-(x ** 2) / 8)]
        | x <- grid LowerPos (Range -5 5) 1000
        ]

rectChart_Example :: Chart b
rectChart_Example = rectChart_ ropts widescreen rss

rectHudExample :: Chart b
rectHudExample = 
    hud ( hudLegends_ . each . legendPlace_ .~ PlaceBottom $
          hudLegends_ . each . legendAlign_ .~ AlignCenter $
          hudbits "Rect Chart" Nothing ["blue gaussian", "grey wider distribution"]
          ((`LegendRect` 0.05) <$> ropts) $
          hudRange_ .~ Just (fold $ fold rss) $
          hudAspect_ .~ widescreen $
          hudAxes_ .~ [defXAxis] $
          def)

pixel_Example :: Chart b
pixel_Example = text_ opt "I'm a pixel!" <> pixel_ (Pixel one ublue)
  where
    opt = def {textColor = withOpacity black 0.8, textSize = 0.2}

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
  pixelChart_
    asquare
    [ [ Pixel
        (Rect x (x + 0.05) y (y + 0.05))
        (blend (x * y + x * x) (rybColor 0 `withOpacity` 1) ublue)
      | x <- grid MidPos (one :: Range Double) 20
      , y <- grid MidPos (one :: Range Double) 20
      ]
    ]

pixelateChartExample :: Chart b
pixelateChartExample = pixelateChart def asquare one (\(Pair x y) -> x*y+x*x)

pixelHudExample :: Chart b
pixelHudExample = 
    hud (hudbits "Pixel Chart" Nothing ["red", "blue"]
          ((`LegendPixel` 0.05) <$> ropts) $
          hudRange_ .~ Just one $
          hudAspect_ .~ asquare $
          def)

arrowsExample :: Chart b
arrowsExample =
  arrows
    (def
     {arrowMaxLength = 0.5, arrowMaxHeadLength = 0.2, arrowMaxStaffWidth = 0.01})
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
    hud ( hudbits "Arrow Chart" Nothing ["this way up"] [] $
--          ((`LegendArrow` 0.05) <$> [def]) $
          hudRange_ .~ Nothing $
          hudAspect_ .~ asquare $
          def)

hudExample :: Chart b
hudExample = hud def

withHudExample :: Chart b
withHudExample = withHud hopts (lineChart lopts) ls
  where
    hopts =
      def
      { hudTitles = [(def, "withHud Example")]
      , hudLegends =
          [ def
            { legendChartType =
                zipWith
                  (\x y -> (LegendLine x 0.05, y))
                  lopts
                  ["line1", "line2", "line3"]
            }
          ]
      }

axisExample :: Chart b
axisExample = axis aopts one (Range 0 100000)
  where
    aopts =
      def
      { axisLabel =
          (axisLabel def)
          { labelGap = 0.0001
          , labelText =
              (labelText (axisLabel def))
              {textSize = 0.06, textAlignH = AlignLeft, textRotation = -45}
          }
      }

legends :: [(LegendType b, Text)]
legends =
  [(LegendText def, "legend")] <> [(LegendPixel (blob ublue) 0.05, "pixel")] <>
    -- [ (LegendArrow (def {arrowMinStaffWidth=0.01,
    --                     arrowMinHeadLength=0.03}) 0.05, "arrow")] <>
  [(LegendRect def 0.05, "rect")] <>
  [(LegendGLine def def 0.10, "glyph+line")] <>
  [(LegendGlyph def, "just a glyph")] <>
  zipWith
    (\x y -> (LegendLine x 0.05, y))
    lopts
    ["short", "much longer name", "line 3"]

legendExample :: Chart b
legendExample = legend $ def {legendChartType = legends}

mainExample :: Chart b
mainExample = withHud opts (lineChart lopts) ls
  where
    opts =
      hudTitles_ .~ titles $ hudAxes_ .~
      [ defXAxis
      , defYAxis
      , axisLabel_ . labelOrientation_ .~ Pair 0 1 $ axisPlace_ .~ PlaceTop $
        defXAxis
      , axisLabel_ . labelOrientation_ .~ Pair 1 0 $ axisPlace_ .~ PlaceRight $
        defYAxis
      ] $
      hudAxes_ %~
      map (axisPad_ .~ 1) $
      hudLegends_ .~
      [legendChartType_ .~ legends $ def] $
      def

titles :: [(TitleOptions, Text)]
titles =
  [ (def, "Example Chart")
  , ( titleAlign_ .~ AlignCenter $ titleText_ . textRotation_ .~ 90 $ titleText_ .
      textSize_ .~
      0.12 $
      titlePlace_ .~
      PlaceLeft $
      def
    , "left axis title")
  , ( titleText_ . textColor_ .~ ublue $ titleText_ . textSize_ .~ 0.08 $
      titleAlign_ .~
      AlignRight $
      titlePlace_ .~
      PlaceBottom $
      def
    , "bottom right, non-essential note")
  ]

scaleExample :: IO ()
scaleExample =
    fileSvg "other/scaleExample.svg" (300,120) $ withHud (hudAspect_ .~ widescreen $ hudRange_ .~ Just (Rect 0 12 0 0.2) $ def) (lineChart (repeat def)) ((\x -> [Pair x 0, Pair x (x/100)]) <$> [0..10])

-- gallery
scatterHistExample :: [[Pair Double]] -> Chart b
scatterHistExample xys =
    beside (r2 (1,0))
    (beside (r2 (0,-1))
    (sc1 <> hud1)
    (reflectY histx))
    (reflectY $ rotateBy (3/4) histy)
  where
    sopts =
        zipWith3 (\x y z -> GlyphOptions x y (ucolor 0 0 0 0) 0 z)
        [0.01,0.02,0.03]
        ((\x -> withOpacity (d3Colors1 x) 0.3) <$> [6,8])
        [circle, triangle, square]

    mainAspect = Aspect $ Rect -0.5 0.5 -0.5 0.5
    minorAspect = Aspect $ Rect -0.5 0.5 -0.1 0.1
    sc1 = glyphChart_ sopts mainAspect xys
    histx = rectChart_ defHist minorAspect hx
    histy = rectChart_ defHist minorAspect hy
    hud1 = hud (hudAxes_ .~ [axisPlace_ .~ PlaceTop $
                             axisLabel_ . labelOrientation_ .~ Pair 0 1 $
                             def] $
                hudAspect_ .~ mainAspect $
                hudRange_ .~ Just (range xys) $
                def)
    defHist =
        (\x -> rectBorderSize_ .~ 0 $
         rectColor_ .~ d3Colors1 x `withOpacity` 0.5 $
         def) <$> [6,8]
    hx = makeHist 50 . fmap (view _x) <$> xys
    hy = makeHist 50 . fmap (view _y) <$> xys


labelledBarExample :: Chart b
labelledBarExample =
    rectChart_ [def]
    sixbyfour
    [rs] <>
    textChart (repeat (textColor_ .~ ucolor 0.33 0.33 0.33 0.8 $ def)) sixbyfour
    (Rect -0.5 9.5 (-2) 11)
    [zipWith (\x y -> (show y, Pair x ((if y>0 then -1 else 0.5) + y))) [0..] ys] <>
    hud
    ( hudAxes_ .~
      [ axisTickStyle_ .~
        TickLabels labels' $
        def
      ]
      $ hudAspect_ .~ sixbyfour
      $ hudRange_ .~ Just (fold (abs <$> rs))
      $ def
    )
  where
    labels' = fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..]
    rs :: [Rect Double]
    rs = (\(Ranges a b) -> Ranges (abs a) (abs b)) <$> zipWith4 Rect [0..10] [1..11] (replicate 11 0) ys
    ys = [1,2,3,5,8,0,-2,11,2,1]

skinnyExample :: IO (Diagram B)
skinnyExample = do
    qs <- makeQuantiles 20
    qs' <- makeQuantiles 4
    let r = Ranges (space qs) (Range 0 0.2)
    let hud' =
            hud (HudOptions 1.1
                 [axisLabel_ . labelText_ . textSize_ .~ 0.25 $ def] [] []
                 (Just r)
                 skinny clear)
    let labels' = textChart
            [textAlignH_ .~ AlignLeft $
             textRotation_ .~ 45 $
             textSize_ .~ 0.2 $
             def] skinny r
            [zipWith (\x y -> (x,Pair y 0.05))
             ["min","3rd Q","median","1st Q","max"] qs']
    let ticks' = glyphChart [def] skinny r [(`Pair` 0.02) <$> qs]
    pure $ hud' <> ticks' <> labels'

histDiffExample :: ([Rect Double],[Rect Double]) -> Chart b
histDiffExample (h1, h2) =
    let deltah = zipWith (\(Rect x y z w) (Rect _ _ _ w') -> Rect x y z (w-w')) h1 h2
        mainAspect = Aspect (Rect -0.75 0.75 -0.5 0.5)
        botAspect = Aspect (Rect -0.75 0.75 -0.2 0.2)
        (Ranges rx ry) = fold $ fold [h1,h2]
        (Ranges _ deltary) = fold (abs <$> deltah)
    in
      pad 1.1 $
        beside (r2 (0,-1))
          (rectChart
            [ rectBorderColor_ .~ ucolor 0 0 0 0 $
              rectColor_ .~ ucolor 0.365 0.647 0.855 0.2 $
              def
            , rectBorderColor_ .~ ucolor 0 0 0 0 $
              rectColor_ .~ ucolor 0.333 0.333 0.333 0.2 $
              def ]
            mainAspect
            (Ranges rx ry)
            [h1,h2])
         (rectChart
          [ rectBorderColor_ .~ ucolor 0 0 0 0 $
            rectColor_ .~ ucolor 0.88 0.53 0.23 0.8 $
            def ]
         botAspect
         (Ranges rx deltary)
         [deltah] <>
         hud (hudAspect_ .~ botAspect $
              hudRange_ .~ Just (Ranges rx deltary) $
              def))


clip :: Rect Double -> Chart b -> Chart b
clip (Rect xl xu yl yu) c =
    clipped (pathFromLocTrail $
             moveTo (p2(xl,yl)) $
             scaleY (yu - yl) $
             scaleX (xu - xl) $
             moveOriginTo (p2(-0.5,-0.5))
             unitSquare) c

grp :: Int -> [a] -> [[a]]
grp n = unfoldr
        (\x -> let y = splitAt n x in
            if null (fst y) then Nothing else Just y)

-- | chop a chart extent into a double list of Rects
chop :: Pair Int -> QDiagram b V2 Double Any -> [[Rect Double]]
chop p@(Pair _ n) ch = grp n $ gridSpace (Rect xl xu yl yu) p
  where
    (xl,xu) = fromMaybe (-0.5,0.5) (extentX ch)
    (yl,yu) = fromMaybe (-0.5,0.5) (extentY ch)

exampleClipping :: RectOptions -> Double -> Int -> QDiagram B V2 Double Any -> QDiagram B V2 Double Any
exampleClipping rcfg p n ch =
    stack (Pair 0 1) (pad p . centerXY) $
    hori (\a -> pad p $ bound rcfg 1 $ centerXY $ clip a ch) <$> chop (Pair n n) ch

exampleClipping' :: RectOptions -> Double -> Double -> Int -> QDiagram Rasterific V2 Double Any -> QDiagram Rasterific V2 Double Any
exampleClipping' rcfg rot p n ch =
    stack (Pair 0 1) (pad p . centerXY) $
    hori (\a -> Diagrams.Prelude.rotate (rot @@ deg) $
           pad p $ bound rcfg 1 $ centerXY $ clip a ch) <$> chop (Pair n n) ch

animationExample :: FilePath -> IO ()
animationExample f = do
    let c = (\x -> bound (rectColor_ .~ ucolor 1 1 1 0.1 $ def) 1 $
            exampleClipping' (rectBorderSize_ .~ 0.001 $
                              rectColor_ .~ ucolor 1 1 1 0.1 $ def)
               (x*12) 1 5 lineChart_Example) <$> [0..30]
    animatedGif f (mkSizeSpec (Just <$> r2 (600,400))) LoopingNever 20 c

schoolbookExample :: Chart b
schoolbookExample =
    pad 1.1 $
    lineChart_
    (repeat $
      lineColor_ .~ ucolor 0.4 0.5 0.2 0.8 $
      lineSize_ .~ 0.001 $
      def) asquare $
    (\x -> [Pair -10.0 (-10 + 2*x), Pair 10.0 (-10 + 2*x)]) . 
    fromIntegral <$> ([0..10] :: [Int])

main :: IO ()
main = do
  fileSvg "other/text_Example.svg" (400, 100) text_Example
  fileSvg "other/textsExample.svg" (400, 100) textsExample
  fileSvg "other/textChart_Example.svg" (300, 100) textChart_Example
  fileSvg "other/labelledExample.svg" (300, 100) labelledExample
  fileSvg "other/glyph_Example.svg" (400, 100) glyph_Example
  fileSvg "other/glyphsExample.svg" (400, 100) glyphsExample
  fileSvg "other/glyphChart_Example.svg" (450, 150) glyphChart_Example
  fileSvg "other/lglyphsExample.svg" (400, 100) lglyphsExample
  fileSvg "other/lglyphChart_Example.svg" (600, 200) (lglyphChart_Example widescreen)
  fileSvg "other/linesExample.svg" (400, 100) (linesExample 100)
  fileSvg "other/lineChart_Example.svg" (300, 200) lineChart_Example
  fileSvg "other/glineChart_Example.svg" (300, 200) glineChart_Example
  fileSvg "other/rect_Example.svg" (300, 200) $ rect_Example 2
  fileSvg "other/rectsExample.svg" (300, 200) rectsExample
  fileSvg "other/rectChart_Example.svg" (300, 100) rectChart_Example
  fileSvg "other/pixel_Example.svg" (100, 100) pixel_Example
  fileSvg "other/pixelsExample.svg" (300, 100) pixelsExample
  fileSvg "other/pixelChart_Example.svg" (300, 300) pixelChart_Example
  fileSvg "other/arrowsExample.svg" (100, 300) arrowsExample
  fileSvg "other/arrowChart_Example.svg" (300, 300) arrowChart_Example
  fileSvg "other/hudExample.svg" (300, 300) hudExample
  fileSvg "other/withHudExample.svg" (300, 200) withHudExample
  fileSvg "other/axisExample.svg" (400, 100) axisExample
  fileSvg "other/legendExample.svg" (300, 300) legendExample
  fileSvg "other/mainExample.svg" (600, 400) mainExample

  -- readme images
  fileSvg "other/textHudExample.svg" (600, 400) $
      textHudExample <> textChart_Example
  fileSvg "other/glyphHudExample.svg" (600, 400) $
      glyphHudExample <> glyphChart_Example
  fileSvg "other/lglyphHudExample.svg" (600, 400) $
      lglyphHudExample <> lglyphChart_Example widescreen
  fileSvg "other/lineHudExample.svg" (600, 400) $
      lineHudExample <> lineChart_Example
  fileSvg "other/glineHudExample.svg" (600, 400)
      glineHudExample
  fileSvg "other/rectHudExample.svg" (600, 400) $
      rectHudExample <> rectChart_Example
  fileSvg "other/pixelHudExample.svg" (600, 400) $
      pixelHudExample <> pixelateChartExample
  fileSvg "other/arrowHudExample.svg" (600, 400) $
      arrowHudExample <> arrowChart_Example

  -- gallery
  xys <- mkScatterData
  putStrLn ("scatterHistExample" :: Text)
  fileSvg "other/scatterHistExample.svg" (600,400) (scatterHistExample xys)
  putStrLn ("labelledBarExample" :: Text)
  fileSvg "other/labelledBarExample.svg" (600,400) labelledBarExample
  putStrLn ("skinnyExample" :: Text)
  skinnyExample' <- skinnyExample
  fileSvg "other/skinnyExample.svg" (600,150) skinnyExample'
  putStrLn ("histDiffExample" :: Text)
  hs <- makeHistDiffExample
  fileSvg "other/histDiffExample.svg" (600,600) $
      histDiffExample hs
  putStrLn ("clippingExample" :: Text)
  fileSvg "other/clippingExample.svg" (600,600) $
      exampleClipping (rectColor_ .~ ucolor 0.3 0.3 0.3 0.1 $ def) 1.1 5
      lineChart_Example
  putStrLn ("animationExample" :: Text)
  animationExample "other/animationExample.gif"
  putStrLn ("schoolbookExample" :: Text)
  fileSvg "other/schoolbookExample.svg" (400,400) schoolbookExample

  -- small hud examples
  renderRasterific "other/hud.png" (dims (r2(100,100))) (showOrigin $ hud def :: QDiagram Rasterific V2 Double Any)
  fileSvg "other/hud.svg" (100,100) (showOrigin $ hud def)
  scaleExample
