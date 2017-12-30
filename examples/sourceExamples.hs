{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE IncoherentInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Chart
import Control.Lens hiding (beside)
import Data.List (zipWith3, (!!), head)
import Diagrams.Backend.SVG (B, SVG)
import Diagrams.Prelude hiding ((*.), scaleX, scaleY, (<>))
import FakeData
import Formatting
import NumHask.Prelude
import qualified Data.Text as Text
import Data.Generics.Labels()
import Data.Time
import Data.Time.Calendar.WeekDate

hudbits :: Text -> Maybe Text -> [Text] -> [LegendType] -> HudOptions -> HudOptions
hudbits t subt ts ls x =
    #titles .~
    [ (#place .~ PlaceLeft $
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
    [ #chartType .~ zip ls ts
    $ #align .~ AlignRight
    $ def
    ] $
    #axes . each . #gap .~ 0.1 $
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
  textChart_ (repeat $ #size .~ 0.33 $ def) widescreen [ts]

textHudExample :: Chart b
textHudExample = hud
    ( hudbits "Text Chart" (Just "text and glyphs have a similar feel") [] [] $
      #range .~ Just (range ts) $
      #aspect .~ widescreen $
      def)

labelledExample :: Chart b
labelledExample =
  labelled
    (LabelOptions
       (#alignH .~ AlignLeft $ #rotation .~ 45 $ def)
       (Pair 1 1)
       0.05)
    "a label"
    (glyph_ def)

glyph_Example :: Chart b
glyph_Example = glyph_ def

glyphsExample :: Chart b
glyphsExample = glyphs def (dataXY sin (Range 0 (2*pi)) 30)

gopts :: [GlyphOptions]
gopts = [ #borderSize .~ 0.001 $ def
        , #borderSize .~ 0.001 $
          #size .~ 0.1 $
          #color .~ rybColor 7 `withOpacity` 0.4 $
          #shape .~ Triangle $ def
        ]

gdata :: [[Pair Double]]
gdata = [ dataXY sin (Range 0 (2*pi)) 30
        , dataXY cos (Range 0 (2*pi)) 30
        ]

glyphChart_Example :: Chart b
glyphChart_Example = glyphChart_ gopts widescreen gdata

glyphHudExample :: Chart b
glyphHudExample = 
    hud (#legends . each . #align .~ AlignLeft $
         hudbits "Glyph Chart" (Just "text elements are paths not svg text") ["sin", "cos"]
          (LegendGlyph <$> gopts) $
          #range .~ Just (range gdata) $
          #aspect .~ widescreen $
          #axes .~ [ #label . #text . #size .~ 0.2 $
                        #tickStyle .~ TickPlaced pis $
                        #label . #text . #font .~ Lin $
                        defXAxis
                      , defYAxis
                      ] $
          def)
    where
      pis = [ (0,"zero")
            , (pi/2, "π/2")
            , (pi, "π")
            , (3 * pi / 2, "3π/2")
            , (2*pi,"2π")
            ]

lglyphsExample :: Chart b
lglyphsExample =
  lglyphs def def $
  zip (show <$> [0 ..]) [Pair (x / 10) (sin x / 10) | x <- [0 .. 10]]


lgdata :: [[(Text, Pair Double)]]
lgdata =
    [(\(p@(Pair x y)) -> (show x <> "," <> show y, fromIntegral <$> p)) <$> g]
  where
    g = Pair <$> [0 .. 5] <*> [0 .. 5] :: [Pair Int]

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

lglyphHudExample :: Chart b
lglyphHudExample = hud
    ( #titles . each . _1 . #gap .~ 0.2 $
      hudbits "LGlyph Chart" (Just "Glyphs with text labels are very useful") [] [] $
      #axes .~ [] $
      #range .~ Just (range (fmap snd <$> lgdata)) $
      #aspect .~ widescreen $
      def)

linesExample :: Int -> Chart b
linesExample n = lines (#color .~ red `withOpacity` 0.5 $ def)
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

lineHudExample :: Chart b
lineHudExample = 
    hud (hudbits "Line Chart" Nothing ["hockey stick", "slope", "vertical"]
          ((`LegendLine` 0.05) <$> lopts) $
          #range .~ Just (range ls) $
          def)

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

glineHudExample :: Chart b
glineHudExample = 
    hud ( #legends . each . #gap .~ 0.2 $
          #titles . each . _1 . #gap .~ 0.2 $
          hudbits "Gline Chart" Nothing ["triangle", "square", "circle"]
          (zipWith (\x y -> LegendGLine x y 0.1) gopts3 lopts) $
          #axes .~ [] $
          #range .~ Just (Rect 0 5 0 5) $
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
rss = [ rectXY (\x -> exp (-(x ** 2) / 2)) (Range -5 5) 50
      , rectXY (\x -> 0.5 * exp (-(x ** 2) / 8)) (Range -5 5) 50
      ]

rectChart_Example :: Chart b
rectChart_Example = rectChart_ ropts widescreen rss

rectHudExample :: Chart b
rectHudExample =
    hud ( #legends . each . #place .~ PlaceBottom $
          #legends . each . #align .~ AlignCenter $
          hudbits "Rect Chart" Nothing ["blue gaussian", "grey wider distribution"]
          ((`LegendRect` 0.05) <$> ropts) $
          #range .~ Just (fold $ fold rss) $
          #aspect .~ widescreen $
          #axes .~ [defXAxis] $
          def)

pixel_Example :: Chart b
pixel_Example = text_ opt "I'm a pixel!" <> pixel_ (Pixel one ublue)
  where
    opt = #color .~ withOpacity black 0.8 $
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
    [ (\(r,c) -> Pixel r
                (blend c
                 (ucolor 0.47 0.73 0.86 1)
                 (ucolor 0.01 0.06 0.22 1)
                )) <$>
      rectF (\(Pair x y) -> (x+y)*(x+y))
      one (Pair 40 40)
    ]
 
pixelateChartExample :: Chart b
pixelateChartExample = pixelateChart def asquare one (\(Pair x y) -> (x+y)*(x+y))

pixelHudExample :: Chart b
pixelHudExample =
    hud (hudbits "Pixel Chart" Nothing ["red", "blue"]
          ((`LegendPixel` 0.05) <$> ropts) $
          #range .~ Just one $
          #aspect .~ asquare $
          def)

arrowsExample :: Chart b
arrowsExample =
  arrows
    ( #maxLength .~ 0.5 $
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
    hud ( hudbits "Arrow Chart" Nothing ["this way up"] [] $
--          ((`LegendArrow` 0.05) <$> [def]) $
          #range .~ Nothing $
          #aspect .~ asquare $
          def)

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
          ]
          $ def

axisExample :: Chart b
axisExample = axis aopts one (Range 0 100000)
  where
    aopts :: AxisOptions
    aopts =
        #label . #text %~
        ((#rotation .~ -45) .
         (#size .~ 0.06) .
         (#alignH .~ AlignLeft)) $
        #gap .~ 0.0001 $
        def

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

mainExample :: Chart b
mainExample = withHud opts (lineChart lopts) ls
  where
    opts =
      #titles .~ titles $ #axes .~
      [ defXAxis
      , defYAxis
      , #label . #orientation .~ Pair 0 1 $ #place .~ PlaceTop $
        defXAxis
      , #label . #orientation .~ Pair 1 0 $ #place .~ PlaceRight $
        defYAxis
      ] $
      #axes %~
      map (#outerPad .~ 1) $
      #legends .~
      [#chartType .~ legends $ def] $
      def

titles :: [(TitleOptions, Text)]
titles =
  [ (def, "Example Chart")
  , ( #align .~ AlignCenter $ #text . #rotation .~ 90 $ #text .
      #size .~
      0.12 $
      #place .~
      PlaceLeft $
      def
    , "left axis title")
  , ( #text . #color .~ ublue $ #text . #size .~ 0.08 $
      #align .~
      AlignRight $
      #place .~
      PlaceBottom $
      def
    , "bottom right, non-essential note")
  ]

scaleExample :: IO ()
scaleExample =
    fileSvg "other/scaleExample.svg" (300,120) $ withHud
      ( #aspect .~ widescreen $
        #range .~ Just (Rect 0 12 0 0.2) $
        def)
      (lineChart (repeat def))
      (vlineOneD ((0.01*) <$> [0..10]))

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
        [Circle, Triangle, Square]

    mainAspect = Rect -0.5 0.5 -0.5 0.5
    minorAspect = Rect -0.5 0.5 -0.1 0.1
    sc1 = glyphChart_ sopts mainAspect xys
    histx = rectChart_ defHist minorAspect hx
    histy = rectChart_ defHist minorAspect hy
    hud1 = hud (#axes .~ [#place .~ PlaceTop $
                             #label . #orientation .~ Pair 0 1 $
                             def] $
                #aspect .~ mainAspect $
                #range .~ Just (range xys) $
                def)
    defHist =
        (\x -> #borderSize .~ 0 $
         #color .~ d3Colors1 x `withOpacity` 0.5 $
         def) <$> [6,8]
    hx = makeHist 50 . fmap (view _x) <$> xys
    hy = makeHist 50 . fmap (view _y) <$> xys


barExample :: Chart b
barExample  =
    barChart def (BarData [ys] Nothing Nothing) <>
    hud
    ( #titles .~ [(def,"Bar Chart")] $
      #axes .~
      [ #tickStyle .~
        TickLabels labels' $
        def
      ]
      $ #range .~ Just (fold (abs <$> rs))
      $ def
    )
  where
    labels' = fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..]
    rs = rectBars 0.1 ys
    ys = [1,2,3,5,8,0,-2,11,2,1]

data SurveyQ = SurveyQ
    { surveyTitle :: Text
    , surveyData :: [(Text,Int)]
    , surveyBarGap :: Double
    , surveyNumberDrop :: Double
    , surveyBarColor :: AlphaColour Double
    , surveyNumberColor :: AlphaColour Double
    , surveyAutoOptions :: AutoOptions
    } deriving (Show, Generic)

q7 :: SurveyQ
q7 = SurveyQ
    "How frequently do you use Haskell?"
    [ ("Daily",469)
    , ("Weekly",452)
    , ("Monthly", 215)
    , ("Yearly", 36)
    , ("Rarely", 51)
    ]
    0.2
    0.07
    (ucolor 0.341 0.224 0.388 1)
    (ucolor 1 1 0.33 1)
    (#allowDiagonal .~ False $ #maxXRatio .~ 0.16 $ def)

q24 :: SurveyQ
q24 = SurveyQ
    "Which editors do you use for Haskell?"
    [ ("Vim", 534)
    , ("Emacs",501)
    , ("VS Code", 202)
    , ("Atom", 169)
    , ("Sublime", 92)
    , ("Notepad++", 28)
    , ("VS", 3)
    , ("Other", 134)
    ]
    0.2
    (-0.03)
    (ucolor 0.341 0.224 0.388 1)
    (ucolor 0.33 0.33 0.33 1)
    def

surveyChart :: SurveyQ -> Chart b
surveyChart (SurveyQ t d bgap ngap bc tc ao) =
    surveyText tc ngap (snd <$> d) <>
    surveyBars bc bgap (snd <$> d) <>
    surveyHud ao t d

surveyBars :: AlphaColour Double -> Double -> [Int] -> Chart b
surveyBars rc gap d =
    rectChart
    [ #borderSize .~ 0
    $ #color .~ rc
    $ def
    ]
    sixbyfour
    (barRange d)
    [rectBars gap $ fromIntegral <$> d]

surveyHud :: AutoOptions -> Text -> [(Text, Int)] -> Chart b
surveyHud ao t d =
    hud 
    ( #outerPad .~ 1
    $ #titles .~ [(def, t)]
    $ #axes .~
      [ #tickStyle .~ TickRound 4
        $ defYAxis
      , adjustAxis ao aspx rx $
        #gap .~ 0 $
        #tickStyle .~ TickLabels (fst <$> d) $
        defXAxis
      ]
    $ #range .~ Just (barRange (snd <$> d) )
    $ #aspect .~ sixbyfour
    $ def)
  where
    (Ranges rx _) = barRange (snd <$> d)
    (Ranges aspx _) = sixbyfour

surveyText :: AlphaColour Double -> Double -> [Int] -> Chart b
surveyText tc gap ys =
    textChart
    (repeat (#color .~ tc $ def))
    sixbyfour
    (barRange ys)
    [zipWith (\x y ->
                (show y
                , Pair (x+0.5) ((if y>0 then -ngap else ngap) + fromIntegral y)))
      [0..] ys]
  where
    ngap = gap * fromIntegral (maximum ys) :: Double

barRange :: [Int] -> Rect Double
barRange ys = Rect 0 (fromIntegral $ length ys) 0 (fromIntegral $ maximum ys)

skinnyExample :: IO (Diagram B)
skinnyExample = do
    qs <- makeQuantiles 20
    qs' <- makeQuantiles 4
    let r = Ranges (space qs) (Range 0 0.2)
    let hud' =
            hud (HudOptions 1.1
                 [#label . #text . #size .~ 0.25 $ def] [] [] []
                 (Just r)
                 skinny clear)
    let labels' = textChart
            [#alignH .~ AlignLeft $
             #rotation .~ 45 $
             #size .~ 0.2 $
             def] skinny r
            [zipWith (\x y -> (x,Pair y 0.05))
             ["min","3rd Q","median","1st Q","max"] qs']
    let ticks' = glyphChart [def] skinny r [(`Pair` 0.02) <$> qs]
    pure $ hud' <> ticks' <> labels'

histDiffExample :: ([Rect Double],[Rect Double]) -> Chart b
histDiffExample (h1, h2) =
    let deltah = zipWith (\(Rect x y z w) (Rect _ _ _ w') -> Rect x y z (w-w')) h1 h2
        mainAspect = Rect -0.75 0.75 -0.5 0.5
        botAspect = Rect -0.75 0.75 -0.2 0.2
        (Ranges rx ry) = fold $ fold [h1,h2]
        (Ranges _ deltary) = fold (abs <$> deltah)
    in
      pad 1.1 $
        beside (r2 (0,-1))
          (rectChart
            [ #borderColor .~ ucolor 0 0 0 0 $
              #color .~ ucolor 0.365 0.647 0.855 0.2 $
              def
            , #borderColor .~ ucolor 0 0 0 0 $
              #color .~ ucolor 0.88 0.53 0.23 0.8 $
              def ]
            mainAspect
            (Ranges rx ry)
            [h1,h2])
          (rectChart
          [ #borderColor .~ ucolor 0 0 0 0 $
            #color .~ ucolor 0.88 0.53 0.23 0.8 $
            def ]
          botAspect
          (Ranges rx deltary)
          [deltah] <>
          hud ( #aspect .~ botAspect $
                #range .~ Just (Ranges rx deltary) $
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

schoolbookHud :: Chart b
schoolbookHud = hud
    ( #axes .~ [] $
      #aspect .~ asquare $
      #titles .~ [(def,"y = x² - 3")] $
      #range .~ Just (Rect -5 5 -5 5) $
      #grids .~
      [ GridOptions Vert (GridExact GridOuterPos 10) (LineOptions 0.005 schoolBlue)
      , GridOptions Hori (GridExact GridOuterPos 10) (LineOptions 0.005 schoolBlue)
      , GridOptions Vert (GridExact GridOuterPos 50) (LineOptions 0.002 schoolBlue)
      , GridOptions Hori (GridExact GridOuterPos 50) (LineOptions 0.002 schoolBlue)
      ] $
      def)
  where
    schoolBlue = ucolor 0.19 0.74 0.89 0.7

parabola :: Rect Double -> (Double -> Double) -> Int -> Range Double -> Chart b
parabola r f grain xscope = 
    lineChart [#size .~ 0.01 $ #color .~ ucolor 0.6 0.6 0.6 1 $ def] asquare r
    [dataXY f xscope grain]

ceptLines :: Renderable (Path V2 Double) b => Rect Double -> Rect Double -> (Double -> Double) -> Double -> QDiagram b V2 Double Any
ceptLines asp r@(Ranges rx ry) f x =
    mconcat $ lines (#color .~ ucolor 0.2 0.2 0.2 1 $ #size .~ 0.005 $ def) .
    fmap (Chart.project r asp) <$>
    [ [Pair (lower rx) (f x), Pair x (f x)]
    , [Pair x (lower ry), Pair x (f x)]
    ]

cepts :: Renderable (Path V2 Double) b => Rect Double -> Rect Double -> (Double -> Double) -> Double -> QDiagram b V2 Double Any
cepts a r@(Ranges rx ry) f x =
    textChart [def, #alignH .~ AlignCenter $ #rotation .~ 0 $ def]
    a r
    [ [("x = " <> sformat (fixed 1) x, Pair x (lower ry - 1))]
    , [("y = " <> sformat (fixed 1) (f x), Pair (lower rx - 1.5) (f x))]
    ]

schoolbookExample :: Double -> Chart b
schoolbookExample x =
    bound (#color .~ ucolor 1 1 1 0.1 $ def) 1.05 $
    schoolbookHud <>
    parabola r f grain xscope <>
    ceptLines asquare r f x <>
    glyphChart [#color .~ red `withOpacity` 0.5 $ def] asquare r [[Pair x (f x)]] <>
    cepts asquare r f x
  where
    f x = x*x - 3
    r = Rect -5 5 -5 5
    xscope = Range -3 3
    grain = 50

gridExample :: Chart b
gridExample =
    hud
    (def & #grids .~
     [ GridOptions Vert
       (GridExact GridOuterPos 10) (LineOptions 0.001 (black `withOpacity` 1))
     , GridOptions Hori
       (GridExact GridOuterPos 10) (LineOptions 0.001 (black `withOpacity` 1))
     ] &
     #axes . ix 0 %~
     ( (#tickStyle .~ TickPlaced
        (zip (grid OuterPos (Range -0.5 0.5) 10) (replicate 11 "abcdef"))
       ) .
       (#label . #text . #alignH .~ AlignLeft) .
       (#gap .~ 0) .
       (#label . #text . #rotation .~ -45))
    & #axes . ix 1 %~ (#label . #text . #alignH .~ AlignLeft))

timeData :: Int -> IO [Day]
timeData n = do
    now <- getCurrentTime
    let (UTCTime today _) = now
    let toWeekDay x = let (_,_,d) = toWeekDate x in d
    let isWeekend x = toWeekDay x `elem` [6,7]
    pure $ filter (not . isWeekend) $ take n $ (`addDays` today) <$> [0..]

timeExample :: [Day] -> QDiagram SVG V2 Double Any
timeExample dates =
    hud
        ( #axes .~ [ adef, defYAxis] $
          #range .~ Just r $ def) <>
        glyphChart [ #color .~ red `withOpacity` 1
                   $ #borderSize .~ 0
                   $ #size .~ 0.01
                   $ def] sixbyfour r [xs']
        <> lglyphChart [def]
        [ #shape .~ Square
        $ #color .~ blue `withOpacity` 1
        $ #borderSize .~ 0
        $ #size .~ 0.04 $ def] sixbyfour r
        [zip
         (Text.pack . formatTime defaultTimeLocale "%a, %d %b" . (\x -> dates !! x) . fst <$> ts)
         ((\x -> xs' !! x) . fst <$> ts)]
  where
    today = Data.List.head dates
    g = 6
    xs = fromIntegral . (`diffDays` today) <$> dates
    xs' = lineOneD xs
    r = range [xs']
    (Ranges rx _) = r
    (ts, _) = 
            placedTimeLabelDiscontinuous PosInnerOnly Nothing g ((`UTCTime` 0) <$> dates)
    ts' = (\(x,y) -> (fromIntegral x,y)) <$> ts
    (Ranges aspx _) = sixbyfour
    adef = adjustAxis def aspx rx $ #tickStyle .~ TickPlaced ts' $ defXAxis

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
  putStrLn ("barExample" :: Text)
  fileSvg "other/barExample.svg" (600,400) barExample


  -- A few helper examples
  putStrLn ("gridExample" :: Text)
  fileSvg "other/gridExample.svg" (600, 400) gridExample
  ts <- timeData 200
  putStrLn ("timeExample" :: Text)
  fileSvg "other/timeExample.svg" (600, 400) $ timeExample ts

  -- gallery
  xys <- mkScatterData
  putStrLn ("scatterHistExample" :: Text)
  fileSvg "other/scatterHistExample.svg" (600,400) (scatterHistExample xys)
  putStrLn ("skinnyExample" :: Text)
  skinnyExample' <- skinnyExample
  fileSvg "other/skinnyExample.svg" (600,150) skinnyExample'
  putStrLn ("histDiffExample" :: Text)
  hs <- makeHistDiffExample
  fileSvg "other/histDiffExample.svg" (600,600) $
      histDiffExample hs
  putStrLn ("clippingExample" :: Text)
  fileSvg "other/clippingExample.svg" (600,600) $
      exampleClipping (#color .~ ucolor 0.3 0.3 0.3 0.1 $ def) 1.1 5
      lineChart_Example
  putStrLn ("schoolbookExample" :: Text)
  fileSvg "other/schoolbookExample.svg" (400,400) (schoolbookExample -1)

  -- small hud examples
  fileSvg "other/hud.svg" (100,100) (showOrigin $ hud def)
  scaleExample

  -- haskell survey examples
  fileSvg "other/q7Example.svg" (600,400) $
      surveyChart q7

  fileSvg "other/q24Example.svg" (600,400) $
      surveyChart q24
