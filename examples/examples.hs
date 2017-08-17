{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

import Chart
import NumHask.Prelude
import Diagrams.Prelude hiding ((*.), aspect, scaleY, width)
import Diagrams.Backend.SVG (B)

import FakeData

import Data.List ((!!), zipWith3)
import Data.Text (pack)
import Data.Colour.Palette.ColorSet

scratch :: Diagram SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600,400)

exampleTitles :: [(TitleConfig, Text)]
exampleTitles =
     [ (def, "Example Chart")
     , ((titleAlign .~ AlignCenter $
         titleText . textRotation .~ 90 $
         titleText . textSize .~ 0.12 $
         titlePlace .~ PlaceLeft $ def), "left axis title")
     , ((titleText . textColor .~ withOpacity (d3Colors1 4) 0.8 $
         titleText . textSize .~ 0.08 $
         titleAlign .~ AlignRight $
         titlePlace .~ PlaceBottom $ def, "bottom right, non-essential note"))
     ]

exampleLegend :: LegendConfig
exampleLegend =
    legendChartType .~
    [ (LegendText def, "legend")] <>
    [ (LegendPixel (blob (withOpacity blue 0.4)) 0.05, "pixel")] <>
    [ (LegendArrow (arrowMinHeadLength .~ 0.03 $
                     arrowMinStaffWidth .~ 0.01 $ def) 1, "arrow")] <>
    [ (LegendRect def 0.05, "rect")] <>
    [ (LegendGLine def def 0.05, "glyph+line")] <>
    [ (LegendGlyph def, "just a glyph")] <>
    zipWith (\x y -> (LegendLine x 0.05, y))
    lineDefs ["short", "much longer name", "line 3"] $
    def

exampleChart :: Diagram B
exampleChart = exampleLine <>
    axes
    (chartTitles .~ exampleTitles $
     chartAxes .~
     [ defXAxis,
       defYAxis,
       axisLabel . labelOrientation .~ Pair 0 1 $
       axisPlace .~ PlaceTop $ defXAxis,
       axisLabel . labelOrientation .~ Pair 1 0 $
       axisPlace .~ PlaceRight $ defYAxis] $
      chartAxes %~ (map (axisPad .~ 1)) $
      chartLegends .~ [exampleLegend] $
      def)

exampleEmpty :: Diagram B
exampleEmpty = withChart def (\_ _ -> mempty) [corners one]

exampleGrid :: Diagram B
exampleGrid =
    scatterChart_
    [GlyphConfig 0.01 (color 0.376 0.741 0.408 1.00) (color 0 0 0 0) 0 circle]
    sixbyfour
    [Pair <$> [0..10] <*> [0..10]]

lineDefs :: [LineConfig]
lineDefs = zipWith (\x y -> LineConfig x (withOpacity (d3Colors1 y) 0.6)) [0.01,0.02,0.005] [0,1,2]

lineData :: [[Pair Double]]
lineData =
    fmap (uncurry Pair) <$>
    [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
    , [(0.0,0.0),(3.0,3.0)]
    , [(0.5,4.0),(0.5,0)]
    ]

exampleLine :: Diagram B
exampleLine = lineChart_ lineDefs sixbyfour lineData

exampleLineAxes :: Diagram B
exampleLineAxes =
    lineChart_ lineDefs sixbyfour lineData <>
    axes (chartRange .~ Just (range lineData) $ def)

exampleLineAxes2 :: Diagram B
exampleLineAxes2 =
    withChart def (lineChart lineDefs) lineData

scatterDefs :: ( ) => [GlyphConfig]
scatterDefs = zipWith3 (\x y z -> GlyphConfig x y (color 0 0 0 0) 0 z) [0.01,0.02,0.03] ((\x -> withOpacity (d3Colors1 x) 0.5) <$> [1,2,3]) [circle, triangle, square]
 
exampleScatter :: [[Pair Double]] -> Diagram B
exampleScatter xys =
    withChart (chartAspect .~ asquare $ def) (scatterChart scatterDefs) xys

exampleScatter2 :: [[Pair Double]] -> Diagram B
exampleScatter2 xys =
    withChart (chartAspect .~ asquare $ def) (scatterChart scatterDefs) xys1
  where
    xys1 = fmap (over _x (*1e8) . over _y (*1e-8)) <$> xys

histDefs :: [RectConfig]
histDefs =
    [ def
    , rectBorderColor .~ color 0 0 0 0
      $ rectColor .~ color 0.333 0.333 0.333 0.4
      $ def
    ]

exampleHist :: [[Rect Double]] -> Diagram B
exampleHist rs =
    histChart_ histDefs widescreen rs <>
    axes
    ( chartRange .~ Just (fold . fold $ rs)
    $ chartAspect .~ widescreen
    $ def)

exampleHistGrey :: [Rect Double] -> Diagram B
exampleHistGrey xys =
    histChart_ [histDefs!!1] widescreen [xys] <>
    axes
    ( chartRange .~ Just (fold xys)
    $ chartAspect .~ widescreen
    $ def)

-- exampleHistCompare :: DealOvers -> Histogram -> Histogram -> Diagram B
-- exampleHistCompare o h1 h2 = undefined
    -- histCompareChart (fromHist o h1) (fromHist o h2)

exampleHistGrey2 :: [Rect Double] -> Diagram B
exampleHistGrey2 rs =
    lineChart_ lineDefs widescreen
    [(\(Rect x z _ w) -> Pair ((x+z)/(one+one)) w) <$> rs] <>
    axes
    ( chartRange .~ Just (fold rs)
    $ chartAspect .~ widescreen
    $ def)

exampleLabelledBar :: Diagram B
exampleLabelledBar =
    histChart_ [def]
    sixbyfour
    [rs] <>
    axes
    ( chartAxes .~
      [ axisTickStyle .~
        TickLabels labels' $ def
      ]
      $ chartAspect .~ sixbyfour
      $ chartRange .~ Just (fold rs)
      $ def
    )
  where
    labels' = fmap pack <$> take 10 $ (:[]) <$> ['a'..]
    rs :: [Rect Double]
    rs = (\(Ranges a b) -> Ranges (abs a) (abs b)) <$> zipWith4 Rect [0..10] [1..11] (replicate 11 0) [1,2,3,5,8,0,-2,11,2,1]

exampleArrow :: ArrowConfig Double -> [Arrow] -> Diagram B
exampleArrow cfg xs =
    arrowChart_ cfg asquare (normArrows xs) <>
    axes ( chartRange .~ Just (space $ pos <$> normArrows xs)
           $ chartAspect .~ asquare $ def)
{-
-- clipping
exampleClipping :: Diagram B
exampleClipping =
    foldl' step1 mempty $
    (\y -> foldl' step mempty
      ((`Ranges` y) <$> qb)) <$> qb
  where
    -- box = rect_ (RectConfig 0 (color 0 0 0 0.1) (color 0 0 0 0))
    step x a = beside (r2 (1,0)) x (pad 1.05 $ center $ clip ch a)
    step1 x a = beside (r2 (0,1)) x (pad 1.05 $ center a)
    qs = fromIntegral <$> ([0..4] :: [Int]) :: [Double]
    qb = (\x -> Range (-0.5 + x*0.2) (-0.5 + (x+1.0)*0.2)) <$> qs
    clip ch1 (Ranges x y) = clipped (pathFromLocTrail $ unitSquare # scaleX (width x) # scaleY (width y)) ch1
    ch = lineChart_ lineDefs asquare lineData
-}

-- compound charts
exampleCompound :: IO [Chart [[Pair Double]]]
exampleCompound = do
    xys <- mkScatterData
    let qsc = Chart (scatterChart scatterDefs) (range xys) xys
    let xy0 = range (lineData <> xys)
    pure [qsc, qaxes xy0, qline]
  where
      qline = Chart (lineChart lineDefs) (range lineData) lineData
      qaxes xy =
          Chart
          (\asp _ _ -> axes
            ( chartRange .~ Just xy
            $ chartAspect .~ asp
            $ def)) xy []

exampleScatterHist :: [[Pair Double]] -> Diagram B
exampleScatterHist xys =
    beside (r2 (1,0))
    (beside (r2 (0,-1))
    (sc1 <> axes1)
    (reflectY histx))
    (reflectY $ rotateBy (3/4) histy)
  where
    mainAspect = Aspect $ Rect -0.5 0.5 -0.5 0.5
    minorAspect = Aspect $ Rect -0.5 0.5 -0.1 0.1
    sc1 = scatterChart_ [glyphColor .~ color 0.365 0.647 0.855 0.1 $ def, def]
        mainAspect xys
    histx = histChart_ defHist minorAspect hx
    histy = histChart_ defHist minorAspect hy
    defHist =
        [ def
        , rectBorderColor .~ color 0 0 0 0
          $ rectColor .~ color 0.333 0.333 0.333 0.4
          $ def
        ]
    hx = makeHist 50 . fmap (view _x) <$> xys
    hy = makeHist 50 . fmap (view _y) <$> xys
    axes2 =
        [ axisLabel . labelText .~
          ( textAlignV .~ AlignMid $
            textAlignH .~ AlignCenter $
            def) $
          axisLabel . labelStrut .~ 0.04 $
          axisOrientation .~ X $
          axisPlace .~ PlaceTop $
          def,
          axisGap .~ 0.05 $
          axisLabel . labelText .~
          ( textAlignV .~ AlignMid $
            textAlignH .~ AlignCenter $
            def) $
          axisOrientation .~ Y $
          axisPlace .~ PlaceLeft $
          def]
    axes1 =
        axes
        (ChartConfig 1 axes2 [] [] (Just $ (foldMap space xys :: Rect Double)) mainAspect clear)

exampleGgplot :: Diagram B
exampleGgplot =
    lineChart_ (repeat (LineConfig 0.002 (color 0.98 0.98 0.98 1))) sixbyfour (gridX <> gridY) <>
    -- blob (color 0.92 0.92 0.92 1) (box (Rect -0.75 0.75 -0.5 0.5)) <>
    axes ggdef
  where
    gridX = (\x -> [Pair 0 x, Pair 10 x]) . fromIntegral <$> ([0..10] :: [Int])
    gridY = (\x -> [Pair x 0, Pair x 10]) . fromIntegral <$> ([0..10] :: [Int])
    ggdef =
        ChartConfig
        1.1
        [defx]
        []
        []
        (Just $ Rect 0 10 0 10)
        sixbyfour
        clear
    defx =
        AxisConfig
        1.0
        X
        PlaceBottom
        (box (color 0 0 0 0.2))
        0.001
        def
        0.02
        0.02
        def
        (TickRound 5)

examplePixels :: Diagram B
examplePixels =
    pixelf
    (pixelGrain .~ Pair 10 10 $ def)
    asquare (Rect -1 1 -1 1)
    (\(Pair x y) -> x*y+x*x) <>
    axes
    ( chartRange .~ Just (Rect -1 1 -1 1)
    $ chartAspect .~ asquare
    $ def)
 
makeOneDim :: IO [[(Double,Text)]]
makeOneDim = do
    qs <- makeQuantiles 20
    qs5 <- makeQuantiles 4
    let labels5 = ["min","3rd Q","median","1st Q","max"]
    pure [zip qs5 labels5, zip qs (repeat "")]
 
exampleOneDim :: [[(Double,Text)]] -> Diagram B
exampleOneDim qss =
    axes (ChartConfig 1.1  [def] [] []
          (Just (Ranges (space $ fst <$> (qss!!0)) (Range 0 0)))
          skinny clear) <>
    axes (ChartConfig 1.1
          [ axisRect . rectColor .~ transparent $
            axisMark . glyphSize .~ 0.05 $
            axisMark . glyphColor .~ (withOpacity red 0.3) $
            axisTickStyle .~ TickPlaced (qss!!1) $
            def]
           []
           []
          (Just (Ranges (space $ fst <$> (qss!!1)) (Range 0 0)))
          skinny clear) <>
    axes (ChartConfig 1.1
          [ axisRect . rectColor .~ transparent $
            axisMark . glyphSize .~ 0.1 $
            axisLabel . labelText .~ (textSize .~ 0.08 $ def) $
            axisMark . glyphColor .~ (withOpacity black 0.5) $
            axisTickStyle .~ TickPlaced (qss!!0) $
            def]
           []
           []
          (Just (Ranges (space $ fst <$> (qss!!0)) (Range 0 0)))
          skinny clear)
 
{-
exampleLineAnim :: Aspect -> Rect Double -> Diagram Rasterific
exampleLineAnim asp r =
    axes
    ( chartRange .~ Just r
    $ chartAspect .~ asp
    $ def) -- <>
    -- blob (Color 1 1 1 1)
    -- (scaleX (1.5 * width aspx) $
    -- scaleY (1.5 * width aspy) $
    --  unitSquare)

exampleScaleAnimation :: FilePath -> IO ()
exampleScaleAnimation f = do
    let c = exampleLineAnim sixbyfour <$> (\x -> (1.5**x) *. one) <$> [-10..10]
    animatedGif f (mkSizeSpec (Just <$> r2 (600,400))) LoopingNever 100 c
-}

alphabetaTest :: (Double, Double) -> ((Double, Double), QDiagram SVG V2 Double Any) -> ((Double, Double), QDiagram SVG V2 Double Any) -> IO ()
alphabetaTest s (sa, a) (sb, b) = do
    fileSvg "other/testAlpha.svg" sa a
    fileSvg "other/testBeta.svg" sb b
    fileSvg "other/testab.svg" s (a <> b)

main :: IO ()
main = do
  let sOne = (400,400)
  let s6by4 = (600,400)
  let sWide = (750,250)

  fileSvg "other/exampleChart.svg" s6by4 exampleChart
  putStrLn ("exampleAxes" :: Text)
  fileSvg "other/exampleGgplot.svg" sWide exampleGgplot
  putStrLn ("exampleGgplot" :: Text)
  fileSvg "other/exampleGrid.svg" sOne exampleGrid
  putStrLn ("exampleGrid" :: Text)
  fileSvg "other/exampleLine.svg" s6by4 exampleLine
  putStrLn ("exampleLine" :: Text)
  fileSvg "other/exampleLineAxes.svg" s6by4 exampleLineAxes
  putStrLn ("exampleLineAxes" :: Text)
  fileSvg "other/exampleLineAxes2.svg" s6by4 exampleLineAxes2
  putStrLn ("exampleLineAxes2" :: Text)
  xys <- mkScatterData
  fileSvg "other/exampleScatter.svg" sOne (exampleScatter xys)
  putStrLn ("exampleBox" :: Text)
  fileSvg "other/exampleScatter2.svg" sOne (exampleScatter2 xys)
  putStrLn ("exampleScatter2" :: Text)
  xs <- mkHistData
  fileSvg "other/exampleHist.svg" sWide (exampleHist xs)
  putStrLn ("exampleHist" :: Text)
  rectqs <- makeRectQuantiles 50
  fileSvg "other/exampleHistUnequal.svg" sWide (exampleHistGrey rectqs)
  putStrLn ("exampleHistUnequal" :: Text)
  fileSvg "other/exampleHistUnequal2.svg" sWide (exampleHistGrey2 rectqs)
  putStrLn ("exampleHistUnequal2" :: Text)
  -- hs <- mkHistogramData
  --  fileSvg "other/exampleHistCompare.svg" s6by4 (exampleHistCompare (IncludeOvers 1) (hs!!0) (hs!!1))
  putStrLn ("exampleHistCompare skipped" :: Text)
  fileSvg "other/exampleScatterHist.svg" sOne (pad 1.1 $ center $ exampleScatterHist xys)
  putStrLn ("exampleScatterHist" :: Text)
  fileSvg "other/exampleLabelledBar.svg" s6by4 exampleLabelledBar
  putStrLn ("exampleLabelledBar" :: Text)
  fileSvg "other/exampleArrow.svg" sOne (exampleArrow def (arrowData (Pair 10 10)))
  putStrLn ("exampleArrow" :: Text)
  exc <- exampleCompound
  fileSvg "other/exampleCompound.svg" s6by4 (pad 1.1 $ center $ combine golden exc)
  putStrLn ("exampleCompound" :: Text)
  -- fileSvg "other/exampleClipping.svg" sOne exampleClipping
  putStrLn ("exampleClipping skipped" :: Text)
  fileSvg "other/examplePixels.svg" sOne examplePixels
  putStrLn ("examplePixels" :: Text)
  qss <- makeOneDim
  fileSvg "other/exampleOneDim.svg" (750,150) (exampleOneDim qss)
  putStrLn ("exampleOneDim" :: Text)
  -- exampleScaleAnimation "other/anim.gif"
  putStrLn ("anim.gif skipped" :: Text)

  scratch $ unitSquare

