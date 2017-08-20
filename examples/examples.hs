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

titles :: [(TitleConfig, Text)]
titles =
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

legends :: LegendConfig
legends =
    legendChartType .~
    [ (LegendText def, "legend")] <>
    [ (LegendPixel (blob (withOpacity blue 0.4)) 0.05, "pixel")] <>
    [ (LegendArrow (arrowMinHeadLength .~ 0.03 $
                     arrowMinStaffWidth .~ 0.01 $ def) 1, "arrow")] <>
    [ (LegendRect def 0.05, "rect")] <>
    [ (LegendGLine def def 0.05, "glyph+line")] <>
    [ (LegendGlyph def, "just a glyph")] <>
    zipWith (\x y -> (LegendLine x 0.05, y))
    lcfgs ["short", "much longer name", "line 3"] $
    def

exampleChart :: Diagram B
exampleChart = exampleLine <>
    axes
    (chartTitles .~ titles $
     chartAxes .~
     [ defXAxis,
       defYAxis,
       axisLabel . labelOrientation .~ Pair 0 1 $
       axisPlace .~ PlaceTop $ defXAxis,
       axisLabel . labelOrientation .~ Pair 1 0 $
       axisPlace .~ PlaceRight $ defYAxis] $
      chartAxes %~ (map (axisPad .~ 1)) $
      chartLegends .~ [legends] $
      def)

lcfgs :: [LineConfig]
lcfgs = zipWith (\x y -> LineConfig x (withOpacity (d3Colors1 y) 0.6)) [0.01,0.02,0.005] [0,1,2]

lineData :: [[Pair Double]]
lineData =
    map (uncurry Pair) <$>
    [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
    , [(0.0,0.0),(3.0,3.0)]
    , [(0.5,4.0),(0.5,0)]
    ]

style' :: Text -> [Text] -> [LegendType] -> ChartConfig -> ChartConfig
style' t ts ls x =
    chartTitles .~
    [((titlePlace .~ PlaceRight $
       titleAlign .~ AlignLeft $
       titleText . textRotation .~ 270 $
       def), t)] $
    chartLegends .~
    [ legendChartType .~
      (zip ls ts) $
      legendAlign .~ AlignRight $ def
    ] $ x

exampleLine :: Diagram B
exampleLine =
    axes (style' "Line Chart" ["line 1", "line 2", "line 3"]
          ((\x -> (LegendLine x 0.05)) <$> lcfgs) $
          chartRange .~ Just (range lineData) $
          def) <>
    lineChart_ lcfgs sixbyfour lineData

gcfgs :: ( ) => [GlyphConfig]
gcfgs =
    zipWith3 (\x y z ->
                GlyphConfig x (withOpacity (d3Colors1 y) 0.2)
                (withOpacity (d3Colors1 y) 0.8) 0.01 z)
    [0.04,0.07,0.06]
    [1,2,3]
    [circle, triangle, square]

exampleGlyph :: Diagram B
exampleGlyph =
    withChart cfg (glyphChart gcfgs) lineData
  where
    cfg = style' "Glyph Chart" ["glyph 1", "glyph 2", "glyph 3"]
          (LegendGlyph <$> gcfgs) def

exampleGline :: Diagram B
exampleGline = withChart cfg (glineChart lcfgs gcfgs) lineData
  where
    cfg = style' "Glyph-Line Chart" ["glyph-line 1", "glyph-line 2", "glyph-line 3"]
          (zipWith (\x y -> LegendGLine x y 0.05) gcfgs lcfgs) def

exampleLGlyph :: Diagram B
exampleLGlyph =
    axes (chartAxes .~ [] $ chartTitles .~ [(titleStrut .~ 0.1 $ def, "Labelled Glyph Chart")] $ def) <>
    glyphChart gcfgs sixbyfour
    (range (map (\(Pair x y) -> Pair (fromIntegral x) (fromIntegral y)) <$> [g]))
    lineData  <>
    lglyphChart_
    [labelStrut .~ 0.015 $ def]
    [GlyphConfig 0.01 (withOpacity black 0.3) transparent 0 circle]
    sixbyfour
    [(\(p@(Pair x y)) -> ((show x <> "," <> show y), fromIntegral <$> p)) <$> g]
  where
    g = Pair <$> [0..5] <*> [0..5] :: [Pair Int]

scatterConfigs :: ( ) => [GlyphConfig]
scatterConfigs =
    zipWith3 (\x y z -> GlyphConfig x y (color 0 0 0 0) 0 z)
    [0.01,0.02,0.03]
    ((\x -> withOpacity (d3Colors1 x) 0.5) <$> [1,2,3])
    [circle, triangle, square]
 
exampleScatter :: [[Pair Double]] -> Diagram B
exampleScatter xys =
    withChart
    ( chartAspect .~ asquare $ cfg) (glyphChart scatterConfigs) xys1
  where
    xys1 = fmap (over _x (*1e8) . over _y (*1e-8)) <$> xys
    cfg = style' "Scatter Chart" ["orange circles", "green triangles"]
          (map (\x -> LegendGlyph x) gcfgs) def

rcfgs :: [RectConfig]
rcfgs =
    [ def
    , blob (withOpacity black 0.2)
    ]

exampleHist :: [[Rect Double]] -> Diagram B
exampleHist rs =
    histChart_ rcfgs widescreen rs <>
    axes
    ( chartRange .~ Just (fold . fold $ rs)
    $ chartAspect .~ widescreen
    $ chartAxes .~ [defXAxis]
    $ cfg)
  where
    cfg = style' "Histogram Chart" ["default", "solid grey"]
          (map (\x -> LegendRect x 0.05) rcfgs) def

exampleHistUnequal :: [Rect Double] -> Diagram B
exampleHistUnequal xys =
    histChart_ (drop 1 rcfgs) widescreen [xys] <>
    axes
    ( chartRange .~ Just (fold xys)
    $ chartAspect .~ widescreen
    $ cfg)
  where
    cfg = style' "Histogram - Unequal Bins" [] [] def
 
exampleLabelledBar :: Diagram B
exampleLabelledBar =
    histChart_ [def]
    sixbyfour
    [rs] <>
    axes
    ( chartAxes .~
      [ axisTickStyle .~
        TickLabels labels' $
        def
      ]
      $ chartAspect .~ sixbyfour
      $ chartRange .~ Just (fold rs)
      $ cfg
    )
  where
    labels' = fmap pack <$> take 10 $ (:[]) <$> ['a'..]
    rs :: [Rect Double]
    rs = (\(Ranges a b) -> Ranges (abs a) (abs b)) <$> zipWith4 Rect [0..10] [1..11] (replicate 11 0) [1,2,3,5,8,0,-2,11,2,1]
    cfg = style' "Labelled Bin Chart" [] [] def

exampleArrow :: ArrowConfig Double -> [Arrow] -> Diagram B
exampleArrow cfg xs =
    arrowChart_ cfg asquare (normArrows xs) <>
    axes ( chartRange .~ Just (space $ pos <$> normArrows xs)
           $ chartAspect .~ asquare $ def)

-- exampleHistCompare :: DealOvers -> Histogram -> Histogram -> Diagram B
-- exampleHistCompare o h1 h2 = undefined
    -- histCompareChart (fromHist o h1) (fromHist o h2)

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
    ch = lineChart_ lcfgs asquare lineData
-}

-- compound charts
exampleCompound :: IO [Chart [[Pair Double]]]
exampleCompound = do
    xys <- mkScatterData
    let qsc = Chart (glyphChart scatterConfigs) (range xys) xys
    let xy0 = range (lineData <> xys)
    pure [qsc, qaxes xy0, qline]
  where
      qline = Chart (lineChart lcfgs) (range lineData) lineData
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
    sc1 = glyphChart_ [glyphColor .~ color 0.365 0.647 0.855 0.1 $ def, def]
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
          ( textAlignH .~ AlignCenter $
            def) $
          axisLabel . labelStrut .~ 0.04 $
          axisOrientation .~ X $
          axisPlace .~ PlaceTop $
          def,
          axisGap .~ 0.05 $
          axisLabel . labelText .~
          ( textAlignH .~ AlignCenter $
            def) $
          axisOrientation .~ Y $
          axisPlace .~ PlaceLeft $
          def]
    axes1 =
        axes
        (ChartConfig 1 axes2 [] [] (Just $ (foldMap space xys :: Rect Double)) mainAspect clear)

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
histCompareChart :: [(Rect Double)] -> [(Rect Double)] -> Chart' a
histCompareChart h1 h2 =
    let deltah = zipWith (\(Rect x y z w) (Rect _ _ _ w') -> Rect x y z (w-w')) h1 h2
        mainAspect = Aspect (Rect -0.75 0.75 -0.5 0.5)
        botAspect = Aspect (Rect -0.75 0.75 -0.2 0.2)
    in
      pad 1.1 $
        beside (r2 (0,-1)) (histChart_
        [ def
        , rectBorderColor .~ UColor 0 0 0 0
        $ rectColor .~ UColor 0.333 0.333 0.333 0.1
        $ def ]
        mainAspect [h1,h2] <>
        axes (ChartOptions 1.1
              [def]
              (Just (fold $ fold [abs <$> h1,abs <$> h2]))
              mainAspect (fromColor transparent)
              True))
        (histChart_
        [ rectBorderColor .~ UColor 0 0 0 0
        $ rectColor .~ UColor 0.888 0.333 0.333 0.8
        $ def ] botAspect [abs <$> deltah] <>
        axes (ChartOptions 1.1
              [ axisLabelText .~
                ( textBottom .~ 0.65 $
                  textRight .~ 1 $
                  def) $
                axisOrientation .~ Y $
                axisPlacement .~ AxisLeft $
                def
              ]
              (Just (fold $ abs <$> deltah))
              botAspect (fromColor transparent)
              True))
-}


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

main :: IO ()
main = do
  let sOne = (400,400)
  let s6by4 = (600,400)
  let sWide = (750,250)

  fileSvg "other/exampleChart.svg" s6by4 exampleChart
  putStrLn ("example Chart" :: Text)
  fileSvg "other/exampleLine.svg" s6by4 exampleLine
  putStrLn ("example Line" :: Text)
  fileSvg "other/exampleGlyph.svg" s6by4 exampleGlyph
  putStrLn ("example Glyph" :: Text)
  fileSvg "other/exampleGline.svg" s6by4 exampleGline
  putStrLn ("example Glyph+Line" :: Text)
  fileSvg "other/exampleLGlyph.svg" s6by4 exampleLGlyph
  putStrLn ("example Labelled Glyph" :: Text)
  xys <- mkScatterData
  fileSvg "other/exampleScatter.svg" sOne (exampleScatter xys)
  putStrLn ("example Scatter" :: Text)
  xs <- mkHistData
  fileSvg "other/exampleHist.svg" sWide (exampleHist xs)
  putStrLn ("example Histogram" :: Text)
  rectqs <- makeRectQuantiles 50
  fileSvg "other/exampleHistUnequal.svg" sWide (exampleHistUnequal rectqs)
  putStrLn ("example Histogram - Unequal Bins" :: Text)
  -- hs <- mkHistogramData
  --  fileSvg "other/exampleHistCompare.svg" s6by4 (exampleHistCompare (IncludeOvers 1) (hs!!0) (hs!!1))
  putStrLn ("exampleHistCompare skipped" :: Text)
  fileSvg "other/exampleScatterHist.svg" sOne (pad 1.1 $ center $ exampleScatterHist xys)
  putStrLn ("example Scatter + side Histograms" :: Text)
  fileSvg "other/exampleLabelledBar.svg" s6by4 exampleLabelledBar
  putStrLn ("example Labelled Bar Chart" :: Text)
  fileSvg "other/exampleArrow.svg" sOne (exampleArrow def (arrowData (Pair 10 10)))
  putStrLn ("example Arrow Chart" :: Text)
  exc <- exampleCompound
  fileSvg "other/exampleCompound.svg" s6by4 (pad 1.1 $ center $ combine golden exc)
  putStrLn ("exampleCompound" :: Text)
  -- fileSvg "other/exampleClipping.svg" sOne exampleClipping
  putStrLn ("example Clipping skipped" :: Text)
  fileSvg "other/examplePixels.svg" sOne examplePixels
  putStrLn ("example Pixel Chart" :: Text)
  qss <- makeOneDim
  fileSvg "other/exampleOneDim.svg" (750,150) (exampleOneDim qss)
  putStrLn ("example OneDim Chart" :: Text)
  -- exampleScaleAnimation "other/anim.gif"
  putStrLn ("anim.gif skipped" :: Text)

  scratch $ unitSquare

