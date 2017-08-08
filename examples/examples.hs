{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif
  
import Chart
import NumHask.Prelude
import Diagrams.Prelude hiding ((*.), aspect, scaleX, scaleY, width)
import Diagrams.Backend.Rasterific

import FakeData

import Data.List ((!!))
import Data.Text (pack)

scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600,400)

exampleBox :: Chart a
exampleBox = box one

exampleAxes :: Chart' a
exampleAxes = axes def

exampleEmpty :: Chart' a
exampleEmpty = withChart def (\_ _ -> mempty) [corners one]

exampleGrid :: Chart a
exampleGrid =
    scatterChart_
    [ScatterConfig 0.01 (Color 0.376 0.741 0.408 1.00)]
    sixbyfour
    [Pair <$> [0..10] <*> [0..10]]

lineDefs :: [LineConfig]
lineDefs =
    [ LineConfig 0.01 (Color 0.945 0.345 0.329 0.8)
    , LineConfig 0.02 (Color 0.698 0.569 0.184 0.5)
    , LineConfig 0.005 (Color 0.5 0.5 0.5 1.0)
    ]

lineData :: [[Pair Double]]
lineData =
    fmap (uncurry Pair) <$>
    [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
    , [(0.0,0.0),(3.0,3.0)]
    , [(0.5,4.0),(0.5,0)]
    ]

exampleLine :: Chart a
exampleLine = lineChart_ lineDefs sixbyfour lineData

exampleLineAxes :: Chart' a
exampleLineAxes =
    lineChart_ lineDefs sixbyfour lineData <>
    axes (chartRange .~ Just (range lineData) $ def)

exampleLineAxes2 :: Chart' a
exampleLineAxes2 =
    withChart def (lineChart lineDefs) lineData

scatterDefs :: [ScatterConfig]
scatterDefs = zipWith ScatterConfig [0.01,0.02,0.03] (opacs 0.5 palette)

exampleScatter :: [[Pair Double]] -> Chart' a
exampleScatter xys =
    withChart (chartAspect .~ asquare $ def) (scatterChart scatterDefs) xys

exampleScatter2 :: [[Pair Double]] -> Chart' a
exampleScatter2 xys =
    withChart (chartAspect .~ asquare $ def) (scatterChart scatterDefs) xys1
  where
    xys1 = fmap (over _x (*1e8) . over _y (*1e-8)) <$> xys

histDefs :: [RectConfig]
histDefs =
    [ def
    , rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.4
      $ def
    ]

exampleHist :: [[Rect Double]] -> Chart' a
exampleHist rs =
    histChart_ histDefs widescreen rs <>
    axes
    ( chartRange .~ Just (fold . fold $ rs)
    $ chartAspect .~ widescreen
    $ def)

exampleHistGrey :: [Rect Double] -> Chart' a
exampleHistGrey xys =
    histChart_ [histDefs!!1] widescreen [xys] <>
    axes
    ( chartRange .~ Just (fold xys)
    $ chartAspect .~ widescreen
    $ def)

exampleHistCompare :: DealOvers -> Histogram -> Histogram -> Chart' a
exampleHistCompare o h1 h2 =
    histCompareChart (fromHist o h1) (fromHist o h2)

exampleHistGrey2 :: [Rect Double] -> Chart' a
exampleHistGrey2 rs =
    lineChart_ lineDefs widescreen
    [(\(Rect x z _ w) -> Pair ((x+z)/(one+one)) w) <$> rs] <>
    axes
    ( chartRange .~ Just (fold rs)
    $ chartAspect .~ widescreen
    $ def)

exampleLabelledBar :: Chart' a
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

exampleArrow :: ArrowConfig Double -> [Arrow] -> Chart' a
exampleArrow cfg xs =
    arrowChart_ cfg asquare (normArrows xs) <>
    axes ( chartRange .~ Just (space $ pos <$> normArrows xs)
           $ chartAspect .~ asquare $ def)

-- clipping
exampleClipping :: Chart a
exampleClipping =
    foldl' step1 mempty $
    (\y -> foldl' step mempty
      ((`Ranges` y) <$> qb)) <$> qb
  where
    step x a = beside (r2 (1,0)) x (pad 1.05 $ center $ blob (Color 0 0 0 0.02) box a <> clip ch a)
    step1 x a = beside (r2 (0,1)) x (pad 1.05 $ center a)
    qs = fromIntegral <$> ([0..4] :: [Int]) :: [Double]
    qb = (\x -> Range (-0.5 + x*0.2) (-0.5 + (x+1.0)*0.2)) <$> qs
    clip ch1 sq = clipped (pathFromLocTrail $ box sq) ch1
    ch = lineChart_ lineDefs asquare lineData

-- compound charts
exampleCompound :: IO [QChart a [[Pair Double]]]
exampleCompound = do
    xys <- mkScatterData
    let qsc = QChart (scatterChart scatterDefs) (range xys) xys
    let xy0 = range (lineData <> xys)
    pure [qsc, qaxes xy0, qline]
  where
      qline = QChart (lineChart lineDefs) (range lineData) lineData
      qaxes xy =
          QChart
          (\asp _ _ -> axes
            ( chartRange .~ Just xy
            $ chartAspect .~ asp
            $ def)) xy []

exampleScatterHist :: [[Pair Double]] -> Chart' a
exampleScatterHist xys =
    beside (r2 (1,0))
    (beside (r2 (0,-1))
    (sc1 <> axes1)
    (reflectY histx))
    (reflectY $ rotateBy (3/4) histy)
  where
    mainAspect = Aspect $ Rect -0.5 0.5 -0.5 0.5
    minorAspect = Aspect $ Rect -0.5 0.5 -0.1 0.1
    sc1 = scatterChart_ [scatterColor .~ Color 0.365 0.647 0.855 0.1 $ def, def]
        mainAspect xys
    histx = histChart_ defHist minorAspect hx
    histy = histChart_ defHist minorAspect hy
    defHist =
        [ def
        , rectBorderColor .~ Color 0 0 0 0
          $ rectColor .~ Color 0.333 0.333 0.333 0.4
          $ def
        ]
    hx = makeHist 50 . fmap (view _x) <$> xys
    hy = makeHist 50 . fmap (view _y) <$> xys
    axes2 =
        [ axisLabelText .~
          ( textBottom .~ 0.65 $
            textRight .~ 0.5 $
            def) $
          axisLabelStrut .~ 0.04 $
          axisOrientation .~ X $
          axisPlacement .~ AxisTop $
          def,
          axisInsideStrut .~ 0.05 $
          axisLabelText .~
          ( textBottom .~ 0.65 $
            textRight .~ 0.5 $
            def) $
          axisOrientation .~ Y $
          axisPlacement .~ AxisLeft $
          def]
    axes1 =
        axes
        (ChartConfig 1 axes2 (Just $ (foldMap space xys :: Rect Double)) mainAspect (Color 0.5 0.5 1 0.04))

exampleGgplot :: Chart' a
exampleGgplot =
    lineChart_ (repeat (LineConfig 0.002 (Color 0.98 0.98 0.98 1))) sixbyfour (gridX <> gridY) <>
    blob (Color 0.92 0.92 0.92 1) (box (Rect -0.75 0.75 -0.5 0.5)) <>
    axes ggdef
  where
    gridX = (\x -> [Pair 0 x, Pair 10 x]) . fromIntegral <$> ([0..10] :: [Int])
    gridY = (\x -> [Pair x 0, Pair x 10]) . fromIntegral <$> ([0..10] :: [Int])
    ggdef =
        ChartConfig
        1.1
        [defx]
        (Just $ Rect 0 10 0 10)
        sixbyfour
        (Color 1 1 1 0)
    defx =
        AxisConfig
        1.0
        X
        AxisBottom
        0.001
        (Color 0 0 0 0.2)
        0.018
        (Color 0 0 0 1)
        0
        0.01
        (TextConfig 1 0.03 (Color 0.2 0.2 0.2 0.7) 0.5 1 0)
        (TickRound 5)

examplePixels :: Chart' a
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

exampleOneDim :: [[(Double,Text)]] -> Chart' a
exampleOneDim qss =
    axes (ChartConfig 1.1 [def]
          (Just (Ranges (space $ fst <$> (qss!!0)) (Range 0 0)))
          skinny (uncolor transparent)) <>
    axes (ChartConfig 1.1
          [ axisColor .~ uncolor transparent $
            axisMarkSize .~ 0.05 $
            axisMarkColor .~ uncolor (withOpacity red 0.3) $
            axisTickStyle .~ TickPlaced (qss!!1) $
            def]
          (Just (Ranges (space $ fst <$> (qss!!1)) (Range 0 0)))
          skinny (uncolor transparent)) <>
    axes (ChartConfig 1.1
          [ axisColor .~ uncolor transparent $
            axisMarkSize .~ 0.1 $
            axisLabelText .~ (textSize .~ 0.08 $ def) $
            axisMarkColor .~ uncolor (withOpacity black 0.5) $
            axisTickStyle .~ TickPlaced (qss!!0) $
            def]
          (Just (Ranges (space $ fst <$> (qss!!0)) (Range 0 0)))
          skinny (uncolor transparent))

exampleLineAnim :: Aspect -> Rect Double -> Chart' a
exampleLineAnim asp@(Aspect (Ranges aspx aspy)) r =
    axes
    ( chartRange .~ Just r
    $ chartAspect .~ asp
    $ def) <>
    blob (Color 1 1 1 1)
    (scaleX (1.5 * width aspx) $
     scaleY (1.5 * width aspy) $
     unitSquare)

exampleScaleAnimation :: FilePath -> IO ()
exampleScaleAnimation f = do
    let c = exampleLineAnim sixbyfour <$> (\x -> (1.5**x) *. one) <$> [-10..10]
    animatedGif f (mkSizeSpec (Just <$> r2 (600,400))) LoopingNever 100 c

exampleAxesAutoX :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Int -> Double -> Chart' a
exampleAxesAutoX x rig bot rot siz gap' pad' tic asp =
    axes ( chartCanvasColor .~ (Color 0 0 0 0.3) $
           chartAspect .~ aspect asp $
           chartRange .~ Just (Rect 0 x 0 1) $
           chartAxes .~
           [ axisLabelText .~
             ( textPad .~ pad' $
               textSize .~ siz $
               textRight .~ rig $
               textBottom .~ bot $
               textRotation .~ rot $
               def) $
             axisLabelStrut .~ gap' $
             axisTickStyle .~ TickRound tic $
             def] $
           def)


alphabetaTest a b s = do
    fileSvg "other/testAlpha.svg" s a
    fileSvg "other/testBeta.svg" s b
    fileSvg "other/testab.svg" s (a <> b)

main :: IO ()
main = do
  let sOne = (400,400)
  let s6by4 = (600,400)
  let sWide = (750,250)

  fileSvg "other/exampleBox.svg" s6by4 exampleBox
  putStrLn ("exampleBox" :: Text)
  fileSvg "other/exampleAxes.svg" s6by4 exampleAxes
  putStrLn ("exampleAxes" :: Text)
  fileSvg "other/exampleEmpty.svg" s6by4 exampleEmpty
  putStrLn ("exampleEmpty" :: Text)
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
  hs <- mkHistogramData
  fileSvg "other/exampleHistCompare.svg" s6by4 (exampleHistCompare (IncludeOvers 1) (hs!!0) (hs!!1))
  putStrLn ("exampleHistCompare" :: Text)
  fileSvg "other/exampleScatterHist.svg" sOne (pad 1.1 $ center $ exampleScatterHist xys)
  putStrLn ("exampleScatterHist" :: Text)
  fileSvg "other/exampleLabelledBar.svg" s6by4 exampleLabelledBar
  putStrLn ("exampleLabelledBar" :: Text)
  fileSvg "other/exampleArrow.svg" sOne (exampleArrow def (arrowData (Pair 10 10)))
  putStrLn ("exampleArrow" :: Text)
  exc <- exampleCompound
  fileSvg "other/exampleCompound.svg" s6by4 (pad 1.1 $ center $ combine golden exc)
  putStrLn ("exampleCompound" :: Text)
  fileSvg "other/exampleClipping.svg" sOne exampleClipping
  putStrLn ("exampleClipping" :: Text)
  fileSvg "other/examplePixels.svg" sOne examplePixels
  putStrLn ("examplePixels" :: Text)
  qss <- makeOneDim
  fileSvg "other/exampleOneDim.svg" (750,150) (exampleOneDim qss)
  putStrLn ("exampleOneDim" :: Text)
  exampleScaleAnimation "other/anim.gif"
  putStrLn ("anim.gif" :: Text)

  fileSvg "other/axesVar.svg" (400,800)  $
      foldl' (beside (r2 (0,-1))) mempty
      ((\x -> exampleAxesAutoX 1000 0.5 0.5 0 0.05 0.05 1 x 5) <$>
       [0..10])
  putStrLn ("axesVar" :: Text)

  scratch $ unitSquare <> text_ def "text"

