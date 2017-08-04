{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
#if ( __GLASGOW_HASKELL__ < 820 )
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

import Chart
import NumHask.Prelude
import Diagrams.Prelude
 
import Codec.Picture.Gif (GifDelay)
import Diagrams.Backend.Rasterific.CmdLine (B, GifOpts(..))
import Diagrams.Backend.CmdLine (DiagramOpts(..), mainRender)

import FakeData

import qualified Control.Foldl as L
import Data.List ((!!))
import Data.Text (pack)

scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600,400)

exampleBox :: Chart a
exampleBox = box one

exampleAxes :: Chart' a
exampleAxes = axes def

exampleEmpty :: Chart' a
exampleEmpty =
    withChart def (\_ _ -> mempty) [corners one]

exampleGrid :: Chart a
exampleGrid =
    scatterChart
    [ScatterConfig 0.01 (palette!!4)]
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
exampleLine = lineChart lineDefs sixbyfour lineData

exampleLineAxes :: Chart' a
exampleLineAxes =
    lineChart lineDefs sixbyfour lineData <>
    axes (chartRange .~ Just (rangeR2s lineData) $ def)

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

-- | withCharts doesn't work with a V4 where w and z effect the XY plane range
exampleHist :: [[Rect Double]] -> Chart' a
exampleHist rs =
    histChart histDefs widescreen rs <>
    axes
    ( chartRange .~ Just (fold . fold $ rs)
    $ chartAspect .~ widescreen
    $ def)

exampleHistGrey :: [Rect Double] -> Chart' a
exampleHistGrey xys =
    histChart [histDefs!!1] widescreen [xys] <>
    axes
    ( chartRange .~ Just (fold xys)
    $ chartAspect .~ widescreen
    $ def)

exampleHistCompare :: DealOvers -> Histogram -> Histogram -> Chart' a
exampleHistCompare o h1 h2 =
    let h = fromHist o h1
        h' = fromHist o h2
        h'' = zipWith (\(Rect x y z w) (Rect _ _ _ w') -> Rect x y z (w-w')) h h'
        flat = Aspect $ Rect -0.75 0.75 -0.25 0.25
    in
      pad 1.1 $
        beside (r2 (0,-1)) (histChart
        [ def
        , rectBorderColor .~ Color 0 0 0 0
        $ rectColor .~ Color 0.333 0.333 0.333 0.1
        $ def ] sixbyfour [h,h'] <>
        axes (ChartConfig 1.1
              [def]
              (Just (fold $ fold [h,h']))
              sixbyfour (uncolor transparent)))
        (histChart
        [ rectBorderColor .~ Color 0 0 0 0
        $ rectColor .~ Color 0.888 0.333 0.333 0.8
        $ def ] flat [h''] <>
        axes (ChartConfig 1.1
              [ axisAlignedTextBottom .~ 0.65 $
                axisAlignedTextRight .~ 1 $
                axisOrientation .~ Y $
                axisPlacement .~ AxisLeft $
                def
              ]
              (Just (fold h''))
              flat (uncolor transparent)))
 
exampleHistGrey2 :: [Rect Double] -> Chart' a
exampleHistGrey2 rs =
    lineChart lineDefs widescreen
    [(\(Rect x z _ w) -> Pair ((x+z)/(one+one)) w) <$> rs] <>
    axes
    ( chartRange .~ Just (fold rs)
    $ chartAspect .~ widescreen
    $ def)

exampleLabelledBar :: Chart' a
exampleLabelledBar =
    histChart [def]
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
    arrowChart cfg (Aspect one) (normArrows xs) <>
    axes ( chartRange .~ Just (space $ pos <$> normArrows xs)
           $ chartAspect .~ asquare $ def)

-- clipping
exampleClipping :: Chart a
exampleClipping =
    L.fold (L.Fold step1 mempty identity) $
    (\y -> L.fold (L.Fold step mempty identity)
      ((`Ranges` y) <$> qb)) <$> qb
  where
    step x a = beside (r2 (1,0)) x (pad 1.05 $ center $ blob (Color 0 0 0 0.02) box a <> clip ch a)
    step1 x a = beside (r2 (0,1)) x (pad 1.05 $ center a)
    qs = fromIntegral <$> ([0..4] :: [Int]) :: [Double]
    qb = (\x -> (-0.5 + x*0.2) ... (-0.5 + (x+1.0)*0.2)) <$> qs
    clip ch1 sq = clipped (pathFromLocTrail $ box sq) ch1
    ch = lineChart lineDefs asquare lineData

-- compound charts
exampleCompound :: IO [QChart a [[Pair Double]]]
exampleCompound = do
    xys <- mkScatterData
    let qsc = QChart (scatterChart scatterDefs) (rangeR2s xys) xys
    let xy0 = rangeR2s $ lineData <> xys
    pure [qsc, qaxes xy0, qline]
  where
      qline = QChart (lineChart lineDefs) (rangeR2s lineData) lineData
      qaxes xy =
          QChart
          (\a _ -> axes
            ( chartRange .~ Just xy
            $ chartAspect .~ a
            $ def)) xy []

exampleScatterHist :: [[Pair Double]] -> Chart' a
exampleScatterHist xys =
    beside (r2 (1,0))
    (beside (r2 (0,-1))
    (sc1 <> axes1)
    (reflectY histx))
    (reflectY $ rotateBy (3/4) histy)
  where
    sc1 = scatterChart [scatterColor .~ Color 0.365 0.647 0.855 0.1 $ def, def] asquare xys
    histx = histChart defHist xyHist hx
    histy = histChart defHist xyHist hy
    defHist =
        [ def
        , rectBorderColor .~ Color 0 0 0 0
          $ rectColor .~ Color 0.333 0.333 0.333 0.4
          $ def
        ]
    xyHist = Aspect (Ranges one ((0.2*)<$>one))
    hx = makeHist 50 . fmap (view _x) <$> xys
    hy = makeHist 50 . fmap (view _y) <$> xys
    axes2 =
        [ axisAlignedTextBottom .~ 0.65 $
          axisAlignedTextRight .~ 0.5 $
          axisLabelStrut .~ 0.04 $
          axisOrientation .~ X $
          axisPlacement .~ AxisTop $
          def,
          axisInsideStrut .~ 0.05 $
          axisAlignedTextBottom .~ 0.65 $
          axisAlignedTextRight .~ 1 $
          axisOrientation .~ Y $
          axisPlacement .~ AxisLeft $
          def]
    axes1 =
        axes
        (ChartConfig 1 axes2 (Just $ rangeR2s xys) asquare (Color 0.5 0.5 1 0.04))

exampleGgplot :: Chart' a
exampleGgplot =
    lineChart (repeat (LineConfig 0.002 (Color 0.98 0.98 0.98 1))) sixbyfour (gridX <> gridY) <>
    blob (Color 0.92 0.92 0.92 1) (box (Ranges ((1.5*) <$> one) one)) <>
    axes ggdef
  where
    gridX = (\x -> [Pair 0 x, Pair 10 x]) . fromIntegral <$> ([0..10] :: [Int])
    gridY = (\x -> [Pair x 0, Pair x 10]) . fromIntegral <$> ([0..10] :: [Int])
    ggdef =
        ChartConfig
        1.1
        [defx]
        (Just $ Ranges (0 ... 10) (0 ... 10))
        sixbyfour
        (Color 1 1 1 0)
    defx =
        AxisConfig
        1.0
        X
        AxisBottom
        0
        (Color 0 0 0 0.2)
        0.018
        (Color 0 0 0 1)
        0
        0.01
        0.04
        (Color 0.2 0.2 0.2 0.7)
        (TickRound 5)
        0.5
        1

examplePixels :: Chart' a
examplePixels =
    pixelf
    (pixelGrain .~ Pair 10 10 $ def)
    asquare (Ranges (-1 ... 1) (-1 ... 1))
    (\(Pair x y) -> x*y+x*x) <>
    axes
    ( chartRange .~ Just (Ranges (-1 ... 1) (-1 ... 1))
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
    axes (ChartConfig 1.1 [def] (Just (Ranges (space $ fst <$> (qss!!0)) (Range 0 0))) skinny (uncolor transparent)) <>
    axes (ChartConfig 1.1 [axisColor .~ uncolor transparent $ axisMarkSize .~ 0.05 $ axisMarkColor .~ uncolor (withOpacity red 0.3) $ axisTickStyle .~ TickPlaced (qss!!1) $ def] (Just (Ranges (space $ fst <$> (qss!!1)) (Range 0 0))) skinny (uncolor transparent)) <>
    axes (ChartConfig 1.1 [axisColor .~ uncolor transparent $ axisMarkSize .~ 0.1 $ axisTextSize .~ 0.08 $ axisMarkColor .~ uncolor (withOpacity black 0.5) $ axisTickStyle .~ TickPlaced (qss!!0) $ def] (Just (Ranges (space $ fst <$> (qss!!0)) (Range 0 0))) skinny (uncolor transparent))
  where
    skinny = Aspect (Ranges ((5*) <$> one) one)

displayHeader :: FilePath -> [(Diagram B, GifDelay)] -> IO ()
displayHeader fn =
  mainRender ( DiagramOpts (Just 900) (Just 700) fn
             , GifOpts {_dither = False, _noLooping = False, _loopRepeat = Just 2}
             )

chartRange' :: [[Rect Double]] -> Rect Double
chartRange' = fold . fold

exampleHistAnim :: Rect Double -> Aspect -> [[Rect Double]] -> Chart' a
exampleHistAnim cr asp rs =
    histChartWithRange cr histDefs asp rs <>
    axes
    ( chartRange .~ Just cr
    $ chartAspect .~ asp
    $ def)

exampleAnimation :: FilePath -> IO ()
exampleAnimation f = do
    xs <- mkHistData
    let yss = inits (xs!!0)
    let cr = chartRange' yss
    let us :: [Diagram B ]
        us = exampleHistAnim cr widescreen . pure <$> yss
 
    displayHeader f $ zip us (repeat (10 :: Int))

main :: IO ()
main = do
  let sOne = (400,400)
  let s6by4 = (600,400)
  let sWide = (750,250)

  fileSvg "other/exampleBox.svg" s6by4 exampleBox
  fileSvg "other/exampleAxes.svg" s6by4 exampleAxes
  fileSvg "other/exampleEmpty.svg" s6by4 exampleEmpty
  fileSvg "other/exampleGgplot.svg" sWide exampleGgplot
  fileSvg "other/exampleGrid.svg" sOne exampleGrid
  fileSvg "other/exampleLine.svg" s6by4 exampleLine
  fileSvg "other/exampleLineAxes.svg" s6by4 exampleLineAxes
  fileSvg "other/exampleLineAxes2.svg" s6by4 exampleLineAxes2
  xys <- mkScatterData
  fileSvg "other/exampleScatter.svg" sOne (exampleScatter xys)
  fileSvg "other/exampleScatter2.svg" sOne (exampleScatter2 xys)
  xs <- mkHistData
  fileSvg "other/exampleHist.svg" sWide (exampleHist xs)
  rectqs <- makeRectQuantiles 50
  fileSvg "other/exampleHistUnequal.svg" sWide (exampleHistGrey rectqs)
  fileSvg "other/exampleHistUnequal2.svg" sWide (exampleHistGrey2 rectqs)
  hs <- mkHistogramData
  fileSvg "other/exampleHistCompare.svg" s6by4 (exampleHistCompare (IncludeOvers 1) (hs!!0) (hs!!1))
  fileSvg "other/exampleScatterHist.svg" sOne (pad 1.1 $ center $ exampleScatterHist xys)
  fileSvg "other/exampleLabelledBar.svg" s6by4 exampleLabelledBar
  fileSvg "other/exampleArrow.svg" sOne (exampleArrow def (arrowData (Pair 10 10)))
  exc <- exampleCompound
  fileSvg "other/exampleCompound.svg" s6by4 (pad 1.1 $ center $ combine golden exc)
  fileSvg "other/exampleClipping.svg" sOne exampleClipping
  fileSvg "other/examplePixels.svg" sOne examplePixels
  qss <- makeOneDim
  fileSvg "other/exampleOneDim.svg" (750,150) (exampleOneDim qss)
  exampleAnimation "other/anim.gif"

  scratch (exampleHistCompare (IncludeOvers 1) (hs!!0) (hs!!1))
  -- fileSvg "other/scratchpad.svg" (600,150) $ pad 1.1 $
  --     showOrigin $ exampleOneDim qss
