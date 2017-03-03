{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Chart.Range
import Chart.Types
import Chart.Unit

import FakeData

import qualified Control.Foldl as L
import Data.List ((!!), zipWith4)
import Data.Text (pack)
import Diagrams.Backend.SVG (SVG)
import Diagrams.Prelude hiding ((<>), arrow)
import Linear (V4(..))
import System.Random.MWC (create)
import Tower.Prelude hiding ((&))

scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600,400)

exampleBox :: Chart a
exampleBox = box sixbyfour

exampleAxes :: Chart' a
exampleAxes = axes def sixbyfour [toCorners (one::XY)]

exampleGrid :: Chart a
exampleGrid =
    scatter
    [ScatterConfig 0.01 (palette1!!4)]
    sixbyfour
    [V2 <$> [0..10] <*> [0..10]]


lineDefs :: [LineConfig]
lineDefs =
    [ LineConfig 0.01 (Color 0.945 0.345 0.329 0.8)
    , LineConfig 0.02 (Color 0.698 0.569 0.184 0.5)
    , LineConfig 0.005 (Color 0.5 0.5 0.5 1.0)
    ]

lineData :: [[V2 Double]]
lineData =
    fmap r2 <$>
    [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
    , [(0.0,0.0),(3.0,3.0)]
    , [(0.5,4.0),(0.5,0)]
    ]

exampleLine :: Chart a
exampleLine = line lineDefs sixbyfour lineData

exampleLineAxes :: Chart' a
exampleLineAxes = line lineDefs sixbyfour lineData <> axes def sixbyfour lineData

scatterDefs :: [ScatterConfig]
scatterDefs = zipWith ScatterConfig [0.01,0.02,0.03] (opacs 0.5 palette1)

exampleScatter :: [[V2 Double]] -> Chart' a
exampleScatter xys = scatter scatterDefs one xys <> axes def one xys

exampleScatter2 :: [[V2 Double]] -> Chart' a
exampleScatter2 xys =
    scatter scatterDefs one xys1 <> axes def one xys1
  where
    xys1 = fmap (over _x (*1e8) . over _y (*1e-8)) <$> xys

mkScatterData :: IO [[V2 Double]]
mkScatterData = do
    g <- create
    xys <- rvsCorr g 1000 0.7
    xys1 <- rvsCorr g 1000 -0.5
    pure [ over _y (+1) . over _x (\x -> x^2 + 3*x - 1) <$> xys
         , over _x (\x -> x^2 + 3*x + 1) <$> xys1]

histDefs :: [RectConfig]
histDefs =
    [ def
    , rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.4
      $ def
    ]

exampleHist :: [[V4 Double]] -> Chart' a
exampleHist xys =
    withAxes def (rangeRects xys) (hist histDefs) wideScreen xys

mkHistData :: IO [[V4 Double]]
mkHistData = do
    g <- create
    xys <- rvs g 1000
    xys1 <- rvs g 1000
    pure $ makeHist 30 <$> [xys,(1.5*) <$> xys1]

exampleLabelledBar :: Chart' a
exampleLabelledBar = hist
    [ def
    , rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.4
      $ def
    ]
    sixbyfour
    xys <> axes
    ( chartAxes .~
      [ axisTickStyle .~
        TickLabels labels $ def
      ]
      $ def
    ) sixbyfour [toCorners (rangeRects xys)]
  where
    labels = fmap pack <$> take 10 $ (:[]) <$> ['a'..]
    xys = [zipWith4 V4 [0..10] (replicate 11 0) [1..11] [1,2,3,5,8,0,-2,11,2,1]]

exampleArrow :: [[V4 Double]] -> Chart' a
exampleArrow xs =
    arrow [def] (V4 one one one one) xs <>
    axes def one [toCorners (rangeR2s xs)]

-- clipping
exampleClipping :: Chart a
exampleClipping =
    L.fold (L.Fold step1 mempty identity) $
    (\y -> L.fold (L.Fold step mempty identity)
      ((`V2` y) <$> qb)) <$>
    qb
  where
    step x a = beside (r2 (1,0)) x (pad 1.05 $ center $ blob (Color 0 0 0 0.02) box a <> clip ch a)
    step1 x a = beside (r2 (0,1)) x (pad 1.05 $ center a)
    qs = fromIntegral <$> [0..4] :: [Double]
    qb = (\x -> (-0.5 + x*0.2) ... (-0.5 + (x+1.0)*0.2)) <$> qs
    clip ch1 sq = clipped (pathFromLocTrail $ box sq) ch1
    ch = line lineDefs one lineData

-- compound charts
exampleCompound :: IO [QChart a]
exampleCompound = do
    xys <- mkScatterData
    let qsc = QChart (scatter scatterDefs) (rangeR2s xys) xys
    let xy = rangeR2s $ lineData <> xys
    pure [qsc, qaxes xy, qline]
  where
      qline = QChart (line lineDefs) (rangeR2s lineData) lineData
      qaxes xy = QChart (axes def) xy [toCorners xy]

exampleScatterHist :: [[V2 Double]] -> Chart' a
exampleScatterHist xys =
    beside (r2 (1,0))
    (beside (r2 (0,-1))
    (sc1 <> axes1)
    (reflectY histx))
    (reflectY $ rotateBy (3/4) histy)
  where
    sc1 = scatter [scatterColor .~ Color 0.365 0.647 0.855 0.1 $ def, def] one xys
    histx = hist defHist xyHist hx
    histy = hist defHist xyHist hy
    defHist =
        [ def
        , rectBorderColor .~ Color 0 0 0 0
          $ rectColor .~ Color 0.333 0.333 0.333 0.4
          $ def
        ]
    xyHist = V2 one ((*0.2) <$> one)
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
        (ChartConfig 1 axes2 (Color 0.5 0.5 1 0.04))
        one
        [toCorners (rangeR2s xys)]

exampleGgplot :: Chart' a
exampleGgplot =
    line (repeat (LineConfig 0.002 (Color 0.98 0.98 0.98 1))) sixbyfour (gridX <> gridY) <>
    blob (Color 0.92 0.92 0.92 1) (box sixbyfour) <>
    axes ggdef sixbyfour [toCorners (V2 (0 ... 10) (0 ... 10))]
  where
    gridX = (\x -> [V2 0 x, V2 10 x]) . fromIntegral <$> [0..10]
    gridY = (\x -> [V2 x 0, V2 x 10]) . fromIntegral <$> [0..10]
    ggdef = ChartConfig 1.1 [defx] (Color 1 1 1 0)
    defx = AxisConfig 1.0 X AxisBottom 0 (Color 0 0 0 0.2) 0.018 (Color 0 0 0 1) 0 0.01 0.04 colorAxis3 (TickRound 5) 0.5 1

main :: IO ()
main = do
  let sOne = (400,400)
  let s6by4 = (600,400)
  let sWide = (750,250)

  fileSvg "other/exampleBox.svg" s6by4 exampleBox
  fileSvg "other/exampleAxes.svg" s6by4 exampleAxes
  fileSvg "other/exampleGgplot.svg" sWide exampleGgplot
  fileSvg "other/exampleGrid.svg" sOne exampleGrid
  fileSvg "other/exampleLine.svg" s6by4 exampleLine
  fileSvg "other/exampleLineAxes.svg" s6by4 exampleLineAxes
  xys <- mkScatterData
  fileSvg "other/exampleScatter.svg" sOne (exampleScatter xys)
  fileSvg "other/exampleScatter2.svg" sOne (exampleScatter2 xys)
  xs <- mkHistData
  fileSvg "other/exampleHist.svg" sWide (exampleHist xs)
  fileSvg "other/exampleScatterHist.svg" sOne (pad 1.1 $ center $ exampleScatterHist xys)
  fileSvg "other/exampleLabelledBar.svg" s6by4 exampleLabelledBar
  fileSvg "other/exampleArrow.svg" sOne (exampleArrow [arrowData])
  exc <- exampleCompound
  fileSvg "other/exampleCompound.svg" s6by4 (pad 1.1 $ center $ combine goldenRatio exc)
  fileSvg "other/exampleClipping.svg" sOne exampleClipping

  scratch $ combine sixbyfour exc
