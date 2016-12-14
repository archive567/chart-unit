{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Chart.Types
import Chart.Unit
import qualified Control.Foldl as L
import Data.List
import qualified Data.Text as Text
import Diagrams.Backend.SVG (SVG)
import Diagrams.Prelude hiding ((<>), unit)
import FakeData
import Linear hiding (identity, unit)
import Protolude hiding ((&))
import System.Random.MWC

exampleEmptyChart :: Chart' a
exampleEmptyChart =
    chartHud
    def
    (const mempty)
    (rangeR2 ([]::[V2 Double]))
    rescaleR2s
    ([[]] :: [[V2 Double]])

exampleAxes :: Chart' a
exampleAxes = chartHud def (const mempty) (V2 (Range (-1e8,1e8)) (Range (-1e-8,1e-8))) rescaleR2s ([[]] :: [[V2 Double]])

exampleGrid :: Chart' a
exampleGrid = unitScatter def [def] [V2 <$> [0..10] <*> [0..10]]

exampleLine :: Chart' a
exampleLine = unitLine def
    (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1))
    (fmap r2 <$>
      [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)]
      , [(0.0,0.0),(3.0,3.0)]])

exampleManyLines :: [[Double]] -> Chart' a
exampleManyLines vs =
    unitLine def ((LineConfig 0.01 . opac 0.5) <$> palette1) $
    zipWith V2 (fromIntegral <$> [0..] :: [Double]) <$>
    ((drop 1 . L.scan L.sum) <$> vs)

exampleDots :: [V2 Double] -> Chart a
exampleDots = scatter1 def

exampleDotsScaled :: [V2 Double] -> Chart a
exampleDotsScaled vs = scatter1 def ((\x -> rescaleR2 (rangeR2 x) x) vs)

exampleDotsScaled2 :: [V2 Double] -> Chart' a
exampleDotsScaled2 vs = unitScatter def [def] $ fmap (* 1e-8) <$> [vs]

exampleScatter :: [V2 Double] -> Chart' a
exampleScatter vs = unitScatter def [def] [vs]

exampleScatters :: [[V2 Double]] -> Chart' a
exampleScatters = unitScatter def $
    zipWith ScatterConfig [0.02,0.05,0.1] $ opacs 0.1 palette1

exampleHist :: [Double] -> Chart' a
exampleHist vs = unitRect def [def] [makeHist 50 vs]

exampleHist2 :: [[V2 Double]] -> Chart' a
exampleHist2 vss = unitRect def
    [ def
    , rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.4
      $ def
    ]
    (fmap (makeHist 50) $ fmap (\(V2 x y) -> x+y) <$> vss)

exampleBar :: Chart' a
exampleBar =
  unitRect
    ( chartAxes .~
      [ axisTickStyle .~
        TickLabels labels $ def
      ]
      $ def
    )
    [def]
    [zipWith4 V4 [0..10] (replicate 11 0) [0..11] [1,2,3,5,8,0,-2,11]]
  where
    labels = fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..]

exampleArrows :: Chart' a
exampleArrows = unitArrow def [def] [arrowData]

exampleArrows2 :: Chart' a
exampleArrows2 = chartWith'' def (centerXY . mconcat . zipWith arrow1 [def]) (rangeV4s [arrowData]) (\r x -> fmap (rescaleV4 r) <$> x) [arrowData]

scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (300,300)

exampleOtherRange :: QDiagram SVG V2 Double Any
exampleOtherRange =
    pad 1.1 $
    beside (r2 (-1,0))
    (beside (r2 (0,-1))
     ((unitSquare # fcA (color $ Color 0.2 0.2 0.2 0.05) # lw 0) <>
       centerXY
       (scaleX (1/1.5) $
        scaleY (1/10)
        (clipped
         (pathFromTrail $
          closeTrail (fromVertices (p2 <$> [(0.5,0),(0.5,10),(2,10),(2,0)]))) $
          (mconcat . zipWith line1
            (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)))
          (fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)],
                         [(0.0,0.0),(3.0,3.0)]]))))
      (axis def (Range (0.5,2))))
    (axis (axisOrientation .~ Y $ def)
     (Range (0,2)))

-- exampleOtherRange' :: QDiagram SVG V2 Double Any
exampleOtherRange' (V2 rx@(Range (lx,ux)) ry@(Range (ly,uy))) canvas =
    pad 1.1 $
    beside (r2 (-1,0))
    (beside (r2 (0,-1))
     ((unitSquare # fcA (color $ Color 0.2 0.2 0.2 0.05) # lw 0) <>
       centerXY
       (scaleX (1/(ux-lx)) $
        scaleY (1/(uy-ly))
        (clipped box canvas)))
      (axis def rx))
    (axis (axisOrientation .~ Y $ def) ry)
  where
    box = pathFromTrail $
           closeTrail
            (fromVertices
              (p2 <$> [(lx,ly),(lx,uy),(ux,uy),(ux,ly)]))

t1 :: QDiagram SVG V2 Double Any
t1 = exampleOtherRange' (V2 (Range (1.2,2.6)) (Range (0.6,1.4)))
     ((mconcat . zipWith line1
            (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)))
          (fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)],
                         [(0.0,0.0),(3.0,3.0)]]))

t2 xys = exampleOtherRange' (V2 (Range (0.5,2)) (Range (0,10))) (scatter1 def xys)


main :: IO ()
main = do
  gen <- create
  xs <- rvs gen 1000
  xss <- replicateM 5 (rvs gen 30)
  xys <- rvsCorr gen 1000 0.7
  xys' <- rvsCorr gen 1000 -0.7
  let s = (400,400)

  scratch exampleOtherRange

  fileSvg "other/exampleEmptyChart.svg" s exampleEmptyChart
  fileSvg "other/exampleAxes.svg" s exampleAxes
  fileSvg "other/exampleGrid.svg" s exampleGrid
  fileSvg "other/exampleLine.svg" s exampleLine
  fileSvg "other/exampleManyLines.svg" s (exampleManyLines xss)
  fileSvg "other/exampleDots.svg" s (exampleDots xys)
  fileSvg "other/exampleDotsScaled.svg" s (exampleDotsScaled xys)
  fileSvg "other/exampleDotsScaled2.svg" s (exampleDotsScaled2 xys)
  fileSvg "other/exampleScatter.svg" s (exampleScatter xys)
  fileSvg "other/exampleScatters.svg" s (exampleScatters [xys,xys'])
  fileSvg "other/exampleHist.svg" s (exampleHist xs)
  fileSvg "other/exampleHist2.svg" s (exampleHist2 [xys,xys'])
  fileSvg "other/exampleBar.svg" s exampleBar
  fileSvg "other/exampleArrows.svg" s exampleArrows
  fileSvg "other/exampleArrows2.svg" s exampleArrows2

{-

HUD setup possibilities:

- beside-style
  - the axies and other material can move around according to extraneous details of the chart (such as overhang)
  - individual hud elements are naturally separated

- moveTo-style
  - difficult to create a chart with no data

- canvas-style

-}

-- scratchSvg $ pad 1.1 $ ((beside (r2 (-1,0)) (beside (r2 (0,-1)) unitSquare (pad 1.1 $ showOrigin $ (axis def (range $ [0..100]))))) (axis (axisOrientation .~ Y $ axisPlacement .~ AxisLeft $ def) (range [0..100])))
-- scratchSvg $ pad 1.1 $ (unitSquare <> (pad 1.1 $ showOrigin $ (unitAxis (axisOrientation .~ X $ axisPlacement .~ AxisBottom $ axisInsideStrut .~ -0.1 $ def) (range $ [0..100]))))

-- arrow chart dev
-- chartHud def (centerXY . mconcat . zipWith arrow1 [def]) (rangeArrow def arrowData) (\r x -> fmap (rescaleArrow def r) <$> x) [arrowData]
