{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Chart.Types
import Chart.Unit
import Chart.Range
import qualified Control.Foldl as L
import Data.List
import qualified Data.Text as Text
import Diagrams.Backend.SVG (SVG)
-- import qualified Diagrams.TwoD.Text
import Diagrams.Prelude hiding ((<>), unit)
import FakeData
import Linear hiding (identity, unit)
import Protolude hiding ((&))
import System.Random.MWC
import Data.Semigroup
import qualified Data.List.NonEmpty as NonEmpty

exampleCanvasLines :: [[V2 Double]] -> Canvas
exampleCanvasLines xys = Canvas ch r
  where
    ch = mconcat $ zipWith line1 (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) xys
    r = rangeR2s xys

exampleCanvasLinesUnitized :: [[V2 Double]] -> Canvas
exampleCanvasLinesUnitized xys = Canvas ch r
  where
    ch = mconcat $ zipWith line1 (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) (unitizeR2s xys)
    r = rangeR2s xys

exampleCanvasScatterUnitized :: IO Canvas
exampleCanvasScatterUnitized = do
    gen <- create
    xys <- rvsCorr gen 1000 0.7
    xys' <- rvsCorr gen 1000 -0.7
    let ch = mconcat $ zipWith scatter1 (zipWith ScatterConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) (unitizeR2s [xys,xys'])
    let r = rangeR2s [xys, xys']
    pure $ Canvas ch r


exsc :: IO (QC [[V2 Double]])
exsc = do
    gen <- create
    xys <- rvsCorr gen 1000 0.7
    xys' <- rvsCorr gen 1000 -0.7
    let r = rangeR2s [xys, xys']
    pure $ QC qcScatter r [xys,xys']

qcScatter xy xyss = mconcat $ zipWith scatter1 (zipWith ScatterConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) (scaleXYs xy xyss)

qcLine xy xyss = mconcat $ zipWith line1 (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) (scaleXYs xy xyss)


scaleXYs xy qss = rescaleXY xy xy' <$> qss
  where
    xy' = rangeR2s qss

rescaleXY (V2 rx ry) (V2 rx' ry') qs =
    (over _x (rescaleP rx rx') . over _y (rescaleP ry ry')) <$> qs


mkQCS qcs = QCS qcs (sconcat $ (view qxy) <$> qcs)

rends (QCS qcs xys) = sconcat $ (\x -> (view qchart x) xys (view qdata x)) <$> qcs


v1 t = view qxys (mkQCS (t NonEmpty.:| [exline]))

-- t <- exsc
-- scratch $ (rend $ t) <> (box v1 <> (rend $ exline))

exline :: QC [[V2 Double]]
exline = QC qcLine (rangeR2s dl) dl

rend :: QC a -> Chart SVG
rend (QC qc r d) = qc r d

rendunit :: QC a -> Chart SVG
rendunit (QC qc _ d) = qc unitRangeV2 d





exampleCanvasBlank :: Canvas
exampleCanvasBlank = Canvas mempty (V2 (Range (0,1)) (Range (0,1)))

exampleCanvasBox :: V2 (Range Double) -> Canvas
exampleCanvasBox r = Canvas (box r) r

exampleCanvasBoxUnitized :: V2 (Range Double) -> Canvas
exampleCanvasBoxUnitized r = Canvas (unitSquare) r

box :: (V t ~ V2, HasOrigin t, Transformable t, TrailLike t) => V2 (Range (N t)) -> t
box (V2 (Range (lx,ux)) (Range (ly,uy))) = moveOriginTo (p2 (-lx-(ux-lx)/2,-ly-(uy-ly)/2)) $ scaleX (ux-lx) $ scaleY (uy-ly) unitSquare

unitbox :: (V t ~ V2, HasOrigin t, TrailLike t) => V2 (Range (N t)) -> t
unitbox (V2 (Range (lx,ux)) (Range (ly,uy))) = moveOriginTo (p2 (x',y')) $ unitSquare
  where
    x' = if lx == ux then 0 else -lx/(ux-lx) - 0.5
    y' = if ly == uy then 0 else -ly/(uy-ly) - 0.5

xxx :: QDiagram SVG V2 Double Any
xxx = (view qdd $ exampleCanvasHud def (view qdr ch)) <> ((box $ view qdr ch) <> (view qdd ch))
  where
    ch = exampleCanvasLines $ fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)], [(0.0,0.0),(3.0,3.0)]]

dl :: [[V2 Double]]
dl = fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)], [(0.0,0.0),(3.0,3.0)]]

exampleCanvasHud ::
      ChartConfig
    -> V2 (Range Double)
    -> Canvas
exampleCanvasHud (ChartConfig p axes cc) range' = Canvas ch range'
  where
    ch = L.fold (L.Fold step begin (pad p)) axes
    begin = showOrigin $ box range' # fcA (color cc) # lw 1
    step x cfg = beside dir x (showOrigin $ mo $ axis' cfg r)
      where
        r = case view axisOrientation cfg of
              X -> rx
              Y -> ry
        dir = case view axisPlacement cfg of
          AxisBottom -> r2 (0,-1)
          AxisTop -> r2 (0,1)
          AxisLeft -> r2 (-1,0)
          AxisRight -> r2 (1,0)
        mo = case view axisOrientation cfg of
              X -> moveOriginTo (p2 (-lx-(ux-lx)/2,0))
              Y -> moveOriginTo (p2 (0,-ly-(uy-ly)/2))

    (V2 rx@(Range (lx,ux)) ry@(Range (ly,uy))) = range'

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
scratch = fileSvg "other/scratchpad.svg" (600,300)

-- exampleClipping :: Chart' a
exampleClipping :: (Renderable (Path V2 Double) b, R2 r, Traversable f) => f (r Double) -> QDiagram b V2 Double Any
exampleClipping xys =
    beside (r2 (0,1))
    (beside (r2 (0,1))
     (stroke (pathFromLocTrail $ abox -0.5 0.5 -0.5 0.5) <>
          clipped (pathFromLocTrail $ abox -0.5 0 -0.5 0) (l1 <> s1 xys))
      (l1 <> s1 xys))
    (clipped (pathFromLocTrail $ abox -0.5 0 -0.5 0) (l1 <> s1 xys))

abox :: forall t. (V t ~ V2, HasOrigin t, Transformable t, TrailLike t) => N t -> N t -> N t -> N t -> t
abox lx ux ly uy = moveOriginTo (p2 (-lx-(ux-lx)/2,-ly-(uy-ly)/2)) $ scaleX (ux-lx) $ scaleY (uy-ly) unitSquare

s1 :: forall (f :: * -> *) (r :: * -> *) b. (Renderable (Path V2 Double) b, R2 r, Traversable f) => f (r Double) -> QDiagram b V2 Double Any
s1 xys = scatter1 (ScatterConfig 0.02 (opac 0.2 $ palette1 !! 1)) (unitizeR2 xys)

l1 :: Chart a
l1 = mconcat $ zipWith line1 (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) (unitizeR2s d)
  where
    d = fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)], [(0.0,0.0),(3.0,3.0)]]

-- drawing a box
-- for a line chart after clipping
-- scratch $ pad 1.1 $ (unitSquare # moveOriginTo (p2 (-0.5,-0.5)) # scaleX 5 # scaleY 2) # moveOriginTo (p2 (2,0)) <> decon -2 3 0 2
-- drawing a box using a pathFromTrail
-- for a line chart
-- scratch $ (stroke $ moveOriginTo (p2 (0.5,0.5)) $ pathFromTrail $ closeTrail (fromVertices (p2 <$> [(-0.5,-0.5),(-0.5,0.5),(0.5,0.5),(0.5,-0.5)]))) <> (mconcat $ zipWith line1 (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) (unitizeR2s l1))
-- for a scatter chart
-- scratch $ (stroke $ moveOriginTo (p2 (0.5,0.5)) $ pathFromTrail $ closeTrail (fromVertices (p2 <$> [(-0.5,-0.5),(-0.5,0.5),(0.5,0.5),(0.5,-0.5)]))) <> (scatter1 (ScatterConfig 0.02 (opac 0.2 $ palette1 !! 1)) (unitizeR2 xys))
-- both
-- scratch $ (stroke $ moveOriginTo (p2 (0.5,0.5)) $ pathFromTrail $ closeTrail (fromVertices (p2 <$> [(-0.5,-0.5),(-0.5,0.5),(0.5,0.5),(0.5,-0.5)]))) <> (scatter1 (ScatterConfig 0.02 (opac 0.2 $ palette1 !! 1)) (unitizeR2 xys)) <> (mconcat $ zipWith line1 (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) (unitizeR2s l1))

main :: IO ()
main = do
  gen <- create
  xs <- rvs gen 1000
  xss <- replicateM 5 (rvs gen 30)
  xys <- rvsCorr gen 1000 0.7
  xys' <- rvsCorr gen 1000 -0.7
  let s = (400,400)

  scratch (exampleClipping xys)

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
makePng :: IO ()
makePng = do
  gen <- create
  xs <- rvs gen 1000
  xys <- rvsCorr gen 1000 0.7
  xys' <- rvsCorr gen 1000 -0.7
  let s = (400,400)
  -- png versions
  {-
  filePng "other/exampleEmptyChart.png" s exampleEmptyChart
  filePng "other/exampleAxes.png" s exampleAxes
  filePng "other/exampleGrid.png" s exampleGrid
  filePng "other/exampleLine.png" s exampleLine
  filePng "other/exampleManyLines.png" s (exampleManyLines xss)
  filePng "other/exampleDots.png" s (exampleDots xys)
  filePng "other/exampleDotsScaled.png" s (exampleDotsScaled xys)
  filePng "other/exampleDotsScaled2.png" s (exampleDotsScaled2 xys)
  filePng "other/exampleScatter.png" s (exampleScatter xys)
  -}
  filePng "other/exampleScatters.png" s (exampleScatters [xys,xys'])
  filePng "other/exampleHist.png" s (exampleHist xs)
  filePng "other/exampleHist2.png" s (exampleHist2 [xys,xys'])
  filePng "other/exampleBar.png" s exampleBar
{-
  filePng "other/exampleArrows.png" s exampleArrows
  filePng "other/exampleArrows2.png" s exampleArrows2
-}
-}

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
