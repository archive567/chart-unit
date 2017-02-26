{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Chart.Types
import Chart.Unit
import Chart.Range
import qualified Control.Foldl as L
import Data.List
import qualified Data.Text as Text
import Diagrams.Backend.SVG (SVG)
import qualified Diagrams.TwoD.Text
import Diagrams.Prelude hiding ((<>), unit)
import FakeData
import Linear hiding (identity, unit)
import Tower.Prelude hiding ((&))
import System.Random.MWC
import Formatting


scratch :: Chart SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600,300)

exampleBox :: Chart' a
exampleBox = box sixby4

exampleAxes :: Chart' a
exampleAxes = qcAxes def sixby4 [toCorners (one::XY)]

exampleGrid :: Chart a
exampleGrid =
    qcScatter
    [ScatterConfig 0.01 (palette1!!4)]
    sixby4
    [V2 <$> [0..10] <*> [0..10]]

exampleLine :: Chart a
exampleLine =
    qcLine defs sixby4 dl
  where
    defs = zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)
    dl = fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)], [(0.0,0.0),(3.0,3.0)]]

exampleLineAxes :: Chart' a
exampleLineAxes =
    qcLine defs sixby4 dl <> qcAxes def sixby4 dl
  where
    defs = zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)
    dl = fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)], [(0.0,0.0),(3.0,3.0)]]

exampleScatter :: [[V2 Double]] -> Chart' a
exampleScatter xys =
    qcScatter defs one xys <> qcAxes def one xys
  where
    defs = zipWith ScatterConfig [0.01,0.02,0.03] (drop 3 (opacs 0.5 palette1))

mkScatterData :: IO [[V2 Double]]
mkScatterData = do
    g <- create
    xys <- rvsCorr g 1000 0.7
    xys1 <- rvsCorr g 400 -0.5
    pure [xys,xys1]

exampleHist :: [[V4 Double]] -> Chart' a
exampleHist xys = qcHist
    [ def
    , rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.4
      $ def
    ]
    (recip sixby4 `times` mconcat (rangeRect <$> xys))
    xys <>
    qcAxes def sixby4 xys

mkHistData :: IO [[V4 Double]]
mkHistData = do
    g <- create
    xys <- rvs g 1000
    xys1 <- rvs g 1000
    pure $ makeHist 30 <$> [xys,(1.5*) <$> xys1]

exampleBar :: Chart' a
exampleBar = qcHist def sixby4 xys <> qcAxes
    ( chartAxes .~
      [ axisTickStyle .~
        TickLabels labels $ def
      ]
      $ def
    ) sixby4 xys
  where
    labels = fmap Text.pack <$> take 10 $ (:[]) <$> ['a'..]
    xys = [zipWith4 V4 [0..10] (replicate 11 0) [0..11] [1,2,3,5,8,0,-2,11]]

-- compound charts
qcomsc :: IO (QCom a)
qcomsc = do
    gen <- create
    xys <- rvsCorr gen 1000 0.7
    xys' <- rvsCorr gen 1000 -0.7
    let r = rangeR2s [xys, xys']
    let d = VD1 [xys,xys']
    let defs = zipWith ScatterConfig [0.01,0.02,0.03] (opacs 0.5 palette1)
    let qcomScatter xy (VD1 x) = qcScatter defs xy x
        qcomScatter xy (VD2 x) = qcScatter defs xy [x]
    pure $ QCom qcomScatter r d

qcoml :: QCom a
qcoml = QCom qcomLine (rangeR2s dl) (VD1 dl)
  where
    defs = zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)
    qcomLine xy (VD1 xs) = qcLine defs xy xs
    qcomLine xy (VD2 x) = qcLine defs xy [x]
    qcomLine xy (VD4 x) = qcLine defs xy x
    dl = fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)], [(0.0,0.0),(3.0,3.0)]]

qcombox :: XY -> QCom a
qcombox xy = QCom (\xy0 _ -> box xy0) xy (VD1 [[]])

qcomax :: XY -> QCom a
qcomax xy = QCom (\xy0 (VD1 xs) -> qcAxes def xy0 xs) xy (VD1 [toCorners xy])

qcomhist :: IO (QCom a)
qcomhist = do
    d0 <- dhist
    let xy0 = mconcat $ rangeRect <$> d0
    pure $ QCom qcomHist xy0 (VD4 d0) 
  where
    dhist = do
        gen <- create
        xs <- rvs gen 1000
        xs2 <- rvs gen 1000 
        let h = makeHist 50 xs
        let h2 = makeHist 50 ((1.4*) <$> xs2)
        pure [h,take 49 h2]
    defs =
        [ def
        , rectBorderColor .~ Color 0 0 0 0
          $ rectColor .~ Color 0.333 0.333 0.333 0.4
          $ def
        ]

    qcomHist xy (VD4 xs) = qcHist defs (recip xy `times` mconcat (rangeRect <$> xs)) xs


-- | render a list of charts using a common scale
qcomRends :: ((Renderable (Diagrams.TwoD.Text.Text Double) a), Renderable (Path V2 Double) a) => XY -> [QCom a] -> QDiagram a V2 Double Any
qcomRends xy qcs = mconcat $ (\x -> view qcomChart x (recip xysum `times` view qcomXY x `times` xy) (view qcomData x)) <$> qcs
    where
      xysum = mconcat $ view qcomXY <$> qcs

sixby4 :: V2 (Extrema Double)
sixby4 = V2 ((1.5*) <$> one) one


-- | charting
qcScatter :: (Renderable (Path V2 Double) b, R2 r, Traversable f) => [ScatterConfig] -> V2 (Extrema Double) -> [f (r Double)] -> QDiagram b V2 Double Any
qcScatter defs xy xyss = mconcat $ zipWith scatter1 defs (scaleXYs xy xyss)

qcLine :: (Renderable (Path V2 Double) b, R2 r, Traversable f) => [LineConfig] -> V2 (Extrema Double) -> [f (r Double)] -> QDiagram b V2 Double Any
qcLine defs xy xyss = mconcat $ zipWith line1 defs (scaleXYs xy xyss)

qcHist :: (Renderable (Path V2 Double) b, Traversable f) => [RectConfig] -> V2 (Extrema Double) -> [f (V4 Double)] -> QDiagram b V2 Double Any
qcHist defs xy xs = (centerXY . mconcat . zipWith rect1 defs $ rescaleRects xy xs)

qcAxes :: ((Renderable (Diagrams.TwoD.Text.Text Double) b), Renderable (Path V2 Double) b, R2 r, Traversable f) => ChartConfig -> V2 (Extrema Double) -> [f (r Double)] -> QDiagram b V2 Double Any
qcAxes (ChartConfig p axes cc) rrange dd = ch
  where
    drange = rangeR2s dd
    ch = L.fold (L.Fold step begin (pad p)) axes
    begin = showOrigin $ box rrange # fcA (color cc) # lw 1
    step x cfg = beside dir x (mo $ axist cfg r tickr)
      where
        r = case view axisOrientation cfg of
              X -> rx
              Y -> ry
        tickr = case view axisOrientation cfg of
              X -> view _x drange
              Y -> view _y drange
        dir = case view axisPlacement cfg of
          AxisBottom -> r2 (0,-1)
          AxisTop -> r2 (0,1)
          AxisLeft -> r2 (-1,0)
          AxisRight -> r2 (1,0)
        mo = case view axisOrientation cfg of
              X -> moveOriginTo (p2 ((-lx)-(ux-lx)/2,0))
              Y -> moveOriginTo (p2 (0,(-ly)-(uy-ly)/2))

    (V2 rx@(Extrema (lx,ux)) ry@(Extrema (ly,uy))) = rrange

-- | scale helpers
scaleXYs :: forall (f :: * -> *) (f1 :: * -> *) (t :: * -> *) a. (R2 t, BoundedField a, Traversable f1, Traversable f, Ord a, Fractional a) => V2 (Extrema a) -> f (f1 (t a)) -> f (f1 (t a))
scaleXYs xy qss = rescaleXY xy xy' <$> qss
  where
    xy' = rangeR2s qss

rescaleXY :: forall (f :: * -> *) (t :: * -> *) a. (R2 t, Field a, Functor f, Fractional a) => V2 (Extrema a) -> V2 (Extrema a) -> f (t a) -> f (t a)
rescaleXY (V2 rx ry) (V2 rx' ry') qs =
    (over _x (rescaleP rx rx') . over _y (rescaleP ry ry')) <$> qs

-- axis rendering
axist ::
    AxisConfig -> Extrema Double -> Extrema Double -> Chart' b
axist cfg r tickr = pad (cfg ^. axisPad) $ strut' $ centerXY $
  atPoints
    (t <$> tickLocations)
    ((`mkLabel` cfg) <$> tickLabels)
  `atop`
  (axisRect (cfg ^. axisHeight) r
   # blob (cfg ^. axisColor))
  where
    strut' x = beside dir x $ strut'' (cfg ^. axisInsideStrut)
    dir = case cfg ^. axisPlacement of
      AxisBottom -> r2 (0,1)
      AxisTop -> r2 (0,-1)
      AxisLeft -> r2 (1,0)
      AxisRight -> r2 (-1,0)
    strut'' = case cfg ^. axisOrientation of
      X -> strutX
      Y -> strutY
    t = case cfg ^. axisOrientation of
      X -> \x -> p2 (x, 0)
      Y -> \y -> p2 (-(cfg ^. axisMarkSize), y)
    ticks = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound n -> mkTicksRound tickr n
      TickExact n -> mkTicksExact tickr n
      TickLabels _ -> []
    tickLocations = case cfg ^. axisTickStyle of
      TickNone -> []
      {- To Do:
        rounded ticks introduce the possibility of marks beyond the existing range.
        if this happens, it should really be fed into the chart rendering as a new,
        revised range.
      -}
      TickRound _ -> rescaleP r tickr <$> ticks
      TickExact _ -> rescaleP r tickr <$> ticks
      TickLabels ls ->
          rescaleP
          (Extrema (0, fromIntegral $ length ls))
          r <$>
          ((\x -> x - 0.5) . fromIntegral <$> [1..length ls])
    tickLabels = case cfg ^. axisTickStyle of
      TickNone -> []
      TickRound _ -> tickFormat <$> ticks
      TickExact _ -> tickFormat <$> ticks
      TickLabels ls -> ls
    tickFormat = sformat (prec 2)
    axisRect h (Extrema (l,u)) = case cfg ^. axisOrientation of
      X -> moveTo (p2 (u,0)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleY h .
          scaleX (u-l) $
          unitSquare
      Y -> moveTo (p2 (0,l)) .
          strokeTrail .
          closeTrail .
          fromVertices .
          scaleX h .
          scaleY (u-l) $
          unitSquare

box :: (Field (N t), V t ~ V2, HasOrigin t, Transformable t, TrailLike t) => V2 (Extrema (N t)) -> t
box (V2 (Extrema (lx,ux)) (Extrema (ly,uy))) = moveOriginTo (p2 ((-lx) - (ux-lx)/2,(-ly) - (uy-ly)/2)) $ scaleX (ux-lx) $ scaleY (uy-ly) unitSquare

-- old examples to replicate
exampleArrows :: Chart' a
exampleArrows = unitArrow def [def] [arrowData]

exampleArrows2 :: Chart' a
exampleArrows2 = chartWith'' def (centerXY . mconcat . zipWith arrow1 [def]) (rangeV4s [arrowData]) (\r x -> fmap (rescaleV4 r) <$> x) [arrowData]

-- clipping
exampleClipping :: (Renderable (Path V2 Double) b, R2 r, Traversable f) => f (r Double) -> QDiagram b V2 Double Any
exampleClipping xys =
    beside (r2 (0,1))
    (beside (r2 (0,1))
     (stroke (pathFromLocTrail $ abox -0.5 0.5 -0.5 0.5) <>
          clipped (pathFromLocTrail $ abox -0.5 0 -0.5 0) (l1 <> s1 xys))
      (l1 <> s1 xys))
    (clipped (pathFromLocTrail $ abox -0.5 0 -0.5 0) (l1 <> s1 xys))

abox :: forall t. (Field (N t), V t ~ V2, HasOrigin t, Transformable t, TrailLike t) => N t -> N t -> N t -> N t -> t
abox lx ux ly uy = moveOriginTo (p2 ((-lx)-(ux-lx)/2,(-ly)-(uy-ly)/2)) $ scaleX (ux-lx) $ scaleY (uy-ly) unitSquare

s1 :: forall (f :: * -> *) (r :: * -> *) b. (Renderable (Path V2 Double) b, R2 r, Traversable f) => f (r Double) -> QDiagram b V2 Double Any
s1 xys = scatter1 (ScatterConfig 0.02 (opac 0.2 $ palette1 !! 1)) (unitizeR2 xys)

l1 :: Chart a
l1 = mconcat $ zipWith line1 (zipWith LineConfig [0.01,0.02,0.03] (opacs 0.5 palette1)) (unitizeR2s dat)
  where
    dat = fmap r2 <$> [ [(0.0,1.0),(1.0,1.0),(2.0,5.0)], [(0.0,0.0),(3.0,3.0)]]

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

  fileSvg "other/exampleAxes.svg" s exampleAxes
  fileSvg "other/exampleGrid.svg" s exampleGrid
  fileSvg "other/exampleLine.svg" s exampleLine
  -- fileSvg "other/exampleManyLines.svg" s (exampleManyLines xss)
  -- fileSvg "other/exampleDots.svg" s (exampleDots xys)
  -- fileSvg "other/exampleDotsScaled.svg" s (exampleDotsScaled xys)
  -- fileSvg "other/exampleDotsScaled2.svg" s (exampleDotsScaled2 xys)
  -- fileSvg "other/exampleScatter.svg" s (exampleScatter xys)
  -- fileSvg "other/exampleScatters.svg" s (exampleScatters [xys,xys'])
  -- fileSvg "other/exampleHist.svg" s (exampleHist xs)
  -- fileSvg "other/exampleHist2.svg" s (exampleHist2 [xys,xys'])
  -- fileSvg "other/exampleBar.svg" s exampleBar
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
