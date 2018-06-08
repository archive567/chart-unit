{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Chart
import Control.Lens hiding (beside)
import Control.Monad.Primitive (PrimState)
import Data.Generics.Labels ()
import Data.List ((!!), head, zipWith3)
import Data.Time
import Data.Time.Calendar.WeekDate
import Formatting
import NumHask.Histogram
import NumHask.Prelude as P
import System.Random.MWC
import System.Random.MWC.Probability
import qualified Data.Text as Text
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Text

-- * example data generation
-- Standard normal random variates in one dimension.
rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standardNormal gen

-- This generates n V2 random variates where the x and y parts are correlated.
rvsCorr :: Gen (PrimState IO) -> Int -> Double -> IO [Pair Double]
rvsCorr gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zipWith Pair s0 s1'

-- * a grid
gridExample :: Chart b
gridExample =
  hud
    (defaultHudOptions & #grids .~
     [ GridOptions
         Vert
         (GridExact GridOuterPos 10)
         (LineOptions 0.001 (ucolor $ black `withOpacity` 1))
     , GridOptions
         Hori
         (GridExact GridOuterPos 10)
         (LineOptions 0.001 (ucolor $ black `withOpacity` 1))
     ] &
     #axes .
     ix 0 %~
     ((#tickStyle .~
       TickPlaced
         (zip (grid OuterPos (Range -0.5 0.5) 10) (replicate 11 "abcdef"))) .
      (#label . #text . #alignH .~ AlignLeft) .
      (#gap .~ 0) .
      (#label . #text . #rotation .~ -45)) &
     #axes .
     ix 1 %~
     (#label . #text . #alignH .~ AlignLeft))
    sixbyfour
    one

timeData :: Int -> IO [Day]
timeData n = do
  now <- getCurrentTime
  let (UTCTime today _) = now
  let toWeekDay x =
        let (_, _, d) = toWeekDate x
        in d
  let isWeekend x = toWeekDay x `elem` [6, 7]
  pure $ filter (not . isWeekend) $ take n $ (`addDays` today) <$> [0 ..]

-- * dealing with time
timeExample :: [Day] -> Chart b
timeExample dates =
  hud (#axes .~ [adef, defYAxis] $ defaultHudOptions) sixbyfour r <>
  glyphChart
    [#color .~ ucolor (red `withOpacity` 1) $ #borderSize .~ 0 $ #size .~ 0.01 $
     defaultGlyphOptions]
    sixbyfour
    r
    [xs'] <>
  lglyphChart
    [defaultLabelOptions]
    [ #shape .~ Square $ #color .~ ucolor (blue `withOpacity` 1) $ #borderSize .~ 0 $
      #size .~ 0.04 $ defaultGlyphOptions
    ]
    sixbyfour
    r
    [ zip
        (Text.pack . formatTime defaultTimeLocale "%a, %d %b" .
         (\x -> dates !! x) .
         fst <$>
         ts)
        ((\x -> xs' !! x) . fst <$> ts)
    ]
  where
    today = Data.List.head dates
    g = 6
    xs = fromIntegral . (`diffDays` today) <$> dates
    xs' = lineOneD xs
    r = range [xs']
    (Ranges rx _) = r
    (ts, _) =
      placedTimeLabelDiscontinuous
        PosInnerOnly
        Nothing
        g
        ((`UTCTime` 0) <$> dates)
    ts' = (\(x, y) -> (fromIntegral x, y)) <$> ts
    (Ranges aspx _) = sixbyfour
    adef = adjustAxis defaultAutoOptions aspx rx $
      #tickStyle .~ TickPlaced ts' $ defXAxis

-- * scatter chart
mkScatterData :: IO [[Pair Double]]
mkScatterData = do
  g <- create
  xys <- rvsCorr g 1000 0.7
  xys1 <- rvsCorr g 1000 -0.5
  pure
    [ (\(Pair x y) -> Pair (x ^^ 2 + 3 * x - 1) (y + 1)) <$> xys
    , (\(Pair x y) -> Pair (x ^^ 2 + 3 * x + 1) y) <$> xys1
    ]

scatterHistExample :: [[Pair Double]] -> Chart b
scatterHistExample xys =
  D.beside
    (D.r2 (1, 0))
    (D.beside (D.r2 (0, -1)) (sc1 <> hud1) (D.reflectY histx))
    (D.reflectY $ D.rotateBy (3 / 4) histy)
  where
    sopts =
      zipWith3
        (\x y z -> GlyphOptions x y (UColor 0 0 0 0) 0 z)
        [0.01, 0.02, 0.03]
        ((\x -> ucolor (withOpacity (d3Colors1 x) 0.3)) <$> [6, 8])
        [Circle, Triangle, Square]
    mainAspect = Rect -0.5 0.5 -0.5 0.5
    minorAspect = Rect -0.5 0.5 -0.1 0.1
    sc1 = glyphChart_ sopts mainAspect xys
    histx = rectChart_ defHist minorAspect hx
    histy = rectChart_ defHist minorAspect hy
    hud1 =
      hud
        (#axes .~ [#place .~ PlaceTop $ #label . #orientation .~ Pair 0 1 $
                   defXAxis] $
         defaultHudOptions)
        mainAspect
        (range xys)
    defHist =
      (\x -> #borderSize .~ 0 $ #color .~ (ucolor $ d3Colors1 x `withOpacity` 0.5)
        $ defaultRectOptions) <$>
      [6, 8]
    makeHist n = makeRects IgnoreOvers . regular n
    hx = makeHist 50 . fmap (view D._x) <$> xys
    hy = makeHist 50 . fmap (view D._y) <$> xys

-- * haskell survey example
data SurveyQ = SurveyQ
  { surveyTitle :: Text
  , surveyData :: [(Text, Int)]
  , surveyBarGap :: Double
  , surveyNumberDrop :: Double
  , surveyBarColor :: UColor Double
  , surveyNumberColor :: UColor Double
  , surveyAutoOptions :: AutoOptions
  } deriving (Show, Generic)

q7 :: SurveyQ
q7 =
  SurveyQ
    "How frequently do you use Haskell?"
    [ ("Daily", 469)
    , ("Weekly", 452)
    , ("Monthly", 215)
    , ("Yearly", 36)
    , ("Rarely", 51)
    ]
    0.2
    0.07
    (UColor 0.341 0.224 0.388 1)
    (UColor 1 1 0.33 1)
    (#allowDiagonal .~ False $ #maxXRatio .~ 0.16 $ defaultAutoOptions)
 
q24 :: SurveyQ
q24 =
  SurveyQ
    "Which editors do you use for Haskell?"
    [ ("Vim", 534)
    , ("Emacs", 501)
    , ("VS Code", 202)
    , ("Atom", 169)
    , ("Sublime", 92)
    , ("Notepad++", 28)
    , ("VS", 3)
    , ("Other", 134)
    ]
    0.2
    (-0.03)
    (UColor 0.341 0.224 0.388 1)
    (UColor 0.33 0.33 0.33 1)
    defaultAutoOptions

surveyChart :: SurveyQ -> Chart b
surveyChart (SurveyQ t d bgap ngap bc tc ao) =
  surveyText tc ngap (snd <$> d) <> surveyBars bc bgap (snd <$> d) <>
  surveyHud ao t d

surveyBars :: UColor Double -> Double -> [Int] -> Chart b
surveyBars rc gap d =
  rectChart
    [#borderSize .~ 0 $ #color .~ rc $ defaultRectOptions]
    sixbyfour
    (barRange [fromIntegral <$> d])
    [rectBars gap $ fromIntegral <$> d]

surveyHud :: AutoOptions -> Text -> [(Text, Int)] -> Chart b
surveyHud ao t d =
  hud
    (#outerPad .~ 1 $ #titles .~ [(defaultTitleOptions, t)] $ #axes .~
     [ #tickStyle .~ TickRound 4 $ defYAxis
     , adjustAxis ao aspx rx $ #gap .~ 0 $ #tickStyle .~ TickLabels (fst <$> d) $
       defXAxis
     ] $
     defaultHudOptions)
    sixbyfour
    r
  where
    r = barRange [fromIntegral . snd <$> d]
    (Ranges rx _) = r
    (Ranges aspx _) = sixbyfour

surveyText :: UColor Double -> Double -> [Int] -> Chart b
surveyText tc gap ys =
  textChart
    (repeat (#color .~ tc $ defaultTextOptions))
    sixbyfour
    (barRange [fromIntegral <$> ys])
    [ zipWith
        (\x y ->
           ( show y
           , Pair
               (x + 0.5)
               ((if y > 0
                   then -ngap
                   else ngap) +
                fromIntegral y)))
        [0 ..]
        ys
    ]
  where
    ngap = gap * fromIntegral (maximum ys) :: Double

-- * one-dim chart example
makeQuantiles :: Double -> IO [Double]
makeQuantiles n = do
  g <- create
  xs <- rvs g 100000
  pure (regularQuantiles n xs)

skinnyExample :: [Double] -> [Double] -> Chart b
skinnyExample qs qs' = hud' <> ticks' <> labels'
  where
    r = Ranges (space qs) (Range 0 0.2)
    hud' =
      hud
      (HudOptions
        1.1
        [#label . #text . #size .~ 0.25 $ defXAxis]
        []
        []
        []
        clear)
      skinny
      r
    labels' =
      textChart
      [#alignH .~ AlignLeft $ #rotation .~ 45 $ #size .~ 0.2 $ defaultTextOptions]
      skinny
      r
      [ zipWith
        (\x y -> (x, Pair y 0.05))
        ["min", "3rd Q", "median", "1st Q", "max"]
        qs'
      ]
    ticks' = glyphChart [defaultGlyphOptions] skinny r [(`Pair` 0.02) <$> qs]

-- * comparing histograms
makeHistDiffExample :: IO ([Rect Double], [Rect Double])
makeHistDiffExample = do
  g <- create
  xys <- rvs g 1000
  xys1 <- rvs g 1000
  let cuts = grid OuterPos (Range -5.0 5.0) 50
  pure
    ( makeRects IgnoreOvers (fill cuts xys)
    , makeRects IgnoreOvers (fill cuts ((1.5 *) <$> xys1)))

histDiffExample :: ([Rect Double], [Rect Double]) -> Chart b
histDiffExample (h1, h2) =
  let deltah =
        zipWith (\(Rect x y z w) (Rect _ _ _ w') -> Rect x y z (w - w')) h1 h2
      mainAspect = Rect -0.75 0.75 -0.5 0.5
      botAspect = Rect -0.75 0.75 -0.2 0.2
      (Ranges rx ry) = fold $ fold [h1, h2]
      (Ranges _ deltary) = fold (abs <$> deltah)
  in D.pad 1.1 $
     D.beside
       (D.r2 (0, -1))
       (rectChart
          [ #borderColor .~ utrans $
            #color .~ UColor 0.365 0.647 0.855 0.2 $
            defaultRectOptions
          , #borderColor .~ utrans $
            #color .~ UColor 0.88 0.53 0.23 0.8 $
            defaultRectOptions
          ]
          mainAspect
          (Ranges rx ry)
          [h1, h2])
       (rectChart
          [ #borderColor .~ utrans $
            #color .~ UColor 0.88 0.53 0.23 0.8 $
            defaultRectOptions
          ]
          botAspect
          (Ranges rx deltary)
          [deltah]) <>
        hud defaultHudOptions botAspect (Ranges rx deltary)

-- * clipping
clip :: Rect Double -> Chart b -> Chart b
clip (Rect xl xu yl yu) c =
  D.clipped
    (D.pathFromLocTrail $ D.moveTo (D.p2 (xl, yl)) $ D.scaleY (yu - yl) $
     D.scaleX (xu - xl) $
     D.moveOriginTo (D.p2 (-0.5, -0.5)) D.unitSquare)
    c

grp :: Int -> [a] -> [[a]]
grp n =
  unfoldr
    (\x ->
       let y = splitAt n x
       in if null (fst y)
            then Nothing
            else Just y)

-- | chop a chart extent into a double list of Rects
chop ::
  ( D.Renderable (D.Path D.V2 Double) b
  , D.Renderable (Diagrams.TwoD.Text.Text Double) b) =>
  Pair Int -> Chart b -> [[Rect Double]]
chop p@(Pair _ n) ch = grp n $ gridSpace (Rect xl xu yl yu) p
  where
    (xl, xu) = fromMaybe (-0.5, 0.5) (D.extentX ch)
    (yl, yu) = fromMaybe (-0.5, 0.5) (D.extentY ch)

clippingExample ::
     RectOptions
  -> Double
  -> Int
  -> Chart b
  -> Chart b
clippingExample rcfg p n ch =
  stack (Pair 0 1) (D.pad p . D.centerXY) $
  hori (\a -> D.pad p $ bound rcfg 1 $ D.centerXY $ clip a ch) <$>
  chop (Pair n n) ch

-- * schoolbook
schoolbookHud :: Chart b
schoolbookHud =
  hud
    (#axes .~ [] $
     #titles .~ [(defaultTitleOptions, "y = x² - 3")] $
     #grids .~
     [ GridOptions
         Vert
         (GridExact GridOuterPos 10)
         (LineOptions 0.005 schoolBlue)
     , GridOptions
         Hori
         (GridExact GridOuterPos 10)
         (LineOptions 0.005 schoolBlue)
     , GridOptions
         Vert
         (GridExact GridOuterPos 50)
         (LineOptions 0.002 schoolBlue)
     , GridOptions
         Hori
         (GridExact GridOuterPos 50)
         (LineOptions 0.002 schoolBlue)
     ] $
     defaultHudOptions)
    asquare
    (Rect -5 5 -5 5)
  where
    schoolBlue = UColor 0.19 0.74 0.89 0.7

parabola :: Rect Double -> (Double -> Double) -> Int -> Range Double -> Chart b
parabola r f grain xscope =
  lineChart
    [#size .~ 0.01 $ #color .~ UColor 0.6 0.6 0.6 1 $ defaultLineOptions]
    asquare
    r
    [dataXY f xscope grain]

ceptLines ::
    Rect Double
  -> Rect Double
  -> (Double -> Double)
  -> Double
  -> Chart b
ceptLines asp r@(Ranges rx ry) f x =
  mconcat $ lines (#color .~ UColor 0.2 0.2 0.2 1 $ #size .~ 0.005 $
                   defaultLineOptions) .
  fmap (Chart.project r asp) <$>
  [[Pair (lower rx) (f x), Pair x (f x)], [Pair x (lower ry), Pair x (f x)]]

cepts ::
    Rect Double
  -> Rect Double
  -> (Double -> Double)
  -> Double
  -> Chart b
cepts a r@(Ranges rx ry) f x =
  textChart
    [ defaultTextOptions
    , #alignH .~ AlignCenter $ #rotation .~ 0 $ defaultTextOptions]
    a
    r
    [ [("x = " <> sformat (fixed 1) x, Pair x (lower ry - 1))]
    , [("y = " <> sformat (fixed 1) (f x), Pair (lower rx - 1.5) (f x))]
    ]

schoolbookExample :: Double -> Chart b
schoolbookExample x =
  bound (#color .~ UColor 0 0 0 0.1 $ defaultRectOptions) 1.05 $ schoolbookHud <>
  parabola r f grain xscope <>
  ceptLines asquare r f x <>
  glyphChart [#color .~ ucolor (red `withOpacity` 0.5) $
              defaultGlyphOptions] asquare r [[Pair x (f x)]] <>
  cepts asquare r f x
  where
    f x = x * x - 3
    r = Rect -5 5 -5 5
    xscope = Range -3 3
    grain = 50

exampleDir :: FilePath
exampleDir = "./chart-unit-examples/other/"

exampleSvg :: FilePath -> Pair Double -> Chart SVG -> IO ()
exampleSvg t s c =
  fileSvg
  (exampleDir <> t)
  (#size .~ s $ defaultSvgOptions)
  c

sStandard :: Pair Double
sStandard = Pair 300 200

main :: IO ()
main = do
  exampleSvg "gridExample.svg" sStandard gridExample
  ts <- timeData 200
  exampleSvg "timeExample.svg" sStandard (timeExample ts)
  xys <- mkScatterData
  exampleSvg "scatterHistExample.svg"sStandard (scatterHistExample xys)
  putStrLn ("skinnyExample" :: Text)
  qs <- makeQuantiles 20
  qs' <- makeQuantiles 4
  exampleSvg "skinnyExample.svg" (Pair 600 150) $
    skinnyExample qs qs'
  hs <- makeHistDiffExample
  exampleSvg "histDiffExample.svg" (Pair 600 600) (histDiffExample hs)
  exampleSvg "clippingExample.svg" (Pair 600 600) $
    clippingExample
      (#color .~ UColor 0.3 0.3 0.3 0.1 $ defaultRectOptions)
      1.1
      5
      (schoolbookExample -1)
  exampleSvg "schoolbookExample.svg" (Pair 400 400)
    (schoolbookExample -1)
  exampleSvg "q7Example.svg" sStandard (surveyChart q7)
  exampleSvg "q24Example.svg" sStandard (surveyChart q24)
