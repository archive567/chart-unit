{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Diagrams.Prelude as D
import qualified Diagrams.TwoD.Text
import Formatting
import NumHask.Histogram
import NumHask.Prelude as P
import System.Random.MWC
import System.Random.MWC.Probability

import qualified Data.Text as Text

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
    (def & #grids .~
     [ GridOptions
         Vert
         (GridExact GridOuterPos 10)
         (LineOptions 0.001 (black `withOpacity` 1))
     , GridOptions
         Hori
         (GridExact GridOuterPos 10)
         (LineOptions 0.001 (black `withOpacity` 1))
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
  hud (#axes .~ [adef, defYAxis] $ def) sixbyfour r <>
  glyphChart
    [#color .~ red `withOpacity` 1 $ #borderSize .~ 0 $ #size .~ 0.01 $ def]
    sixbyfour
    r
    [xs'] <>
  lglyphChart
    [def]
    [ #shape .~ Square $ #color .~ blue `withOpacity` 1 $ #borderSize .~ 0 $
      #size .~ 0.04 $ def
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
    adef = adjustAxis def aspx rx $ #tickStyle .~ TickPlaced ts' $ defXAxis

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
        (\x y z -> GlyphOptions x y (ucolor 0 0 0 0) 0 z)
        [0.01, 0.02, 0.03]
        ((\x -> withOpacity (d3Colors1 x) 0.3) <$> [6, 8])
        [Circle, Triangle, Square]
    mainAspect = Rect -0.5 0.5 -0.5 0.5
    minorAspect = Rect -0.5 0.5 -0.1 0.1
    sc1 = glyphChart_ sopts mainAspect xys
    histx = rectChart_ defHist minorAspect hx
    histy = rectChart_ defHist minorAspect hy
    hud1 =
      hud
        (#axes .~ [#place .~ PlaceTop $ #label . #orientation .~ Pair 0 1 $ def] $
         def)
        mainAspect
        (range xys)
    defHist =
      (\x -> #borderSize .~ 0 $ #color .~ d3Colors1 x `withOpacity` 0.5 $ def) <$>
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
  , surveyBarColor :: AlphaColour Double
  , surveyNumberColor :: AlphaColour Double
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
    (ucolor 0.341 0.224 0.388 1)
    (ucolor 1 1 0.33 1)
    (#allowDiagonal .~ False $ #maxXRatio .~ 0.16 $ def)
 
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
    (ucolor 0.341 0.224 0.388 1)
    (ucolor 0.33 0.33 0.33 1)
    def

surveyChart :: SurveyQ -> Chart b
surveyChart (SurveyQ t d bgap ngap bc tc ao) =
  surveyText tc ngap (snd <$> d) <> surveyBars bc bgap (snd <$> d) <>
  surveyHud ao t d

surveyBars :: AlphaColour Double -> Double -> [Int] -> Chart b
surveyBars rc gap d =
  rectChart
    [#borderSize .~ 0 $ #color .~ rc $ def]
    sixbyfour
    (barRange [fromIntegral <$> d])
    [rectBars gap $ fromIntegral <$> d]

surveyHud :: AutoOptions -> Text -> [(Text, Int)] -> Chart b
surveyHud ao t d =
  hud
    (#outerPad .~ 1 $ #titles .~ [(def, t)] $ #axes .~
     [ #tickStyle .~ TickRound 4 $ defYAxis
     , adjustAxis ao aspx rx $ #gap .~ 0 $ #tickStyle .~ TickLabels (fst <$> d) $
       defXAxis
     ] $
     def)
    sixbyfour
    r
  where
    r = barRange [fromIntegral . snd <$> d]
    (Ranges rx _) = r
    (Ranges aspx _) = sixbyfour

surveyText :: AlphaColour Double -> Double -> [Int] -> Chart b
surveyText tc gap ys =
  textChart
    (repeat (#color .~ tc $ def))
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
        [#label . #text . #size .~ 0.25 $ def]
        []
        []
        []
        clear)
      skinny
      r
    labels' =
      textChart
      [#alignH .~ AlignLeft $ #rotation .~ 45 $ #size .~ 0.2 $ def]
      skinny
      r
      [ zipWith
        (\x y -> (x, Pair y 0.05))
        ["min", "3rd Q", "median", "1st Q", "max"]
        qs'
      ]
    ticks' = glyphChart [def] skinny r [(`Pair` 0.02) <$> qs]

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
          [ #borderColor .~ ucolor 0 0 0 0 $
            #color .~ ucolor 0.365 0.647 0.855 0.2 $
            def
          , #borderColor .~ ucolor 0 0 0 0 $
            #color .~ ucolor 0.88 0.53 0.23 0.8 $
            def
          ]
          mainAspect
          (Ranges rx ry)
          [h1, h2])
       (rectChart
          [ #borderColor .~ ucolor 0 0 0 0 $
            #color .~ ucolor 0.88 0.53 0.23 0.8 $
            def
          ]
          botAspect
          (Ranges rx deltary)
          [deltah]) <>
        hud def botAspect (Ranges rx deltary)

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
     #titles .~ [(def, "y = x² - 3")] $
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
     def)
    asquare
    (Rect -5 5 -5 5)
  where
    schoolBlue = ucolor 0.19 0.74 0.89 0.7

parabola :: Rect Double -> (Double -> Double) -> Int -> Range Double -> Chart b
parabola r f grain xscope =
  lineChart
    [#size .~ 0.01 $ #color .~ ucolor 0.6 0.6 0.6 1 $ def]
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
  mconcat $ lines (#color .~ ucolor 0.2 0.2 0.2 1 $ #size .~ 0.005 $ def) .
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
    [def, #alignH .~ AlignCenter $ #rotation .~ 0 $ def]
    a
    r
    [ [("x = " <> sformat (fixed 1) x, Pair x (lower ry - 1))]
    , [("y = " <> sformat (fixed 1) (f x), Pair (lower rx - 1.5) (f x))]
    ]

schoolbookExample :: Double -> Chart b
schoolbookExample x =
  bound (#color .~ ucolor 1 1 1 0.1 $ def) 1.05 $ schoolbookHud <>
  parabola r f grain xscope <>
  ceptLines asquare r f x <>
  glyphChart [#color .~ red `withOpacity` 0.5 $ def] asquare r [[Pair x (f x)]] <>
  cepts asquare r f x
  where
    f x = x * x - 3
    r = Rect -5 5 -5 5
    xscope = Range -3 3
    grain = 50

main :: IO ()
main = do
  putStrLn ("gridExample" :: Text)
  fileSvg "other/gridExample.svg" def gridExample
  ts <- timeData 200
  putStrLn ("timeExample" :: Text)
  fileSvg "other/timeExample.svg" def $ timeExample ts
  xys <- mkScatterData
  putStrLn ("scatterHistExample" :: Text)
  fileSvg "other/scatterHistExample.svg" def (scatterHistExample xys)
  putStrLn ("skinnyExample" :: Text)
  qs <- makeQuantiles 20
  qs' <- makeQuantiles 4
  fileSvg "other/skinnyExample.svg" (#size .~ Pair 600 150 $ def) $
    skinnyExample qs qs'
  putStrLn ("histDiffExample" :: Text)
  hs <- makeHistDiffExample
  fileSvg "other/histDiffExample.svg" (#size .~ Pair 600 600 $ def) $
    histDiffExample hs
  putStrLn ("clippingExample" :: Text)
  fileSvg "other/clippingExample.svg" (#size .~ Pair 600 600 $ def) $
    clippingExample
      (#color .~ ucolor 0.3 0.3 0.3 0.1 $ def)
      1.1
      5
      (schoolbookExample -1)
  putStrLn ("schoolbookExample" :: Text)
  fileSvg "other/schoolbookExample.svg" (#size .~ Pair 400 400 $ def)
    (schoolbookExample -1)
  putStrLn ("haskell survey examples" :: Text)
  fileSvg "other/q7Example.svg" def $ surveyChart q7
  fileSvg "other/q24Example.svg" def $ surveyChart q24
