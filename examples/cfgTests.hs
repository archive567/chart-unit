{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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


-- import Graphics.SVGFonts

scratch :: Diagram SVG -> IO ()
scratch = fileSvg "other/scratchpad.svg" (600,400)

main :: IO ()
main = scratch $ vert identity [pad 1.1 $ showOrigin $ text_  (textAlignH .~ 0.5 $ textColor .~ withOpacity blue 1 $ textSize .~ 0.03 $ def) "a slightly longer text piece", pad 1.1 $ showOrigin $ text_  (textColor .~ withOpacity red 0.2 $ textSize .~ 0.2 $ def) "-100,000", pad 1.1 $ text_ def "standard horizontal range" <> rect_ (box (color 0 0 1 0.1)) (Ranges one (0.1 *. one))]

stdh :: Diagram B
stdh = pad 1.1 $ text_ def "standard horizontal range" <> rect_ (box (color 0 0 1 0.1)) (Ranges one (0.1 *. one))

stdht :: Double -> Double -> Text -> QDiagram B V2 Double Any
stdht aspectx s t = rect_ (box (color 0.2 0.2 0.2 0.1)) (Ranges (aspectx *. one) (0.1*.one)) <> text_ (textSize .~ s $ def) t

testXAspect :: AxisConfig -> Double -> [Double] -> QDiagram B V2 Double Any
testXAspect cfg xr xas =
    vert identity $ zipWith (\x y -> hori centerXY [x,y]) [stdht 0.5 0.06 $ "aspect=" <> show x | x <- xas] [axes $ justxaxis xa 0.1 xr cfg | xa <- xas]

justxaxis :: Double -> Double -> Double -> AxisConfig -> ChartConfig
justxaxis xaspect yaspect xrange x =
    ChartConfig
    1
    [x]
    []
    []
    (Just (Ranges (xrange *. one) one))
    (Aspect (Ranges (xaspect *. one) (yaspect *. one)))
    clear

justyaxis :: Double -> Double -> Double -> AxisConfig -> ChartConfig
justyaxis xaspect yaspect yrange x =
    ChartConfig
    1
    [x]
    []
    []
    (Just (Ranges one (yrange *. one)))
    (Aspect (Ranges (xaspect *. one) (yaspect *. one)))
    clear

compareCharts :: Double -> Double -> [Text] -> [AxisConfig] -> QDiagram B V2 Double Any
compareCharts xr xa labels cfgs =
    vert identity $ zipWith (\x y -> hori centerXY [x,y]) (stdht 0.5 0.06 <$> labels)  ((\x -> axes $ justxaxis xa 0.1 xr x) <$> cfgs)

testRectHeight :: [Double] -> QDiagram B V2 Double Any
testRectHeight xs = compareCharts 100 1 ((\x -> "rect height = " <> show x) <$> xs) ((\x -> axisRectHeight .~ x $ defXAxis) <$> xs)

testMarkHeight :: [Double] -> QDiagram B V2 Double Any
testMarkHeight xs = compareCharts 100 1 ((\x -> "mark height = " <> show x) <$> xs) ((\x -> axisMark . glyphSize .~ x $ defXAxis) <$> xs)

testMarkDarkness :: [Double] -> QDiagram B V2 Double Any
testMarkDarkness xs = compareCharts 100 1 ((\x -> "mark darkness = " <> show x) <$> xs) ((\x -> axisMark . glyphBorderColor .~ (withOpacity black x) $ defXAxis) <$> xs)

testMarkDarknessLength :: [Double] -> [Double] -> QDiagram B V2 Double Any
testMarkDarknessLength xs ys = compareCharts 100 1 ["dark:"<>show x<>" thick"<>show y | x <- xs, y <- ys] [axisMark . glyphBorderColor .~ (withOpacity black x) $ axisMark . glyphSize .~ y $ defXAxis | x <- xs, y <- ys]

testMarkDarknessThickness :: [Double] -> [Double] -> QDiagram B V2 Double Any
testMarkDarknessThickness xs ys = compareCharts 100 1 ["dark:"<>show x<>" thick"<>show y | x <- xs, y <- ys] [axisMark . glyphBorderColor .~ (withOpacity black x) $ axisMark . glyphBorderSize .~ y $ defXAxis | x <- xs, y <- ys]

testLabelGap :: [Double] -> QDiagram B V2 Double Any
testLabelGap xs = compareCharts 100 1 ((\x ->"gap = " <> show x) <$> xs) ((\x -> axisLabel . labelStrut .~ x $ defXAxis) <$> xs)

testLabelGapFontSize :: [Double] -> [Double] -> QDiagram B V2 Double Any
testLabelGapFontSize xs ys = compareCharts 100 1 ["gap:"<>show x<>" size"<>show y | x <- xs, y <- ys] [axisLabel . labelText . textSize .~ y $ axisLabel . labelStrut .~ x $ defXAxis | x <- xs, y <- ys]
  
axisAuto ::
    Double ->
    AxisConfig ->
    AxisConfig
axisAuto aspect cfg = newcfg
  where
    newcfg = cfg
    -- relSize = aspectd / (cfg ^. axisLabelText ^. textSize * fromIntegral maxl)

--  testTitles 1.5 -1 "a left-aligned title" (textPad .~ 1.2 $ textAlignH .~ 0.5 $ textSize .~ 0.2 $ def)

testTitles aspx cfg = vert (pad 1.2 . bound (box (withOpacity red 0.2)) 1)
    [ axes (chartAspect .~ Aspect (Ranges (aspx *. one) one) $ chartPad .~ 1 $ def)
    , (textp (textAlignH .~ 0.5 $ textSize .~ 0.2 $ cfg) "Left-Aligned" (Pair (-0.5 *aspx) 0))
    , (textp (textAlignH .~ -0.5 $ textSize .~ 0.2 $ cfg) "Right-Aligned" (Pair (0.5 *aspx) 0))
    , (textp (textAlignH .~ 0 $ textSize .~ 0.2 $ cfg) "Center-Aligned" (Pair (0 *aspx) 0))
    ]
 
 
