{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Chart where

import Protolude
import Control.Monad.Primitive (unsafeInlineIO)
import Control.Category (id)
import Data.List (transpose)
import System.IO (FilePath)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Core.Envelope
import Formatting
import GHC.Base (String)
import qualified Control.Foldl as L
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed  (Vector,(!))
import qualified Data.Vector.Unboxed as VU

import Chart.Types


-- helpers
rgba (r,g,b,a) = withOpacity (sRGB (r/255) (g/255) (b/255)) a

hex s = sRGB r b g
  where
    (RGB r g b) = toSRGB $ sRGB24read s

range1D = L.fold (L.Fold step initial extract)
  where
    step Nothing x = Just (x,x)
    step (Just (min,max)) x =
      Just (min' x min, max' x max)
    max' x1 x2 = if x1 >= x2 then x1 else x2
    min' x1 x2 = if x1 <= x2 then x1 else x2
    initial = Nothing
    extract = fromMaybe (-0.5,0.5)

unit' xs =
  let (minX,maxX) = range1D xs in
  (\x -> (x-minX)/(maxX-minX) - 0.5) <$> xs

toFile :: FilePath -> (Double, Double) -> QDiagram SVG V2 Double Any -> IO ()
toFile name size = renderSVG name (mkSizeSpec (Just <$> r2 size))

-- scatter chart
dots size c xys =
  atPoints (p2 <$> zip (unit' (fst <$> xys))  (unit' (snd <$> xys)))
    (repeat $ circle size #
     fcA c #
     lcA (withOpacity black 0) #
     lw none
    )

unitScatter :: AxisConfig -> ScatterConfig -> [(Double,Double)] -> QDiagram SVG V2 Double Any
unitScatter acfg cfg xys =
  beside (r2 (-1,0))
  (beside (r2 (0,-1))
   (dots (view scatterSize cfg) (view scatterColor cfg) xys)
   (unitAxis acfg (fst <$> xys))
  )
  (unitAxis (axisOrientation .~ Y $ acfg) (snd <$> xys))
  # pad (view scatterPad cfg)

-- axis rendering
unitAxis :: AxisConfig -> [Double] -> QDiagram SVG V2 Double Any
unitAxis cfg xs =
  atPoints
    (p2 . trans <$> tickLocations)
    ((\x -> mkLabel x cfg) <$> tickLabels)
  `atop`
  (axisRect (view axisHeight cfg) (range1D $ unit' xs) # axisStyle (view axisColor cfg))
  where
    trans = case view axisOrientation cfg of
      X -> \x -> (x,0)
      Y -> \y -> (-(view axisVruleSize cfg), y)
    tickLocations = case view axisTickStyle cfg of
      TickNone -> []
      TickNumber n -> unit' $ tickList (range1D xs) n
      TickLabels ls -> unit' $ fromIntegral <$> [1..length ls]
    tickLabels = case view axisTickStyle cfg of
      TickNone -> []
      TickNumber n -> formatToString (prec 2) <$> tickList (range1D xs) n
      TickLabels ls -> ls
    axisRect height (min, max) = case view axisOrientation cfg of
      X -> moveTo (p2 (max,0)) . strokeTrail . closeTrail . fromVertices . scaleX (max-min) . scaleY height $ unitSquare
      Y -> moveTo (p2 (0,min)) . strokeTrail . closeTrail . fromVertices . scaleY (max-min) . scaleX height $ unitSquare
    axisStyle c = fcA c # lcA (withOpacity black 0) # lw none

tickList :: (Double,Double) -> Int -> [Double]
tickList (min,max) n = (first +) . (step *) . fromIntegral <$> [0..n']
  where
    span' = max - min
    step' = 10 ^^ floor (logBase 10 (span'/fromIntegral n))
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10 * step'
      | err <= 0.35 = 5 * step'
      | err <= 0.75 = 2 * step'
      | otherwise = step'
    first = step * fromIntegral (floor (min/step) + 1)
    last = fromIntegral (floor (max/step)) * step
    n' = round ((last - first)/step)

mkLabel :: String -> AxisConfig -> QDiagram SVG V2 Double Any
mkLabel label cfg =
  beside dir (beside dir (fcA (view axisVruleColor cfg) $
                          rule (view axisVruleSize cfg)) gap)
  ( Diagrams.Prelude.alignedText 0.5 1 label
  # scale (view axisTextSize cfg)
  # fcA (view axisTextColor cfg))
  where
    dir = case view axisOrientation cfg of
      X -> r2 (0,-1)
      Y -> r2 (-1,0)
    rule = case view axisOrientation cfg of
      X -> vrule
      Y -> hrule
    gap = case view axisOrientation cfg of
      X -> strutY (view axisStrutSize cfg)
      Y -> strutX (view axisStrutSize cfg)

-- bar
unitBar :: BarConfig -> [Double] -> [String] -> QDiagram SVG V2 Double Any
unitBar cfg ys labels =
  beside (r2 (0,-1))
  (bars cfg ys # centerXY # scaleX (1/fromIntegral (length ys)) # scaleY (1/foldl' max 0 ys))
  (unitAxis (axisTickStyle .~ TickLabels labels $ def) [])
  # pad (view barPad cfg)

bars :: BarConfig -> [Double] -> QDiagram SVG V2 Double Any
bars cfg ys =
  cat' (r2 (1,0)) (with Diagrams.Prelude.& sep .~ view barSep cfg)
  ((\y ->
    unitSquare
    # moveOriginTo (p2 (-0.5,-0.5))
    # scaleY y) <$> ys)

barStyle cfg =
     fcA (view barColor cfg) #
     lcA (withOpacity black 0) #
     lw none
