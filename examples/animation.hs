{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main ( main ) where

import Chart
import NumHask.Prelude

import FakeData

import Data.List ((!!))

import Diagrams.Backend.Rasterific.CmdLine

import Diagrams.Backend.CmdLine

import Codec.Picture.Gif


histDefs :: [RectConfig]
histDefs =
    [ def
    , rectBorderColor .~ Color 0 0 0 0
      $ rectColor .~ Color 0.333 0.333 0.333 0.4
      $ def
    ]

chartRange' :: [[Rect Double]] -> Rect Double
chartRange' = fold . fold

exampleHist :: Rect Double -> Aspect -> [[Rect Double]] -> Chart' a
exampleHist cr asp rs =
    histChartWithRange cr histDefs asp rs <>
    axes
    ( chartRange .~ Just cr
    $ chartAspect .~ asp
    $ def)

main :: IO ()
main = do
  xs <- mkHistData
  let yss = inits (xs!!0)
  let cr = chartRange' yss
  let us :: [Diagram B ]
      us = exampleHist cr widescreen . pure <$> yss

  displayHeader "other/anim.gif" $ zip us (repeat (10 :: Int))

displayHeader :: FilePath -> [(Diagram B, GifDelay)] -> IO ()
displayHeader fn =
  mainRender ( DiagramOpts (Just 900) (Just 700) fn
             , GifOpts {_dither = False, _noLooping = False, _loopRepeat = Just 2}
             )
