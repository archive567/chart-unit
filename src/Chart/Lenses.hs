{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Lenses for all the options.  Note the trailing underscore naming convention, rather than the more obnoxious _prefixed conventions.
module Chart.Lenses
    -- * tea anyone?
  ( arrowMinLength_
  , arrowMaxLength_
  , arrowMinHeadLength_
  , arrowMaxHeadLength_
  , arrowMinStaffWidth_
  , arrowMaxStaffWidth_
  , arrowColor_
  , arrowHeadStyle_
  , glyphSize_
  , glyphColor_
  , glyphBorderColor_
  , glyphBorderSize_
  -- , glyphShape_
  , hudPad_
  , hudAxes_
  , hudTitles_
  , hudLegends_
  , hudRange_
  , hudAspect_
  , hudCanvas_
  , axisPad_
  , axisOrientation_
  , axisPlace_
  , axisRect_
  , axisRectHeight_
  , axisMark_
  , axisMarkStart_
  , axisGap_
  , axisLabel_
  , axisTickStyle_
  , titleText_
  , titleAlign_
  , titlePlace_
  , titleGap_
  , legendChartType_
  , legendInnerPad_
  , legendInnerSep_
  , legendGap_
  , legendRowPad_
  , legendPlace_
  , legendAlign_
  , legendSep_
  , legendRect_
  , legendText_
  , lineSize_
  , lineColor_
  , rectBorderSize_
  , rectBorderColor_
  , rectColor_
  , textSize_
  , textAlignH_
  , textColor_
  , textFillRule_
  , textRotation_
  , labelText_
  , labelOrientation_
  , labelGap_
  ) where

import Chart.Arrow
import Chart.Glyph
import Chart.Hud
import Chart.Line
import Chart.Rect
import Chart.Text
import Control.Lens

makeLensesFor
  [ ("arrowMinLength", "arrowMinLength_")
  , ("arrowMaxLength", "arrowMaxLength_")
  , ("arrowMinHeadLength", "arrowMinHeadLength_")
  , ("arrowMaxHeadLength", "arrowMaxHeadLength_")
  , ("arrowMinStaffWidth", "arrowMinStaffWidth_")
  , ("arrowMaxStaffWidth", "arrowMaxStaffWidth_")
  , ("arrowColor", "arrowColor_")
  , ("arrowHeadStyle", "arrowHeadStyle_")
  ]
  ''ArrowOptions

makeLensesFor
  [ ("glyphSize", "glyphSize_")
  , ("glyphColor", "glyphColor_")
  , ("glyphBorderColor", "glyphBorderColor_")
  , ("glyphBorderSize", "glyphBorderSize_")
      -- GHC doesn't yet support impredicative polymorphism
      -- In the type signature:
      --   glyphShape_ :: forall b_agHOV b_agNLd.
      --                  Lens (GlyphOptions b_agHOV) (GlyphOptions b_agNLd) (ghc-prim-0.5.0.0:GHC.Types.Double -> Chart.Core.Chart b_agHOV) (ghc-prim-0.5.0.0:GHC.Types.Double -> Chart.Core.Chart b_agNLd)
    -- , ("glyphShape", "glyphShape_")
  ]
  ''GlyphOptions

makeLensesFor
  [ ("hudPad", "hudPad_")
  , ("hudAxes", "hudAxes_")
  , ("hudTitles", "hudTitles_")
  , ("hudLegends", "hudLegends_")
  , ("hudRange", "hudRange_")
  , ("hudAspect", "hudAspect_")
  , ("hudCanvas", "hudCanvas_")
  ]
  ''HudOptions

makeLensesFor
  [ ("axisPad", "axisPad_")
  , ("axisOrientation", "axisOrientation_")
  , ("axisPlace", "axisPlace_")
  , ("axisRect", "axisRect_")
  , ("axisRectHeight", "axisRectHeight_")
  , ("axisMark", "axisMark_")
  , ("axisMarkStart", "axisMarkStart_")
  , ("axisGap", "axisGap_")
  , ("axisLabel", "axisLabel_")
  , ("axisTickStyle", "axisTickStyle_")
  ]
  ''AxisOptions

makeLensesFor
  [ ("titleText", "titleText_")
  , ("titleAlign", "titleAlign_")
  , ("titlePlace", "titlePlace_")
  , ("titleGap", "titleGap_")
  ]
  ''TitleOptions

makeLensesFor
  [ ("legendChartType", "legendChartType_")
  , ("legendInnerPad", "legendInnerPad_")
  , ("legendInnerSep", "legendInnerSep_")
  , ("legendGap", "legendGap_")
  , ("legendRowPad", "legendRowPad_")
  , ("legendPlace", "legendPlace_")
  , ("legendAlign", "legendAlign_")
  , ("legendSep", "legendSep_")
  , ("legendRect", "legendRect_")
  , ("legendText", "legendText_")
  ]
  ''LegendOptions

makeLensesFor
  [("lineSize", "lineSize_"), ("lineColor", "lineColor_")]
  ''LineOptions

makeLensesFor
  [ ("rectBorderSize", "rectBorderSize_")
  , ("rectBorderColor", "rectBorderColor_")
  , ("rectColor", "rectColor_")
  ]
  ''RectOptions

makeLensesFor
  [ ("textSize", "textSize_")
  , ("textAlignH", "textAlignH_")
  , ("textColor", "textColor_")
  , ("textFillRule", "textFillRule_")
  , ("textRotation", "textRotation_")
  ]
  ''TextOptions

makeLensesFor
  [ ("labelText", "labelText_")
  , ("labelOrientation", "labelOrientation_")
  , ("labelGap", "labelGap_")
  ]
  ''LabelOptions
