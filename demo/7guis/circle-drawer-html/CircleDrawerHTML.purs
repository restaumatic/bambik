module CircleDrawerHTML (circleDrawerHTML) where

import Prelude (identity, (#), ($), (<<<), (>>>), Unit, const)

import CircleDrawerLogic (adjustDiameter, canvasCircles, emptyCanvas, redo, selectOrAddCircle, selectedDiameter, undo)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asField, blank, foreach, informed, mvu, toCase, updated)
import PUI.Web.HTML (attrWith, body, button, div, label, onClickedXY, p, provided, rangeInput, staticText, (:=))
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawerHTML :: Effect Unit
circleDrawerHTML =
  body $ div $ ( Semigroupoid.do
      p ( label $ RecordToRecord.do
          staticText "Diameter "
          rangeInput ) # asField @"value" @"diameter" # provided selectedDiameter # updated (informed adjustDiameter)
      ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
          ( onClickedXY
              ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                    >>> attrWith "fill" (\c -> if c.on then "#ddd" else "transparent") $ blank) # foreach @"key" canvasCircles) # toCase @"clicked" identity)) # updated (match { clicked: informed selectOrAddCircle })
      ( div $ RecordToVariant.do
          button (staticText "Undo") # toCase @"undo" identity
          button (staticText "Redo") # toCase @"redo" identity) # updated (match { undo: const <<< undo, redo: const <<< redo })
  ) # mvu emptyCanvas
