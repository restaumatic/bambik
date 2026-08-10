module CircleDrawerShoelace (circleDrawerShoelace) where

import Prelude (identity, (#), ($), (<<<), (>>>), Unit, const)

import CircleDrawerLogic (adjustDiameter, canvasCircles, emptyCanvas, redo, selectOrAddCircle, selectedDiameter, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, asField, constantly, foreach, informed, mvu, pempty, toCase, updated)
import PUI.Web.HTML (attrWith, body, div, onClickedXY, provided, (:=))
import PUI.Web.Shoelace (button, card, sliderLive)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawerShoelace :: Effect Unit
circleDrawerShoelace =
  body $
    card { caption: "Circle Drawer" } $ ( Semigroupoid.do
        sliderLive { label: "Diameter" } # asField @"value" @"diameter" # provided selectedDiameter # updated (informed adjustDiameter)
        ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
            ( onClickedXY
                ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                      >>> attrWith "fill" (\c -> if c.on then "#ddd" else "transparent") $ pempty # constantly {}) # foreach @"key" canvasCircles) # toCase @"clicked" identity)) # updated (match { clicked: informed selectOrAddCircle })
        ( div $ RecordToVariant.do
            button { label: "Undo" } # asCase @"clicked" @"undo"
            button { label: "Redo" } # asCase @"clicked" @"redo") # updated (match { undo: const <<< undo, redo: const <<< redo })
    ) # mvu emptyCanvas
