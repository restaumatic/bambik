module CircleDrawerShoelace (circleDrawerShoelace) where

import Prelude (identity, (#), ($), (<<<), (>>>), Unit, const)

import CircleDrawerLogic (adjustDiameter, canvasCircles, emptyCanvas, redo, selectOrAddCircle, selectedDiameter, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (blank, foreach, informed, mvu, toCase, updated)
import PUI.Web.HTML (attrWith, body, div, onClickedXY, provided, (:=))
import PUI.Web.Shoelace (button, card, sliderLive)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawerShoelace :: Effect Unit
circleDrawerShoelace =
  body $
    card { caption: "Circle Drawer" } $ ( Semigroupoid.do
        sliderLive @"diameter" {} # provided selectedDiameter # updated (informed adjustDiameter)
        ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
            ( onClickedXY
                ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                      >>> attrWith "fill" (\c -> if c.on then "#ddd" else "transparent") $ blank) # foreach @"key" canvasCircles) # toCase @"picked" identity)) # updated (match { picked: informed selectOrAddCircle })
        ( div $ RecordToVariant.do
            button @"Undo" {}
            button @"Redo" {}) # updated (match { "Undo": const <<< undo, "Redo": const <<< redo })
    ) # mvu emptyCanvas
