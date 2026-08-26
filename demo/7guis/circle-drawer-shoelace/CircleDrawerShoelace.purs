module CircleDrawerShoelace (circleDrawerShoelace) where

import Prelude (identity, (#), ($), (<<<), (>>>), Unit, const)

import CircleDrawerLogic (canvasCircles, emptyCanvas, redo, resizeSelected, selectOrAddCircle, selection, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (blank, foreach, mvu, settled, toCase, updated)
import PUI.Web.HTML (attrWith, body, div, onClickedXY, inCase, (:=))
import PUI.Web.Shoelace (button, card, sliderLive)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Category as Category

circleDrawerShoelace :: Effect Unit
circleDrawerShoelace =
  body $
    card $ ( Category.do
        sliderLive @"Diameter" {} # inCase @"chosen" selection # settled resizeSelected
        ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
            ( onClickedXY
                ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                      >>> attrWith "fill" (\c -> if c.on then "#ddd" else "transparent") $ blank) # foreach @"key" canvasCircles) # toCase @"picked" identity)) # updated (match { picked: selectOrAddCircle })
        ( div $ RecordToVariant.do
            button @"Undo" {}
            button @"Redo" {}) # updated (match { "Undo": const <<< undo, "Redo": const <<< redo })
    ) # mvu emptyCanvas
