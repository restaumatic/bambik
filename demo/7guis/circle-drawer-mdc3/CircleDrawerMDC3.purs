module CircleDrawerMDC3 (circleDrawerMDC3) where

import Prelude (identity, (#), ($), (<<<), (>>>), Unit, const)

import CircleDrawerLogic (canvasCircles, emptyCanvas, redo, resizeSelected, selectOrAddCircle, selection, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (blank, foreach, mvu, settled, toCase, updated)
import PUI.Web.HTML (attrWith, body, onClickedXY, inCase, (:=))
import PUI.Web.MDC3 (button, card, cardActions, elevation5, sliderLive)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Category as Category

circleDrawerMDC3 :: Effect Unit
circleDrawerMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          sliderLive @"Diameter" {} # inCase @"chosen" selection # settled resizeSelected
          ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
              ( onClickedXY
                  ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                        >>> attrWith "fill" (\c -> if c.on then "#ddd" else "transparent") $ blank ) # foreach @"key" canvasCircles ) # toCase @"picked" identity )) # updated (match { picked: selectOrAddCircle })
          ( cardActions $ RecordToVariant.do
              button @"Undo" { icon: "undo" }
              button @"Redo" { icon: "redo" } ) # updated (match { "Undo": const <<< undo, "Redo": const <<< redo })
      ) # mvu emptyCanvas
