module CircleDrawerHTML (circleDrawerHTML) where

import Prelude (identity, (#), ($), (<<<), (>>>), Unit, const)

import CircleDrawerLogic (canvasCircles, emptyCanvas, redo, resizeSelected, selectOrAddCircle, selection, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (blank, foreach, mvu, settled, toCase, updated)
import PUI.Web.HTML (shown, attrWith, body, button, div, label, onClickedXY, p, inCase, rangeInput, staticText, (:=))
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Category as Category

circleDrawerHTML :: Effect Unit
circleDrawerHTML =
  body $ div $ ( Category.do
      p ( label $ Category.do
          (staticText "Diameter ") # shown
          rangeInput @"Diameter" ) # inCase @"chosen" selection # settled resizeSelected
      ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
          ( onClickedXY
              ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                    >>> attrWith "fill" circleFill $ blank ) # foreach @"key" canvasCircles ) # toCase @"picked" identity )) # updated (match { picked: selectOrAddCircle })
      ( div $ RecordToVariant.do
          button (staticText "Undo") # toCase @"undo" identity
          button (staticText "Redo") # toCase @"redo" identity ) # updated (match { undo: const <<< undo, redo: const <<< redo })
  ) # mvu emptyCanvas
circleFill :: { key :: String, x :: String, y :: String, r :: String, status :: [ selected :: {}, unselected :: {} ] } -> String
circleFill { status } = match { selected: \_ -> "#ddd", unselected: \_ -> "transparent" } status
