module CircleDrawerHTML (circleDrawerHTML) where

import Prelude ((#), ($), (<<<), (>>>), Unit, const)

import CircleDrawerViewModel (canvasCircles, emptyCanvas, redo, resizeSelected, selectOrAddCircle, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (blank, foreach, mvu, settled, updated)
import PUI.Web (attrWith, inCase, onClickedXY, shown, staticText, (:=))
import PUI.Web.HTML (body, button, div, label, p, rangeInput)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawerHTML :: Effect Unit
circleDrawerHTML =
  body $ div $ ( Semigroupoid.do
    p ( label $ Semigroupoid.do
      (staticText @"Diameter ") # shown
      rangeInput @"Diameter" ) # inCase @"chosen" _.selected # settled resizeSelected
    ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
      ( onClickedXY @"picked"
        ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
          >>> attrWith "fill" circleFill $ blank ) # foreach @"key" @( key :: String, x :: String, y :: String, r :: String, status :: [ selected :: {}, unselected :: {} ] ) canvasCircles ) ) ) # updated (match { picked: selectOrAddCircle })
    ( div $ RecordToVariant.do
      button @"Undo" {}
      button @"Redo" {} ) # updated (match { "Undo": const <<< undo, "Redo": const <<< redo })
  ) # mvu
    @( circles :: Array { x :: Number, y :: Number, r :: Number }
     , selected :: [ chosen :: { index :: Int }, none :: {} ]
     , "Diameter" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
     , drag :: [ adjusting :: {}, settled :: {} ]
     , undoStack :: Array (Array { x :: Number, y :: Number, r :: Number })
     , redoStack :: Array (Array { x :: Number, y :: Number, r :: Number })
     )
    emptyCanvas

circleFill :: forall r1. { key :: String, x :: String, y :: String, r :: String, status :: [ selected :: {}, unselected :: {} ] | r1 } -> String
circleFill { status } = match { selected: \_ -> "#ddd", unselected: \_ -> "transparent" } status
