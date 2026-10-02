module CircleDrawerMDC2 (circleDrawerMDC2) where

import Prelude ((#), ($), (<<<), (>>>), Unit, const)

import CircleDrawerViewModel (canvasCircles, emptyCanvas, redo, resizeSelected, selectOrAddCircle, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (blank, foreach, mvu, settled, state, updated)
import PUI.Web (attrWith, inCase, onClickedXY, (:=))
import PUI.Web.MDC2 (body, button, cardActions, sliderLive)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawerMDC2 :: Effect Unit
circleDrawerMDC2 =
  body $
    ( Semigroupoid.do
      state @"selected" @[ chosen :: { index :: Int }, none :: {} ]
      state @"circles" @(Array { x :: Number, y :: Number, r :: Number })
      state @"drag" @[ adjusting :: {}, settled :: {} ]
      state @"undoStack" @(Array (Array { x :: Number, y :: Number, r :: Number }))
      state @"redoStack" @(Array (Array { x :: Number, y :: Number, r :: Number }))
      sliderLive @"Diameter" {} # inCase @"chosen" _.selected # settled resizeSelected
      ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
        ( onClickedXY @"picked"
          ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
            >>> attrWith "fill" circleFill $ blank ) # foreach @"key" @String canvasCircles ) ) ) # updated (match { picked: selectOrAddCircle })
      ( cardActions $ RecordToVariant.do
        button @"Undo" { icon: "undo" }
        button @"Redo" { icon: "redo" } ) # updated (match { "Undo": const <<< undo, "Redo": const <<< redo })
    ) # mvu emptyCanvas

circleFill :: forall r1. { key :: String, x :: String, y :: String, r :: String, status :: [ selected :: {}, unselected :: {} ] | r1 } -> String
circleFill { status } = match { selected: \_ -> "#ddd", unselected: \_ -> "transparent" } status
