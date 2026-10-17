module CircleDrawer.MDC2 (circleDrawer) where

import Prelude ((#), ($), (>>>), Unit, identity)

import CircleDrawer (canvasCircles, emptyCanvas, redo, resizeSelected, selectOrAddCircle, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Variant (match)
import Effect (Effect)
import PUI (fold, foreach, joined, looped, settled, with)
import PUI.Web (attrAt, attrWith, inCaseAt, onClickedXY, (:=))
import PUI.Web.MDC2 (body, button', cardActions, sliderLive)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawer :: Effect Unit
circleDrawer =
  body $ Semigroupoid.do
    sliderLive @"Diameter" # inCaseAt @"chosen" @"selected" # settled resizeSelected
    RecordToVariant.do
      ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
        ( onClickedXY @"Canvas clicked" $
          ( circle >>> "stroke" := "#333" >>> attrAt @"x" "cx" >>> attrAt @"y" "cy" >>> attrAt @"r" "r"
            >>> attrWith "fill" circleFill $ identity ) # foreach @"key" canvasCircles ) ) # joined @"Canvas clicked"
      ( cardActions $ RecordToVariant.do
        button' @"Undo" { icon: "undo" }
        button' @"Redo" { icon: "redo" } )
    VariantToRecord.do
      fold @"Canvas clicked" selectOrAddCircle
      fold @"Undo" undo
      fold @"Redo" redo
  # looped # with emptyCanvas

circleFill
  :: { key :: String, r :: String, status :: [ selected :: {}, unselected :: {} ], x :: String, y :: String }
  -> String
circleFill { status } = match { selected: \_ -> "#ddd", unselected: \_ -> "transparent" } status
