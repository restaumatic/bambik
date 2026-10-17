module CircleDrawer.MDC3 (circleDrawer) where

import Prelude ((#), ($), (>>>), Unit, identity)

import CircleDrawer (adjustDiameter, canvasCircles, circleFill, clickCanvas, closeAdjustment, emptyCanvas, hover, menuPlacement, openMenu, redo, resizeSelected, selectedCircleLine, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, foreach, joined, looped, settled, with)
import PUI.Web (attrAt, attrWith, inCaseAt, onClickedXY, onContextMenuXY, onHoveredXY, providedAt, text, (:=))
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, button', card, cardActions, sliderLive, textButton, titleMedium)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawer :: Effect Unit
circleDrawer =
  body $ Semigroupoid.do
    ( card $ Semigroupoid.do
        titleMedium $ text selectedCircleLine
        sliderLive @"Diameter" # settled resizeSelected ) # inCaseAt @"adjusting" @"mode"
    RecordToVariant.do
      textButton @"Close" # providedAt @"adjusting" @"mode"
      ( cardActions $ RecordToVariant.do
        button' @"Undo" { icon: "undo" }
        button' @"Redo" { icon: "redo" } )
      ( div >>> "style" := "position: relative; max-width: 500px;" $ RecordToVariant.do
        ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := canvasFace $ RecordToVariant.do
          ( onClickedXY @"Canvas clicked" $
            ( circle >>> "stroke" := "#333" >>> attrAt @"x" "cx" >>> attrAt @"y" "cy" >>> attrAt @"r" "r"
              >>> attrWith "fill" circleFill $ identity ) # foreach @"key" canvasCircles ) # joined @"Canvas clicked"
          onHoveredXY @"Canvas hovered" identity # joined @"Canvas hovered"
          onContextMenuXY @"Canvas context menu" identity # joined @"Canvas context menu" )
        ( div >>> attrWith "style" menuPlacement $ div >>> "style" := menuFace $ textButton @"Adjust diameter.." ) # providedAt @"menu" @"mode" )
    VariantToRecord.do
      fold @"Canvas clicked" clickCanvas
      fold @"Canvas hovered" hover
      fold @"Canvas context menu" openMenu
      fold @"Adjust diameter.." adjustDiameter
      fold @"Close" closeAdjustment
      fold @"Undo" undo
      fold @"Redo" redo
  # looped # with emptyCanvas

canvasFace :: String
canvasFace = "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; height: auto; touch-action: none;"

menuFace :: String
menuFace = "background: white; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3); border-radius: 4px; padding: 4px;"
