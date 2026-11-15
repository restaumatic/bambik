module CircleDrawer.HTML (circleDrawer) where

import Prelude ((#), ($), (>>>), Unit, identity)

import CircleDrawer (adjustDiameter, canvasCircles, circleFill, clickCanvas, closeAdjustment, emptyCanvas, hover, menuPlacement, openMenu, redo, resizeSelected, selectedCircleLine, undo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (actedOf, fold, foldInto, looped, settled, stepped, with)
import PUI.Web (attrAt, attrWith, inCaseAt, onClickedXY, onContextMenuXY, onHoveredXY, providedAt, staticText, text, (:=))
import PUI.Web.HTML (body, button, div, label, p, rangeInput)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawer :: Effect Unit
circleDrawer =
  body $ div $ Semigroupoid.do
    ( div >>> "style" := frameFace $ Semigroupoid.do
        p $ text selectedCircleLine
        label $ Semigroupoid.do
          staticText "Diameter "
          rangeInput @"Diameter" # settled resizeSelected ) # inCaseAt @"adjusting" @"mode"
    ( Semigroupoid.do
      RecordToVariant.do
        button @"Close" # providedAt @"adjusting" @"mode"
        ( div $ RecordToVariant.do
          button @"Undo"
          button @"Redo" )
        ( div >>> "style" := "position: relative; max-width: 500px;" $ RecordToVariant.do
          ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := canvasFace $ RecordToVariant.do
            ( onClickedXY @"Canvas clicked" $
              ( circle >>> "stroke" := "#333" >>> attrAt @"x" "cx" >>> attrAt @"y" "cy" >>> attrAt @"r" "r"
                >>> attrWith "fill" circleFill $ identity ) # actedOf @"key" canvasCircles )
            onHoveredXY @"Canvas hovered" identity
            onContextMenuXY @"Canvas context menu" identity )
          ( div >>> attrWith "style" menuPlacement $ div >>> "style" := menuFace $ button @"Adjust diameter.." ) # providedAt @"menu" @"mode" )
      VariantToRecord.do
        foldInto @"Canvas clicked" clickCanvas
        foldInto @"Canvas hovered" hover
        foldInto @"Canvas context menu" openMenu
        fold @"Adjust diameter.." adjustDiameter
        fold @"Close" closeAdjustment
        fold @"Undo" undo
        fold @"Redo" redo ) # stepped
  # looped # with emptyCanvas

canvasFace :: String
canvasFace = "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; height: auto; touch-action: none;"

menuFace :: String
menuFace = "background: white; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3); border-radius: 4px; padding: 4px;"

frameFace :: String
frameFace = "border: 1px solid #ccc; padding: 8px; margin: 8px 0;"
