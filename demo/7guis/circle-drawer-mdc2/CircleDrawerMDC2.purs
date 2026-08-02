module CircleDrawerMDC2 (circleDrawerMDC2) where

import Prelude (identity, (#), ($), (*), (+), (-), (/), (/=), (<$>), (<<<), (<=), (==), (>>>), Unit, const, show)

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (sqrt)
import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, asField, constantly, foreach, informed, mvu, toCase, updated)
import PUI.Web.HTML (attrWith, body, onClickedXY, provided, (:=))
import PUI.Web.MDC2 (button, card, cardActions, elevation20, sliderLive)
import PUI.Web.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawerMDC2 :: Effect Unit
circleDrawerMDC2 =
  body $
    elevation20 $
      card { caption: "Circle Drawer" } $ ( Semigroupoid.do
          sliderLive { label: "" } # asField @"diameter" # provided selectedDiameter # updated (informed adjustDiameter)
          ( svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
              ( onClickedXY
                  ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                        >>> attrWith "fill" (\c -> if c.on then "#ddd" else "transparent") $ pempty # constantly {}) # foreach @"key" canvasCircles) # toCase @"clicked" identity)) # updated (match { clicked: informed selectOrAddCircle })
          ( cardActions $ RecordToVariant.do
              button { label: "Undo", icon: "undo" } # asCase @"undo"
              button { label: "Redo", icon: "redo" } # asCase @"redo") # updated (match { undo: const <<< undo, redo: const <<< redo })
      ) # mvu emptyCanvas

canvasCircles
  :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int }
  -> Array { key :: String, x :: String, y :: String, r :: String, on :: Boolean }
canvasCircles { circles, selected } = mapWithIndex (\i c -> { key: show i, x: show c.x, y: show c.y, r: show c.r, on: selected == Just i }) circles

selectOrAddCircle :: { x :: Number, y :: Number, circles :: Array { x :: Number, y :: Number, r :: Number }, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
selectOrAddCircle { x, y, circles, diameter, adjusting, undoStack, redoStack } = case findIndex (\c -> dist c x y <= c.r) circles of
  Just i -> { circles, selected: Just i, diameter: fromMaybe diameter ((\c -> 2.0 * c.r) <$> index circles i), adjusting: false, undoStack, redoStack }
  Nothing ->
    let stacks = pushUndo { circles, undoStack, redoStack }
    in { circles: snoc circles { x, y, r: 20.0 }, selected: Nothing, diameter, adjusting, undoStack: stacks.undoStack, redoStack: stacks.redoStack }

undo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
undo m@{ undoStack, redoStack, circles } = case unsnoc undoStack of
  Just { init: rest, last: prev } ->
    m { circles = prev, undoStack = rest, redoStack = snoc redoStack circles, selected = Nothing, adjusting = false }
  Nothing -> m

redo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
redo m@{ redoStack, undoStack, circles } = case unsnoc redoStack of
  Just { init: rest, last: next } ->
    m { circles = next, redoStack = rest, undoStack = snoc undoStack circles, selected = Nothing, adjusting = false }
  Nothing -> m

selectedDiameter :: { selected :: Maybe Int, diameter :: Number } -> Maybe { diameter :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
selectedDiameter { selected, diameter } = if isJust selected then Just { diameter: { current: diameter, min: minDiameter, max: maxDiameter, step: Nothing } } else Nothing

adjustDiameter :: { diameter :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
adjustDiameter { diameter, circles, selected, adjusting, undoStack, redoStack } = case selected of
  Just i | Just c <- index circles i, c.r /= diameter.current / 2.0 ->
    let stacks = if adjusting then { undoStack, redoStack } else pushUndo { circles, undoStack, redoStack }
    in { circles: fromMaybe circles (updateAt i (c { r = diameter.current / 2.0 }) circles), selected, diameter: diameter.current, adjusting: true, undoStack: stacks.undoStack, redoStack: stacks.redoStack }
  _ -> { circles, selected, diameter: diameter.current, adjusting, undoStack, redoStack }

pushUndo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
pushUndo { undoStack, circles } = { undoStack: take 100 (snoc undoStack circles), redoStack: [] }

dist :: { x :: Number, y :: Number, r :: Number } -> Number -> Number -> Number
dist c x y = sqrt ((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))

emptyCanvas :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
emptyCanvas =
  { circles: []
  , selected: Nothing
  , diameter: 40.0
  , adjusting: false
  , undoStack: []
  , redoStack: []
  }

minDiameter :: Number
minDiameter = 4.0

maxDiameter :: Number
maxDiameter = 200.0
