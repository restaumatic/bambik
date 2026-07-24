module CircleDrawer (circleDrawer) where

import Prelude ((#), ($), (*), (+), (-), (/), (/=), (<$>), (<<<), (<=), (==), (>>>), Unit, const, show)

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (sqrt)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, asField, foreach, mvu, toCase, updates)
import PUI.HTML (attrWith, body, onClickedXY, provided, (:=))
import PUI.MDC (button, card, cardActions, elevation20, sliderLive)
import PUI.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawer :: Effect Unit
circleDrawer =
  body $
    elevation20 $
      card { caption: "Circle Drawer" } $ ( Semigroupoid.do
          sliderLive { min: minDiameter, max: maxDiameter } # asField @"diameter" # provided # lcmap selectedDiameter # updates adjustDiameter
          ( RecordToVariant.do
              svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
                ( onClickedXY
                    ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                          >>> attrWith "fill" (\c -> if c.on then "#ddd" else "transparent") $ pempty # lcmap (const {})) # foreach @"key" # lcmap (\(m :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }) -> mapWithIndex (\i c -> { key: show i, x: show c.x, y: show c.y, r: show c.r, on: m.selected == Just i }) m.circles)) # toCase @"clicked")
              cardActions $ RecordToVariant.do
                button { label: "Undo", icon: "undo" } # asCase @"undo"
                button { label: "Redo", icon: "redo" } # asCase @"redo") # updates (match { clicked: selectOrAddCircle, undo: const <<< undo, redo: const <<< redo })
      ) # mvu emptyCanvas

selectOrAddCircle :: { x :: Number, y :: Number } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
selectOrAddCircle { x, y } m = case findIndex (\c -> dist c x y <= c.r) m.circles of
  Just i -> m { selected = Just i, diameter = fromMaybe m.diameter ((\c -> 2.0 * c.r) <$> index m.circles i), adjusting = false }
  Nothing -> (pushUndo m) { circles = snoc m.circles { x, y, r: 20.0 }, selected = Nothing }

undo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
undo m = case unsnoc m.undoStack of
  Just { init: rest, last: circles } ->
    m { circles = circles, undoStack = rest, redoStack = snoc m.redoStack m.circles, selected = Nothing, adjusting = false }
  Nothing -> m

redo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
redo m = case unsnoc m.redoStack of
  Just { init: rest, last: circles } ->
    m { circles = circles, redoStack = rest, undoStack = snoc m.undoStack m.circles, selected = Nothing, adjusting = false }
  Nothing -> m

selectedDiameter :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> Maybe { diameter :: Number }
selectedDiameter m = if isJust m.selected then Just { diameter: m.diameter } else Nothing

adjustDiameter :: { diameter :: Number } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
adjustDiameter { diameter } m = applyDiameter (m { diameter = diameter })

applyDiameter :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
applyDiameter m = case m.selected of
  Just i | Just c <- index m.circles i, c.r /= m.diameter / 2.0 ->
    let m' = if m.adjusting then m else (pushUndo m) { adjusting = true }
    in m' { circles = fromMaybe m.circles (updateAt i (c { r = m.diameter / 2.0 }) m.circles) }
  _ -> m

pushUndo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
pushUndo m = m { undoStack = take 100 (snoc m.undoStack m.circles), redoStack = [] }

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
