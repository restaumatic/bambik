module CircleDrawer (circleDrawer) where

import Prelude ((#), ($), (*), (+), (-), (/), (/=), (<$>), (<<<), (<=), (==), (>>>), Unit, const, show)

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (sqrt)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, asField, mvu, updates)
import PUI.HTML (Markup(..), attr, body, div, provided, view)
import PUI.MDC (button, card, elevation20, sliderLive)
import PUI.Web (onClickXY)
import QualifiedDo.Semigroupoid as Semigroupoid

type Circle = { x :: Number, y :: Number, r :: Number }

type Canvas =
  { circles :: Array Circle
  , selected :: Maybe Int
  , diameter :: Number
  , adjusting :: Boolean
  , undoStack :: Array (Array Circle)
  , redoStack :: Array (Array Circle)
  }

circleDrawer :: Effect Unit
circleDrawer =
  body $
    elevation20 $
      card { caption: "Circle Drawer" } $ ( Semigroupoid.do
          sliderLive { min: minDiameter, max: maxDiameter } # asField @"diameter"
            # provided # lcmap selectedDiameter # updates adjustDiameter
          ( RecordToVariant.do
              view
                """<svg viewBox="0 0 500 300" style="border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;"></svg>"""
                renderCanvas
                (\node emit -> onClickXY node \x y -> emit (.clicked { x, y } :: [ clicked :: { x :: Number, y :: Number } ]))
              div >>> attr "style" "display: flex; gap: 8px;" $ RecordToVariant.do
                button { label: "Undo", icon: "undo" } # asCase @"undo"
                button { label: "Redo", icon: "redo" } # asCase @"redo"
          ) # updates (match { clicked: selectOrAddCircle, undo: const <<< undo, redo: const <<< redo })
      ) # mvu emptyCanvas

selectOrAddCircle :: { x :: Number, y :: Number } -> Canvas -> Canvas
selectOrAddCircle { x, y } m = case findIndex (\c -> dist c x y <= c.r) m.circles of
  Just i -> m { selected = Just i, diameter = fromMaybe m.diameter ((\c -> 2.0 * c.r) <$> index m.circles i), adjusting = false }
  Nothing -> (pushUndo m) { circles = snoc m.circles { x, y, r: 20.0 }, selected = Nothing }

undo :: Canvas -> Canvas
undo m = case unsnoc m.undoStack of
  Just { init: rest, last: circles } ->
    m { circles = circles, undoStack = rest, redoStack = snoc m.redoStack m.circles, selected = Nothing, adjusting = false }
  Nothing -> m

redo :: Canvas -> Canvas
redo m = case unsnoc m.redoStack of
  Just { init: rest, last: circles } ->
    m { circles = circles, redoStack = rest, undoStack = snoc m.undoStack m.circles, selected = Nothing, adjusting = false }
  Nothing -> m

selectedDiameter :: Canvas -> Maybe { diameter :: Number }
selectedDiameter m = if isJust m.selected then Just { diameter: m.diameter } else Nothing

adjustDiameter :: { diameter :: Number } -> Canvas -> Canvas
adjustDiameter { diameter } m = applyDiameter (m { diameter = diameter })

applyDiameter :: Canvas -> Canvas
applyDiameter m = case m.selected of
  Just i | Just c <- index m.circles i, c.r /= m.diameter / 2.0 ->
    let m' = if m.adjusting then m else (pushUndo m) { adjusting = true }
    in m' { circles = fromMaybe m.circles (updateAt i (c { r = m.diameter / 2.0 }) m.circles) }
  _ -> m

pushUndo :: Canvas -> Canvas
pushUndo m = m { undoStack = take 100 (snoc m.undoStack m.circles), redoStack = [] }

dist :: Circle -> Number -> Number -> Number
dist c x y = sqrt ((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))

renderCanvas :: Canvas -> Array Markup
renderCanvas m = mapWithIndex circle m.circles
  where
  circle i c = Element "circle"
    [ Tuple "cx" (show c.x)
    , Tuple "cy" (show c.y)
    , Tuple "r" (show c.r)
    , Tuple "stroke" "#333"
    , Tuple "fill" (if m.selected == Just i then "#ddd" else "transparent")
    ]
    []

emptyCanvas :: Canvas
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
