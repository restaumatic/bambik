module CircleDrawerLogic (adjustDiameter, canvasCircles, emptyCanvas, redo, selectOrAddCircle, selectedDiameter, undo) where

import Prelude ((*), (+), (-), (/), (/=), (<$>), (<=), (==), show)

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (sqrt)

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
