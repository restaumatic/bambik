module CircleDrawerViewModel (canvasCircles, canvasClickedLine, emptyCanvas, redo, redoneLine, resizeSelected, selectOrAddCircle, undo, undoneLine) where

import Prelude ((*), (+), (-), (/), (/=), (<$>), (<=), (<>), (==), show)

import Data.Array (findIndex, index, length, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.Variant (match)

emptyCanvas :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) }
emptyCanvas =
  { circles: []
  , selected: .none {}
  , "Diameter": { current: 40.0, min: minDiameter, max: maxDiameter, step: .continuous {} }
  , drag: .settled {}
  , undoStack: []
  , redoStack: []
  }

minDiameter :: Number
minDiameter = 4.0

maxDiameter :: Number
maxDiameter = 200.0

freshCircleRadius :: Number
freshCircleRadius = 20.0

canvasCircles :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) } -> Array { key :: String, r :: String, status :: [ selected :: {}, unselected :: {} ], x :: String, y :: String }
canvasCircles { circles, selected } = mapWithIndex (\i c -> { key: show i, x: show c.x, y: show c.y, r: show c.r, status: statusOf i }) circles
  where
  statusOf i = match { chosen: \s -> if s.index == i then .selected {} else .unselected {}, none: \_ -> .unselected {} } selected

selectOrAddCircle :: { event :: { x :: Number, y :: Number }, model :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) } } -> { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) }
selectOrAddCircle { event: { x, y }, model: m@{ circles, "Diameter": diameter, undoStack, redoStack } } = case findIndex (\c -> dist c x y <= c.r) circles of
  Just i -> m { selected = .chosen { index: i }, "Diameter" = diameter { current = fromMaybe diameter.current ((\c -> 2.0 * c.r) <$> index circles i) }, drag = .settled {} }
  Nothing ->
    let stacks = pushUndo { circles, undoStack, redoStack }
    in m { circles = snoc circles { x, y, r: freshCircleRadius }, selected = .none {}, undoStack = stacks.undoStack, redoStack = stacks.redoStack }

undo :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) } -> { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) }
undo m@{ undoStack, redoStack, circles } = case unsnoc undoStack of
  Just { init: rest, last: prev } ->
    m { circles = prev, undoStack = rest, redoStack = snoc redoStack circles, selected = .none {}, drag = .settled {} }
  Nothing -> m

redo :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) } -> { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) }
redo m@{ redoStack, undoStack, circles } = case unsnoc redoStack of
  Just { init: rest, last: next } ->
    m { circles = next, redoStack = rest, undoStack = snoc undoStack circles, selected = .none {}, drag = .settled {} }
  Nothing -> m

resizeSelected :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) } -> { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) }
resizeSelected m@{ "Diameter": diameter, circles, selected, drag, undoStack, redoStack } = match
  { chosen: \s -> case index circles s.index of
    Just c | c.r /= diameter.current / 2.0 ->
      let stacks = match { adjusting: \_ -> { undoStack, redoStack }, settled: \_ -> pushUndo { circles, undoStack, redoStack } } drag
      in m { circles = fromMaybe circles (updateAt s.index (c { r = diameter.current / 2.0 }) circles), drag = .adjusting {}, undoStack = stacks.undoStack, redoStack = stacks.redoStack }
    _ -> m
  , none: \_ -> m
  } selected

pushUndo :: forall r1. { circles :: Array { x :: Number, y :: Number, r :: Number }, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) | r1 } -> { undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
pushUndo { undoStack, circles } = { undoStack: take 100 (snoc undoStack circles), redoStack: [] }

dist :: forall r1. { x :: Number, y :: Number, r :: Number | r1 } -> Number -> Number -> Number
dist c x y = sqrt ((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))

canvasClickedLine :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) } -> String
canvasClickedLine { circles, selected } = match { chosen: \s -> "Selected circle " <> show (s.index + 1), none: \_ -> "Added circle " <> show (length circles) } selected

undoneLine :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) } -> String
undoneLine { undoStack, redoStack } = "Undo: " <> show (length undoStack) <> " left to undo, " <> show (length redoStack) <> " to redo"

redoneLine :: { "Diameter" :: { current :: Number, max :: Number, min :: Number, step :: [ continuous :: {}, discrete :: Number ] }, circles :: Array { r :: Number, x :: Number, y :: Number }, drag :: [ adjusting :: {}, settled :: {} ], redoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ), selected :: [ chosen :: { index :: Int }, none :: {} ], undoStack :: Array (Array { r :: Number, x :: Number, y :: Number } ) } -> String
redoneLine { undoStack, redoStack } = "Redo: " <> show (length redoStack) <> " left to redo, " <> show (length undoStack) <> " to undo"
