module CircleDrawerLogic (canvasCircles, emptyCanvas, redo, resizeSelected, selectOrAddCircle, selection, undo) where

import Prelude ((*), (+), (-), (/), (/=), (<$>), (<=), (==), show)

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (sqrt)
import Data.Variant (match)

emptyCanvas :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], "Diameter" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
emptyCanvas =
  { circles: []
  , selected: .none {}
  , "Diameter": { current: 40.0, min: minDiameter, max: maxDiameter, step: Nothing }
  , drag: .settled {}
  , undoStack: []
  , redoStack: []
  }

minDiameter :: Number
minDiameter = 4.0

maxDiameter :: Number
maxDiameter = 200.0

canvasCircles
  :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ] }
  -> Array { key :: String, x :: String, y :: String, r :: String, status :: [ selected :: {}, unselected :: {} ] }
canvasCircles { circles, selected } = mapWithIndex (\i c -> { key: show i, x: show c.x, y: show c.y, r: show c.r, status: statusOf i }) circles
  where
  statusOf i = match { chosen: \s -> if s.index == i then .selected {} else .unselected {}, none: \_ -> .unselected {} } selected

selectOrAddCircle :: { x :: Number, y :: Number } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], "Diameter" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], "Diameter" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
selectOrAddCircle { x, y } m@{ circles, "Diameter": diameter, undoStack, redoStack } = case findIndex (\c -> dist c x y <= c.r) circles of
  Just i -> m { selected = .chosen { index: i }, "Diameter" = diameter { current = fromMaybe diameter.current ((\c -> 2.0 * c.r) <$> index circles i) }, drag = .settled {} }
  Nothing ->
    let stacks = pushUndo { circles, undoStack, redoStack }
    in m { circles = snoc circles { x, y, r: 20.0 }, selected = .none {}, undoStack = stacks.undoStack, redoStack = stacks.redoStack }

undo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
undo m@{ undoStack, redoStack, circles } = case unsnoc undoStack of
  Just { init: rest, last: prev } ->
    m { circles = prev, undoStack = rest, redoStack = snoc redoStack circles, selected = .none {}, drag = .settled {} }
  Nothing -> m

redo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
redo m@{ redoStack, undoStack, circles } = case unsnoc redoStack of
  Just { init: rest, last: next } ->
    m { circles = next, redoStack = rest, undoStack = snoc undoStack circles, selected = .none {}, drag = .settled {} }
  Nothing -> m

selection :: { selected :: [ chosen :: { index :: Int }, none :: {} ] } -> [ chosen :: { index :: Int }, none :: {} ]
selection { selected } = selected

-- a drag resizes the chosen circle live; the first step of a drag is the one
-- undo transaction, later steps ride it
resizeSelected :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], "Diameter" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: [ chosen :: { index :: Int }, none :: {} ], "Diameter" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, drag :: [ adjusting :: {}, settled :: {} ], undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
resizeSelected m@{ "Diameter": diameter, circles, selected, drag, undoStack, redoStack } = match
  { chosen: \s -> case index circles s.index of
      Just c | c.r /= diameter.current / 2.0 ->
        let stacks = match { adjusting: \_ -> { undoStack, redoStack }, settled: \_ -> pushUndo { circles, undoStack, redoStack } } drag
        in m { circles = fromMaybe circles (updateAt s.index (c { r = diameter.current / 2.0 }) circles), drag = .adjusting {}, undoStack = stacks.undoStack, redoStack = stacks.redoStack }
      _ -> m
  , none: \_ -> m
  } selected

pushUndo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
pushUndo { undoStack, circles } = { undoStack: take 100 (snoc undoStack circles), redoStack: [] }

dist :: { x :: Number, y :: Number, r :: Number } -> Number -> Number -> Number
dist c x y = sqrt ((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))
