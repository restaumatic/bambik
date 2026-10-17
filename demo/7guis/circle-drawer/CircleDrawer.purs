module CircleDrawer (Canvas, Circle, adjustDiameter, canvasCircles, circleFill, clickCanvas, closeAdjustment, emptyCanvas, hover, menuPlacement, openMenu, redo, resizeSelected, selectedCircleLine, undo) where

import Prelude (show, (*), (+), (-), (/), (/=), (<), (<$>), (<=), (<>), (==))

import Data.Array (filter, head, index, length, mapWithIndex, snoc, sortWith, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Number (round, sqrt)
import Data.Variant (match)

type Canvas =
  { "Diameter" ::
    { current :: Number
    , max :: Number
    , min :: Number
    , step :: [ continuous :: {}, discrete :: Number ]
    }
  , circles :: Array Circle
  , mode :: [ adjusting :: { before :: Array Circle }, drawing :: {}, menu :: {} ]
  , pointer :: { x :: Number, y :: Number }
  , redoStack :: Array (Array Circle)
  , selected :: [ chosen :: { index :: Int }, none :: {} ]
  , undoStack :: Array (Array Circle)
  }

type Circle = { r :: Number, x :: Number, y :: Number }

type Shape =
  { key :: String
  , r :: String
  , status :: [ selected :: {}, unselected :: {} ]
  , x :: String
  , y :: String
  }

emptyCanvas :: Canvas
emptyCanvas =
  { circles: []
  , selected: .none {}
  , "Diameter": { current: 2.0 * freshCircleRadius, min: minDiameter, max: maxDiameter, step: .continuous {} }
  , mode: .drawing {}
  , pointer: { x: 0.0, y: 0.0 }
  , undoStack: []
  , redoStack: []
  }

minDiameter :: Number
minDiameter = 4.0

maxDiameter :: Number
maxDiameter = 200.0

freshCircleRadius :: Number
freshCircleRadius = 20.0

canvasWidth :: Number
canvasWidth = 500.0

canvasHeight :: Number
canvasHeight = 300.0

canvasCircles :: Canvas -> Array Shape
canvasCircles { circles, selected } = mapWithIndex shape circles
  where
  shape i c = { key: show i, x: show c.x, y: show c.y, r: show c.r, status: statusOf i }
  statusOf i = match
    { chosen: \s -> if s.index == i then .selected {} else .unselected {}
    , none: \_ -> .unselected {}
    } selected

circleFill :: Shape -> String
circleFill { status } = match { selected: \_ -> "#ccc", unselected: \_ -> "transparent" } status

menuPlacement :: Canvas -> String
menuPlacement { pointer: { x, y } } =
  "position: absolute; left: " <> show (100.0 * x / canvasWidth) <> "%; top: " <> show (100.0 * y / canvasHeight) <> "%;"

selectedCircleLine :: Canvas -> String
selectedCircleLine { circles, selected } = match
  { chosen: \{ index: i } -> case index circles i of
      Just c -> "Adjust diameter of circle at (" <> show c.x <> ", " <> show c.y <> ")"
      Nothing -> "Adjust diameter"
  , none: \_ -> "Adjust diameter"
  } selected

hover :: { event :: { x :: Number, y :: Number }, model :: Canvas } -> Canvas
hover { event: p, model: m@{ circles, mode } } = match
  { drawing: \_ -> m { pointer = p, selected = maybe (.none {}) (\i -> .chosen { index: i }) (nearestUnder p circles) }
  , menu: \_ -> m
  , adjusting: \_ -> m
  } mode

clickCanvas :: { event :: { x :: Number, y :: Number }, model :: Canvas } -> Canvas
clickCanvas { event: p@{ x, y }, model: m@{ circles, mode } } = match
  { drawing: \_ ->
      if isJust (nearestUnder p circles) then m
      else (snapshot m) { circles = snoc circles { x: round x, y: round y, r: freshCircleRadius }, selected = .chosen { index: length circles } }
  , menu: \_ -> m { mode = .drawing {} }
  , adjusting: \_ -> closeAdjustment m
  } mode

openMenu :: { event :: { x :: Number, y :: Number }, model :: Canvas } -> Canvas
openMenu { event: p, model: m@{ circles, mode, selected } } = match
  { drawing: \_ -> match
      { chosen: \{ index: i } -> case index circles i of
          Just c | dist c p <= c.r -> m { mode = .menu {}, pointer = p }
          _ -> m
      , none: \_ -> m
      } selected
  , menu: \_ -> m { mode = .drawing {} }
  , adjusting: \_ -> m
  } mode

adjustDiameter :: Canvas -> Canvas
adjustDiameter m@{ circles, selected, "Diameter": diameter } = match
  { chosen: \{ index: i } -> case index circles i of
      Just c -> m { mode = .adjusting { before: circles }, "Diameter" = diameter { current = 2.0 * c.r } }
      Nothing -> m { mode = .drawing {} }
  , none: \_ -> m { mode = .drawing {} }
  } selected

closeAdjustment :: Canvas -> Canvas
closeAdjustment m@{ mode, circles, undoStack } = match
  { adjusting: \{ before } ->
      if before == circles then m { mode = .drawing {} }
      else m { mode = .drawing {}, undoStack = snoc undoStack before, redoStack = [] }
  , drawing: \_ -> m
  , menu: \_ -> m { mode = .drawing {} }
  } mode

resizeSelected :: Canvas -> Canvas
resizeSelected m@{ "Diameter": diameter, circles, selected } = match
  { chosen: \{ index: i } -> case index circles i of
      Just c | c.r /= radius -> m { circles = fromMaybe circles (updateAt i (c { r = radius }) circles) }
      _ -> m
  , none: \_ -> m
  } selected
  where
  radius = diameter.current / 2.0

undo :: Canvas -> Canvas
undo m = case unsnoc settled.undoStack of
  Just { init: rest, last: prev } ->
    settled { circles = prev, undoStack = rest, redoStack = snoc settled.redoStack settled.circles, selected = .none {} }
  Nothing -> settled
  where
  settled = closeAdjustment m

redo :: Canvas -> Canvas
redo m = case unsnoc settled.redoStack of
  Just { init: rest, last: next } ->
    settled { circles = next, redoStack = rest, undoStack = snoc settled.undoStack settled.circles, selected = .none {} }
  Nothing -> settled
  where
  settled = closeAdjustment m

snapshot :: Canvas -> Canvas
snapshot m@{ circles, undoStack } = m { undoStack = snoc undoStack circles, redoStack = [] }

nearestUnder :: { x :: Number, y :: Number } -> Array Circle -> Maybe Int
nearestUnder p circles =
  _.index <$> head (sortWith _.distance (filter (\{ distance, r } -> distance < r) (mapWithIndex measured circles)))
  where
  measured i c = { index: i, distance: dist c p, r: c.r }

dist :: Circle -> { x :: Number, y :: Number } -> Number
dist c { x, y } = sqrt ((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))
