module CircleDrawer (Canvas, Circle, canvasCircles, emptyCanvas, redo, resizeSelected, selectOrAddCircle, undo) where

import Prelude (show, (*), (+), (-), (/), (/=), (<=), (==))

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (sqrt)
import Data.Variant (match)

type Canvas =
  { "Diameter" ::
    { current :: Number
    , max :: Number
    , min :: Number
    , step :: [ continuous :: {}, discrete :: Number ]
    }
  , circles :: Array Circle
  , drag :: [ adjusting :: {}, settled :: {} ]
  , redoStack :: Array (Array Circle)
  , selected :: [ chosen :: { index :: Int }, none :: {} ]
  , undoStack :: Array (Array Circle)
  }

type Circle = { r :: Number, x :: Number, y :: Number }

emptyCanvas :: Canvas
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

canvasCircles
  :: Canvas
  -> Array
       { key :: String
       , r :: String
       , status :: [ selected :: {}, unselected :: {} ]
       , x :: String
       , y :: String
       }
canvasCircles { circles, selected } = mapWithIndex shape circles
  where
  shape i c = { key: show i, x: show c.x, y: show c.y, r: show c.r, status: statusOf i }
  statusOf i = match
    { chosen: \s -> if s.index == i then .selected {} else .unselected {}
    , none: \_ -> .unselected {}
    } selected

selectOrAddCircle :: { event :: { x :: Number, y :: Number }, model :: Canvas } -> Canvas
selectOrAddCircle { event: { x, y }, model: m@{ circles, "Diameter": diameter } } =
  case findIndex (\c -> dist c x y <= c.r) circles of
    Just i -> m
      { selected = .chosen { index: i }
      , "Diameter" = diameter { current = maybe diameter.current (\c -> 2.0 * c.r) (index circles i) }
      , drag = .settled {}
      }
    Nothing -> (snapshot m) { circles = snoc circles { x, y, r: freshCircleRadius }, selected = .none {} }

undo :: Canvas -> Canvas
undo m@{ undoStack, redoStack, circles } = case unsnoc undoStack of
  Just { init: rest, last: prev } ->
    m { circles = prev, undoStack = rest, redoStack = snoc redoStack circles, selected = .none {}, drag = .settled {} }
  Nothing -> m

redo :: Canvas -> Canvas
redo m@{ redoStack, undoStack, circles } = case unsnoc redoStack of
  Just { init: rest, last: next } ->
    m { circles = next, redoStack = rest, undoStack = snoc undoStack circles, selected = .none {}, drag = .settled {} }
  Nothing -> m

resizeSelected :: Canvas -> Canvas
resizeSelected m@{ "Diameter": diameter, circles, selected, drag } = match
  { chosen: \{ index: i } -> case index circles i of
      Just c | c.r /= radius ->
        (match { adjusting: \_ -> m, settled: \_ -> snapshot m } drag)
          { circles = fromMaybe circles (updateAt i (c { r = radius }) circles), drag = .adjusting {} }
      _ -> m
  , none: \_ -> m
  } selected
  where
  radius = diameter.current / 2.0

snapshot :: Canvas -> Canvas
snapshot m@{ circles, undoStack } = m { undoStack = take 100 (snoc undoStack circles), redoStack = [] }

dist :: Circle -> Number -> Number -> Number
dist c x y = sqrt ((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))
