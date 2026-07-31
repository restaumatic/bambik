module CircleDrawerMD3 (circleDrawerMD3) where

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
import PUI.MDC3 (button, card, cardActions, elevation5, sliderLive)
import PUI.SVG (circle, svg)
import QualifiedDo.Semigroupoid as Semigroupoid

circleDrawerMD3 :: Effect Unit
circleDrawerMD3 =
  body $
    elevation5 $
      card { caption: "Circle Drawer" } $ ( Semigroupoid.do
          sliderLive { min: minDiameter, max: maxDiameter } # asField @"diameter" # provided # lcmap selectedDiameter # updates adjustDiameter
          ( RecordToVariant.do
              svg >>> "viewBox" := "0 0 500 300" >>> "style" := "border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;" $
                ( onClickedXY
                    ( ( circle >>> "stroke" := "#333" >>> attrWith "cx" _.x >>> attrWith "cy" _.y >>> attrWith "r" _.r
                          >>> attrWith "fill" (\c -> if c.on then "#ddd" else "transparent") $ pempty # lcmap (const {})) # foreach @"key" # lcmap canvasCircles) # toCase @"clicked")
              cardActions $ RecordToVariant.do
                button { label: "Undo", icon: "undo" } # asCase @"undo"
                button { label: "Redo", icon: "redo" } # asCase @"redo") # updates (match { clicked: selectOrAddCircle, undo: const <<< undo, redo: const <<< redo })
      ) # mvu emptyCanvas

canvasCircles
  :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int }
  -> Array { key :: String, x :: String, y :: String, r :: String, on :: Boolean }
canvasCircles { circles, selected } = mapWithIndex (\i c -> { key: show i, x: show c.x, y: show c.y, r: show c.r, on: selected == Just i }) circles

selectOrAddCircle :: { x :: Number, y :: Number } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
selectOrAddCircle { x, y } m@{ circles, diameter, undoStack, redoStack } = case findIndex (\c -> dist c x y <= c.r) circles of
  Just i -> m { selected = Just i, diameter = fromMaybe diameter ((\c -> 2.0 * c.r) <$> index circles i), adjusting = false }
  Nothing ->
    let stacks = pushUndo { circles, undoStack, redoStack }
    in m { circles = snoc circles { x, y, r: 20.0 }, selected = Nothing, undoStack = stacks.undoStack, redoStack = stacks.redoStack }

undo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, diameter :: Number, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, diameter :: Number, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
undo m@{ undoStack, redoStack, circles } = case unsnoc undoStack of
  Just { init: rest, last: prev } ->
    m { circles = prev, undoStack = rest, redoStack = snoc redoStack circles, selected = Nothing, adjusting = false }
  Nothing -> m

redo :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, diameter :: Number, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, adjusting :: Boolean, diameter :: Number, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
redo m@{ redoStack, undoStack, circles } = case unsnoc redoStack of
  Just { init: rest, last: next } ->
    m { circles = next, redoStack = rest, undoStack = snoc undoStack circles, selected = Nothing, adjusting = false }
  Nothing -> m

selectedDiameter :: { selected :: Maybe Int, diameter :: Number } -> Maybe { diameter :: Number }
selectedDiameter { selected, diameter } = if isJust selected then Just { diameter } else Nothing

adjustDiameter :: { diameter :: Number } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
adjustDiameter { diameter } m = applyDiameter (m { diameter = diameter })

applyDiameter :: { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) } -> { circles :: Array { x :: Number, y :: Number, r :: Number }, selected :: Maybe Int, diameter :: Number, adjusting :: Boolean, undoStack :: Array (Array { x :: Number, y :: Number, r :: Number }), redoStack :: Array (Array { x :: Number, y :: Number, r :: Number }) }
applyDiameter m@{ selected, circles, diameter, adjusting, undoStack, redoStack } = case selected of
  Just i | Just c <- index circles i, c.r /= diameter / 2.0 ->
    let m' = if adjusting then m
             else let stacks = pushUndo { circles, undoStack, redoStack }
                  in m { adjusting = true, undoStack = stacks.undoStack, redoStack = stacks.redoStack }
    in m' { circles = fromMaybe circles (updateAt i (c { r = diameter / 2.0 }) circles) }
  _ -> m

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
