module Main (main) where

import Prelude

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (sqrt)
import Data.Profunctor (rmap)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Tuple (Tuple(..))
import Data.Variant (match) as Variant
import Effect (Effect)
import PUI (asCase, asField, completed, mvu, updates)
import PUI.HTML (Markup(..), attr, body, div, shownWhen, view) as HTML
import PUI.MDC (button, card, elevation20, slider) as MDC
import PUI.Web (onClickXY)
import QualifiedDo.Semigroupoid as Semigroupoid

type Circle = { x :: Number, y :: Number, r :: Number }

type Model =
  { circles :: Array Circle
  , selected :: Maybe Int
  , diameter :: Number
  , adjusting :: Boolean
  , undoStack :: Array (Array Circle)
  , redoStack :: Array (Array Circle)
  }

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "Circle Drawer" } $ ( Semigroupoid.do
      MDC.slider { label: "Diameter", min: 4.0, max: 200.0, step: Nothing } # asField @"diameter"
        # completed # HTML.shownWhen hasSelection # rmap applyDiameter
      ( RecordToVariant.do
          HTML.view
            """<svg viewBox="0 0 500 300" style="border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;"></svg>"""
            renderCanvas
            (\node emit -> onClickXY node \x y -> emit (clickedAt x y))
          HTML.div >>> HTML.attr "style" "display: flex; gap: 8px;" $ RecordToVariant.do
            MDC.button { label: Just "Undo", icon: Just "undo" } # asCase @"undo"
            MDC.button { label: Just "Redo", icon: Just "redo" } # asCase @"redo"
      ) # updates handle
  ) # mvu
      { circles: []
      , selected: Nothing
      , diameter: 40.0
      , adjusting: false
      , undoStack: []
      , redoStack: []
      }


handle ::
  [ clicked :: { x :: Number, y :: Number }
  , undo :: Model
  , redo :: Model
  ]
  -> Model -> Model
handle e m = Variant.match
  { clicked: \{ x, y } ->
      case findIndex (\c -> dist c x y <= c.r) m.circles of
        Just i -> m { selected = Just i, diameter = fromMaybe m.diameter ((\c -> 2.0 * c.r) <$> index m.circles i), adjusting = false }
        Nothing -> (pushUndo m) { circles = snoc m.circles { x, y, r: 20.0 }, selected = Nothing }
  , undo: \_ -> case unsnoc m.undoStack of
      Just { init: rest, last: circles } ->
        m { circles = circles, undoStack = rest, redoStack = snoc m.redoStack m.circles, selected = Nothing, adjusting = false }
      Nothing -> m
  , redo: \_ -> case unsnoc m.redoStack of
      Just { init: rest, last: circles } ->
        m { circles = circles, redoStack = rest, undoStack = snoc m.undoStack m.circles, selected = Nothing, adjusting = false }
      Nothing -> m
  } e

applyDiameter :: Model -> Model
applyDiameter m = case m.selected of
  Just i | Just c <- index m.circles i, c.r /= m.diameter / 2.0 ->
    let m' = if m.adjusting then m else (pushUndo m) { adjusting = true }
    in m' { circles = fromMaybe m.circles (updateAt i (c { r = m.diameter / 2.0 }) m.circles) }
  _ -> m

pushUndo :: Model -> Model
pushUndo m = m { undoStack = take 100 (snoc m.undoStack m.circles), redoStack = [] }

dist :: Circle -> Number -> Number -> Number
dist c x y = sqrt ((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))

clickedAt :: Number -> Number -> [ clicked :: { x :: Number, y :: Number } ]
clickedAt x y = .clicked { x, y }

renderCanvas :: Model -> Array HTML.Markup
renderCanvas m = mapWithIndex circle m.circles
  where
  circle i c = HTML.Element "circle"
    [ Tuple "cx" (show c.x)
    , Tuple "cy" (show c.y)
    , Tuple "r" (show c.r)
    , Tuple "stroke" "#333"
    , Tuple "fill" (if m.selected == Just i then "#ddd" else "transparent")
    ]
    []

hasSelection :: Model -> Boolean
hasSelection m = isJust m.selected
