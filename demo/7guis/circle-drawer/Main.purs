module Main (main) where

import Prelude

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Number (sqrt)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (completed)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (joinWith)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import PUI (PUI, looped, updates, with)
import PUI.MDC as MDC
import PUI.Web (Node, Web, body, shownWhen, viewEvents)
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))

foreign import onCanvasClick :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit

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
main = body $ MDC.elevation20 $ MDC.card { caption: Just "Circle Drawer" } $ looped
  $ with
    { circles: []
    , selected: Nothing
    , diameter: 40.0
    , adjusting: false
    , undoStack: []
    , redoStack: []
    }
  $ Semigroupoid.do
  completed
    ( shownWhen hasSelection $
        MDC.slider @"diameter" { label: "Diameter", min: 4.0, max: 200.0, step: Nothing }
          # lcmap diameterField
    )
    # rmap applyDiameter
  updates handle RecordToVariant.do
    canvas
    MDC.button @"undo" { label: Just "Undo", icon: Just "undo" }
    MDC.button @"redo" { label: Just "Redo", icon: Just "redo" }


handle ::
  [ clicked :: { x :: Number, y :: Number }
  , undo :: Model
  , redo :: Model
  ]
  -> Model -> Model
handle e m = e # (Variant.case_
  # Variant.on (Proxy @"clicked") (\{ x, y } ->
      case findIndex (\c -> dist c x y <= c.r) m.circles of
        Just i -> m { selected = Just i, diameter = fromMaybe m.diameter ((\c -> 2.0 * c.r) <$> index m.circles i), adjusting = false }
        Nothing -> (pushUndo m) { circles = snoc m.circles { x, y, r: 20.0 }, selected = Nothing })
  # Variant.on (Proxy @"undo") (\_ -> case unsnoc m.undoStack of
      Just { init: rest, last: circles } ->
        m { circles = circles, undoStack = rest, redoStack = snoc m.redoStack m.circles, selected = Nothing, adjusting = false }
      Nothing -> m)
  # Variant.on (Proxy @"redo") (\_ -> case unsnoc m.redoStack of
      Just { init: rest, last: circles } ->
        m { circles = circles, redoStack = rest, undoStack = snoc m.undoStack m.circles, selected = Nothing, adjusting = false }
      Nothing -> m))

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

canvas :: PUI Web Model [ clicked :: { x :: Number, y :: Number } ]
canvas = viewEvents
  """<svg viewBox="0 0 500 300" style="border: 1px solid #ccc; display: block; margin: 10px 0; background: white; width: 100%; max-width: 500px; height: auto; touch-action: none;"></svg>"""
  render
  (\node emit -> onCanvasClick node \x y -> emit (.clicked { x, y }))
  where
  render m = joinWith "" (m.circles # mapWithIndex \i c ->
    "<circle cx=\"" <> show c.x <> "\" cy=\"" <> show c.y <> "\" r=\"" <> show c.r
      <> "\" stroke=\"#333\" fill=\"" <> (if m.selected == Just i then "#ddd" else "transparent") <> "\"/>")

hasSelection :: Model -> Boolean
hasSelection m = isJust m.selected

diameterField :: Model -> { diameter :: Number }
diameterField m = { diameter: m.diameter }
