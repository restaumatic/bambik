-- | 7GUIs task 6: **Circle Drawer** — click to create circles, select and
-- | resize them, with undo/redo.
-- |
-- | The canvas is a `×→+` citizen like a button: it receives the whole
-- | model (and renders the circles), and emits one event case — a click
-- | with coordinates. It stays dumb: hit-testing, selection, creation,
-- | resizing and the undo/redo stacks all live in one fold (`handle`),
-- | plain model functions. The radius slider is an ordinary `×→×` editor
-- | of the model's `radius` field, shown only while a circle is selected;
-- | applying it to the selected circle (and snapshotting undo once per
-- | adjustment session) is the fold's `state` case.
-- |
-- | Adaptation: selection is by left click inside a circle (the reference
-- | uses hover + right-click popup); a resize session becomes one undoable
-- | action, snapshotted at its first slider change.
module Main (main) where

import Prelude

import Data.Array (findIndex, index, mapWithIndex, snoc, take, unsnoc, updateAt)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap, wrap)
import Data.Number (sqrt)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (joinWith)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Control.Monad.State (gets)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))
import UI (UI, looped, silence)
import Web (Node, Web, body, shownWhen, staticHTML)

foreign import setInnerHTML :: Node -> String -> Effect Unit
foreign import onCanvasClick :: Node -> (Number -> Number -> Effect Unit) -> Effect Unit

type Circle = { x :: Number, y :: Number, r :: Number }

type Scene =
  { circles :: Array Circle
  , selected :: Maybe Int
  , adjusting :: Boolean
  , undoStack :: Array (Array Circle)
  , redoStack :: Array (Array Circle)
  }

type Model =
  { circles :: Array Circle
  , selected :: Maybe Int
  , radius :: Number
  , adjusting :: Boolean
  , undoStack :: Array (Array Circle)
  , redoStack :: Array (Array Circle)
  }

main :: Effect Unit
main = body @Unit $ MDC.elevation20 $ MDC.card { caption: Just "Circle Drawer" } Semigroupoid.do
  lcmap (const initial) $ looped Semigroupoid.do
    RecordToRecord.do
      shownWhen (\(m :: Model) -> isJust m.selected) $ lcmap (\m -> { radius: m.radius }) $
        MDC.slider @"radius" { label: "Diameter", min: 2.0, max: 100.0, step: Nothing }
      (identity :: UI Web Scene Scene)
    RecordToVariant.do
      canvas
      MDC.button @"undo" { label: Just "Undo", icon: Just "undo" }
      MDC.button @"redo" { label: Just "Redo", icon: Just "redo" }
      (recordToCase @"state" identity :: UI Web Model [ state :: Model ])
    rmap handle identity
  silence

initial :: Model
initial = { circles: [], selected: Nothing, radius: 20.0, adjusting: false, undoStack: [], redoStack: [] }

handle :: [ clicked :: { x :: Number, y :: Number, model :: Model }, undo :: Model, redo :: Model, state :: Model ] -> Model
handle = Variant.case_
  # Variant.on (Proxy @"state") applyRadius
  # Variant.on (Proxy @"clicked") (\{ x, y, model: m } ->
      case findIndex (\c -> dist c x y <= c.r) m.circles of
        Just i -> m { selected = Just i, radius = fromMaybe m.radius (_.r <$> index m.circles i), adjusting = false }
        Nothing -> (pushUndo m) { circles = snoc m.circles { x, y, r: 20.0 }, selected = Nothing })
  # Variant.on (Proxy @"undo") (\m -> case unsnoc m.undoStack of
      Just { init: rest, last: circles } ->
        m { circles = circles, undoStack = rest, redoStack = snoc m.redoStack m.circles, selected = Nothing, adjusting = false }
      Nothing -> m)
  # Variant.on (Proxy @"redo") (\m -> case unsnoc m.redoStack of
      Just { init: rest, last: circles } ->
        m { circles = circles, redoStack = rest, undoStack = snoc m.undoStack m.circles, selected = Nothing, adjusting = false }
      Nothing -> m)

-- the resize session: the slider edits `radius`; when it differs from the
-- selected circle's, apply it — snapshotting undo once per session
applyRadius :: Model -> Model
applyRadius m = case m.selected of
  Just i | Just c <- index m.circles i, c.r /= m.radius ->
    let m' = if m.adjusting then m else (pushUndo m) { adjusting = true }
    in m' { circles = fromMaybe m.circles (updateAt i (c { r = m.radius }) m.circles) }
  _ -> m

pushUndo :: Model -> Model
pushUndo m = m { undoStack = take 100 (snoc m.undoStack m.circles), redoStack = [] }

dist :: Circle -> Number -> Number -> Number
dist c x y = sqrt ((c.x - x) * (c.x - x) + (c.y - y) * (c.y - y))

-- | The canvas: a `×→+` citizen — the model in (rendered as SVG), one
-- | click case out. The click carries the model it was aimed at, so the
-- | fold can hit-test without the canvas knowing what a selection is.
canvas :: UI Web Model [ clicked :: { x :: Number, y :: Number, model :: Model } ]
canvas = wrap do
  _ <- unwrap (staticHTML """<svg width="500" height="300" style="border: 1px solid #ccc; display: block; margin: 10px 0; background: white;"></svg>""")
  node <- gets _.sibling
  lastRef <- liftEffect $ Ref.new initial
  pure
    { toUser: \m -> do
        Ref.write m lastRef
        setInnerHTML node (joinWith "" (m.circles # mapWithIndex \i c ->
          "<circle cx=\"" <> show c.x <> "\" cy=\"" <> show c.y <> "\" r=\"" <> show c.r
            <> "\" stroke=\"#333\" fill=\"" <> (if m.selected == Just i then "#ddd" else "transparent") <> "\"/>"))
    , fromUser: \prop ->
        onCanvasClick node \x y -> do
          m <- Ref.read lastRef
          void $ prop (.clicked { x, y, model: m })
    }
