-- | The **Shoelace** vocabulary (https://shoelace.style, continued as Web
-- | Awesome) — one of the non-Material design systems, and the evidence
-- | that they are interchangeable: names and signatures match the Material
-- | modules wherever both catalogues have the concept, so a screen changes
-- | design system by changing this one import. What a catalogue has of its
-- | own appears under its own name — here the star `rating`, which Material
-- | has no counterpart for.
-- |
-- | **The page must load** the Shoelace light theme stylesheet, from the
-- | same release as the bundled components; icons load from the matching
-- | CDN, whose base path this vocabulary's `body` sets at mount. No webfont
-- | is needed — Shoelace uses the system font stack.
-- |
-- | The catalogue: `textField`/`textArea`, `rating`, `sliderLive` and
-- | `toggleSwitch` to enter values, `select` to choose one, `button` to
-- | act, `toast` to say what happened, `progressBar` to show a figure,
-- | `card` and `divider` for structure, `body` to mount the app.
-- | Typography is deliberately absent: Shoelace styles plain HTML, so the
-- | `PUI.Web.HTML` elements are the type scale.
module PUI.Web.Shoelace
  ( body
  , button
  , card
  , divider
  , indeterminateLinearProgress
  , progressBar
  , rating
  , select
  , selectUnpicked
  , selectOptional
  , sliderLive
  , textArea
  , textField
  , toast
  , toggleSwitch
  ) where

import Data.Profunctor.Row.Structural (withStructuralEq)
import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Int (fromString)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row.VariantToVariant (forCase)
import Data.Variant (match) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import PUI (Ocular, PUI)
import PUI.Web.HTML (div, span)
import PUI.Web.HTML (body) as HTML
import PUI.Web (selectedAt, selectedOptionalAt, selectedUnpickedAt, Node, OptCaption(..), Web, attribute, displayWiring, editorWiring, el, element, escaped, eventLeaf, eventText, getChecked, getValue, isFocused, listenNode, removeAttribute, selectorWiring, setAttribute, setChecked, setValue, staticHTML, staticText, statusWiring, textOf, separating, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)

-- Implementation notes — the reference above is the contract.
--
-- Shoelace (https://shoelace.style — the design system continued as Web
-- Awesome) components implemented as PUI Web/Ocular (PUI Web) datatypes —
-- a design-system vocabulary beside `PUI.Web.MDC2`/`PUI.Web.MDC3`, proving the
-- vocabularies interchangeable: built on the framework-agnostic
-- `@shoelace-style/shoelace` custom elements (`<sl-button>`, `<sl-rating>`,
-- ...), registered by importing the FFI module, so a component leaf is just
-- `element "sl-..."` plus property/event wiring — exactly the `PUI.Web.MDC3`
-- recipe, and the leaf-echo protocols are the same (focus-guarded text
-- fields, per-feed display echo, a selection editor answering every feed with its row). Two-sorted, same citizenship, and — where the concept exists
-- in both catalogs — the same names and signatures (`textField` carries
-- Shoelace's plain `label` instead of MD's `floatingLabel`; the catalog has
-- no fill/outline split), so a demo switches design systems by switching
-- the import:
--
--   * **components** — UI components with a model interface, every one a citizen
--     of exactly one row shape:
--       `×→×` editors — `textField @l`, `textArea @l`, `rating @l` (the
--         star editor, `{ value :: Number }` — Shoelace's distinctive
--         catalog entry), `sliderLive @l` (`<sl-range>` — reports per drag
--         step, the value shown by the control's own tooltip),
--         `toggleSwitch @l` (`<sl-switch>`), and the
--         type-changing `select @l` (`{ l :: f } → { l :: f }`, `f` the option itself, or a
--         variant around it for the `…Unpicked`/`…Optional` siblings);
--       `×→×` displays — `progressBar` (`<sl-progress-bar>`,
--         `{ value :: Number } → {}`, the filled fraction 0–1);
--       `×→+` events — `button @l` (`<sl-button variant="primary">`);
--       `+→×` statuses — `toast` (`<sl-alert>` shown on feed,
--         auto-dismissing via its own `duration`) — each taking
--         its business case and copy function (`toast @"booked" bookedLine`).
--   * **oculars** — shape-preserving decorators: `card` (`<sl-card>`, a
--     plain surface). Typography is deliberately absent:
--     Shoelace styles plain HTML through its tokens, so the `PUI.Web.HTML`
--     element oculars are the typography.
--   * plus **announcing statics** (`{}`-output chrome with a face, at any row):
--     `divider` (`<sl-divider>`).
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (a `settled`
-- normalization on the whole-row stage), not in a leaf bracket.

-- UIs

-- | The **primary button**: the screen's action. It reports on click,
-- | carrying the data it was showing, under the name the app gives the
-- | action — `button @"Submit the review" {}`. The label defaults to
-- | the case label verbatim (`label:` overrides with real copy).
button :: forall @l provided r v. IsSymbol l => Cons l { | r } () v => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } [ | v ]
button provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { label :: String } in eventLeaf @l $
  el "sl-button" >>> "variant" := "primary" $ staticText config.label

-- | The **text field**: a labelled single-line input. Shows the string it
-- | is given and reports each edit; typing is never interrupted by values
-- | arriving from elsewhere. A whole-row citizen: fed the wide row, it
-- | edits field `l` and carries the rest.
textField :: forall @l r b provided. IsSymbol l => Cons l String b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
textField provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  -- the shadow input keeps the host as `activeElement`, so the focus guard
  -- reads the host
  element "sl-input" (pure unit)
  attribute "label" config.label
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "sl-input"
    , read: \_ -> getValue node
    }

-- | The **multi-line text field**, `rows` lines tall — a note, a review, a
-- | message. Otherwise `textField`.
textArea :: forall @l r b provided. IsSymbol l => Cons l String b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String, rows :: Int } => { | provided } -> PUI Web { | r } { | r }
textArea provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "sl-textarea" (pure unit)
  attribute "label" config.label
  attribute "rows" (show config.rows)
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "sl-input"
    , read: \_ -> getValue node
    }

-- | The **star rating** — Shoelace's distinctive control, with no Material
-- | counterpart: a judgement given by picking a point on a scale the user
-- | recognises at a glance.
-- |
-- | The scale is part of the rating, not part of the screen:
-- | `{ current, max }` travels together as one business datum, so how many
-- | stars there are comes from the data and can differ between contexts —
-- | and a scale nobody supplied is a compile error rather than a wrong
-- | screen. The label is drawn above the stars.
rating :: forall @l r b provided. IsSymbol l => Cons l { current :: Number, max :: Int } b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
rating provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $
  div >>> "style" := "display: inline-flex; flex-direction: column; gap: var(--sl-spacing-3x-small);" $ wrap do
    _ <- unwrap (span >>> "style" := "font-size: var(--sl-input-label-font-size-medium); color: var(--sl-input-label-color);" $ staticText config.label)
    element "sl-rating" (pure unit)
    attribute "label" config.label
    node <- gets _.sibling
    liftEffect $ editorWiring
      { show: \q -> do
          setAttribute node "max" (show q.max)
          setNumberProp "value" node q.current
      , held: pure false
      , reports: listenNode node "sl-change"
      , read: \q -> getNumberProp "value" node <#> \v -> q { current = v }
      }

-- | The **slider**: a quantity chosen by feel, where the range matters more
-- | than the exact number.
-- |
-- | The range is part of the quantity, not part of the screen:
-- | `{ current, min, max, step }` travels together as one business datum, so
-- | limits come from the data and can change while the app runs — a slider
-- | is never silently out of range, and a range nobody supplied is a
-- | compile error rather than a wrong screen. `step` is `.discrete n`
-- | or `.continuous {}`, named like every other two-state field.
-- |
-- | It reports on **every change**, following the drag — so whatever it
-- | drives should be cheap to redo, or be `debounced` downstream. The
-- | current number shows in the control's own tooltip while dragging.
sliderLive :: forall @l r b provided. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
sliderLive provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "sl-range" (pure unit)
  attribute "label" config.label
  attribute "style" "width: 100%; min-width: 240px;"
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: \q -> do
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        Variant.match { discrete: \s -> setAttribute node "step" (show s), continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "value" node q.current
    , held: pure false
    , reports: listenNode node "sl-input"
    , read: \q -> getNumberProp "value" node <#> \v -> q { current = v }
    }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | The label sits beside it and is part of the target, so clicking the
-- | words toggles it too.
toggleSwitch :: forall @l r b provided. IsSymbol l => Cons l Boolean b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
toggleSwitch provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "sl-switch" (void $ unwrap (staticText config.label))
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: setChecked node
    , held: pure false
    , reports: listenNode node "sl-change"
    , read: \_ -> getChecked node
    }

-- | The **dropdown**: one choice out of a list too long to lay out in the
-- | open.
-- | The field holds the option itself, so the model always has one.
-- | Two siblings hold a variant instead: `selectUnpicked @l @c` for a
-- | choice owed but not yet made, `selectOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- | The options belong to the control, not to the model.
select :: forall @l a b r provided. IsSymbol l => Cons l a b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | r } { | r }
select provided options = withStructuralEq @a (selectWith @l false (selectedAt @l) provided options)

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
selectUnpicked :: forall @l @c a b1 v b2 r provided. IsSymbol l => IsSymbol c => Cons c a b1 v => Cons l [ | v ] b2 r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectUnpicked provided options = withStructuralEq @a (selectWith @l false (selectedUnpickedAt @l @c) provided options)

-- | `select` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | its clear button clears it, storing `n` again.
selectOptional :: forall @l @c @n a b1 t v b2 r provided. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} t v => Cons l [ | v ] b2 r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectOptional provided options = withStructuralEq @a (selectWith @l true (selectedOptionalAt @l @c @n) provided options)

selectWith :: forall @l a i o provided. IsSymbol l => Eq a => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> { | provided } -> Array { value :: a, label :: String } -> PUI Web i o
selectWith clearable lift provided options = lift $ "name" := reflectSymbol (Proxy @l) $ wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  liftEffect $ selectorWiring options
    { show: setValue node <<< maybe "" show
    , reports: listenNode node "sl-change"
    , read: fromString <$> getValue node
    }
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided
  markup =
    "<sl-select label=\"" <> escaped config.label <> "\"" <> (if clearable then " clearable" else "") <> " style=\"min-width: 240px;\">"
      <> foldMapWithIndex optionMarkup options
      <> "</sl-select>"
  optionMarkup idx o = "<sl-option value=\"" <> show idx <> "\">" <> escaped o.label <> "</sl-option>"

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a quota, a share, a
-- | rating out of five — `progressBar progressFraction`: the
-- | value is a **read function** like every display's, since a fraction is
-- | derived (a ratio of source fields), not state.
progressBar
  :: forall reads
   . ({ | reads } -> Number) -> PUI Web { | reads } {}
progressBar f = wrap do
  element "sl-progress-bar" (pure unit)
  attribute "style" "width: 100%; min-width: 200px;"
  node <- gets _.sibling
  -- sl-progress-bar runs 0–100
  liftEffect $ displayWiring \r -> setNumberProp "value" node (f r * 100.0)

-- | The **toast**: a brief message at the bottom of the screen that
-- | dismisses itself, for something that has just happened and needs no
-- | reply. It never interrupts.
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the toast is built — `toast @"submitted" thanksLine` — and
-- | let the event carry the bare facts.
toast
  :: forall @l a v r
   . IsSymbol l
  => Cons l a () v
  => (a -> String)
  -> PUI Web [ | v ] { | r }
toast copy = toastFace # forCase @l copy

toastFace :: forall r. PUI Web [ event :: String ] { | r }
toastFace = wrap do
  w <- unwrap $ el "sl-alert" >>> "variant" := "primary" >>> "duration" := "5000" >>> "closable" := ""
    >>> "style" := "position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000; min-width: 300px;" $ wrap do
    _ <- unwrap (el "sl-icon" >>> "slot" := "icon" >>> "name" := "check2-circle" $ staticText "")
    unwrap (textOf eventText)
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        showAlert node
    , fromUser: w.fromUser
    }

-- UIOculars

-- | A **card**: a surface holding one subject's content. The body stacks
-- | its children with even spacing, so a form or a summary can be dropped
-- | in without spacing each row by hand.
-- |
-- | A plain ocular, with no config of its own: a card is a *surface*, and a
-- | heading of its own is ordinary typography placed in its content —
-- | Shoelace styles plain HTML, so the HTML oculars are that type scale.
card :: Ocular (PUI Web)
card content = el "sl-card" $
  div >>> "style" := "display: flex; flex-direction: column; align-items: flex-start; gap: var(--sl-spacing-medium);" $ content

-- announcing statics ({}-output chrome with a face, at any row)

-- | A **divider**: the hairline rule between sections of a surface. Fixed
-- | decoration, carrying no data.
dividerExact :: PUI Web {} {}
dividerExact = staticHTML "<sl-divider style=\"width: 100%;\"></sl-divider>"

divider :: Ocular (PUI Web)
divider = separating dividerExact

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import showAlert :: Node -> Effect Unit

-- Entry point

-- | Mount the app in the page's `<body>`, dressed for Shoelace: the icon
-- | base path — where `sl-rating`'s stars and the alert icons are fetched
-- | from, the CDN release matching the bundled components — is set here,
-- | and the app then mounts exactly as `PUI.Web.HTML.body` does — same
-- | signature, same closed-app demand (`PUI Web {} o`). The entry is a
-- | word of the vocabulary like any other, so a screen changes design
-- | system at its first line by changing the import, and nothing runs at
-- | import time.
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptIconBasePath
  HTML.body ui

foreign import adoptIconBasePath :: Effect Unit

-- | The **indeterminate progress bar**: a status fed a run's `started` and
-- | `ended`, shown between them — what `PUI.action`'s slot dispatches
-- | (`indeterminateLinearProgress # action submit`). No label: it names no
-- | field and no case.
indeterminateLinearProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateLinearProgress = wrap do
  element "sl-progress-bar" (pure unit)
  attribute "indeterminate" ""
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ statusWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "width: 100%; min-width: 200px;"
  hiddenStyle = "width: 100%; min-width: 200px; visibility: hidden;"
