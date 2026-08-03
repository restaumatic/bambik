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
-- | CDN. No webfont is needed — Shoelace uses the system font stack.
-- |
-- | The catalogue: `textField`/`textArea`, `rating`, `sliderLive` and
-- | `toggleSwitch` to enter values, `select` to choose one, `button` to
-- | act, `toast` to say what happened, `progressBar` to show a figure,
-- | `card` and `divider` for structure. Typography is deliberately absent:
-- | Shoelace styles plain HTML, so the `PUI.Web.HTML` elements are the
-- | type scale.
module PUI.Web.Shoelace
  ( button
  , card
  , divider
  , progressBar
  , rating
  , select
  , sliderLive
  , textArea
  , textField
  , toast
  , toggleSwitch
  ) where

import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.Array ((!!), findIndex)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Row.RecordToRecord (field, projected)
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (PUI, constantly)
import PUI.Web.HTML (clicked, div, el, span, staticHTML, staticText, text, (:=))
import PUI.Web (Node, Web, addEventListener, attribute, element, getChecked, getValue, isFocused, removeAttribute, setAttribute, setChecked, setValue)
import Type.Proxy (Proxy(..))

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
-- fields, per-feed display echo, `Just`-only echo on the type-changing
-- selector). Two-sorted, same citizenship, and — where the concept exists
-- in both catalogs — the same names and signatures (`textField` carries
-- Shoelace's plain `label` instead of MD's `floatingLabel`; the catalog has
-- no fill/outline split), so a demo switches design systems by switching
-- the import:
--
--   * **components** — widgets with a model interface, every one a citizen
--     of exactly one row direction:
--       `×→×` editors — `textField @l`, `textArea @l`, `rating @l` (the
--         star editor, `{ value :: Number }` — Shoelace's distinctive
--         catalog entry), `sliderLive @l` (`<sl-range>` — reports per drag
--         step, the value shown by the control's own tooltip),
--         `toggleSwitch @l` (`<sl-switch>`), and the
--         type-changing `select @l` (`{ value :: Maybe a } → { value :: a }`);
--       `×→×` displays — `progressBar` (`<sl-progress-bar>`,
--         `{ value :: Number } → {}`, the filled fraction 0–1);
--       `×→+` events — `button @l` (`<sl-button variant="primary">`);
--       `+→×` statuses — `toast @l` (`<sl-alert>` shown on feed,
--         auto-dismissing via its own `duration`) — canonical
--         `[ event :: String ]` in, adopted via `# forCase @l`.
--   * **oculars** — shape-preserving decorators: `card { caption }`
--     (`<sl-card>` with a header slot). Typography is deliberately absent:
--     Shoelace styles plain HTML through its tokens, so the `PUI.Web.HTML`
--     element oculars are the typography.
--   * plus **announcing statics** (`{} → {}` chrome with a face):
--     `divider` (`<sl-divider>`).
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (`rmap` a total
-- `Model -> Model` after `completed`), not in a leaf bracket.

-- UIs

-- | The **primary button**: the screen's action. It reports on click,
-- | carrying the data it was showing, under the name the app gives the
-- | action — `button { label: "Submit" } # asCase @"submitted"`.
button :: forall r. { label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
button config = recordToCase @"clicked" $ eventLeaf $
  el "sl-button" >>> "variant" := "primary" $ staticText config.label

-- the click-emitter protocol over any `{} → {}` element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld)
eventLeaf :: forall a. PUI Web {} {} -> PUI Web a a
eventLeaf chrome = clicked (chrome # constantly {})

-- | The **text field**: a labelled single-line input. Shows the string it
-- | is given and reports each edit; typing is never interrupted by values
-- | arriving from elsewhere. Attach it to a field of the model with
-- | `# asField @l`.
textField :: { label :: String } -> PUI Web { value :: String } { value :: String }
textField config = field @"value" $ wrap do
  -- focus-guarded like `Web.input`: model updates never clobber the field
  -- being typed in (the shadow input keeps the host as `activeElement`),
  -- but still echo so merge gates keep flowing
  element "sl-input" (pure unit)
  attribute "label" config.label
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \newa -> do
        focused <- isFocused node
        unless focused $ setValue node newa
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "sl-input" node $ const do
          value <- getValue node
          prop value
    }

-- | The **multi-line text field**, `rows` lines tall — a note, a review, a
-- | message. Otherwise `textField`.
textArea :: { label :: String, rows :: Int } -> PUI Web { value :: String } { value :: String }
textArea config = field @"value" $ wrap do
  element "sl-textarea" (pure unit)
  attribute "label" config.label
  attribute "rows" (show config.rows)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \newa -> do
        focused <- isFocused node
        unless focused $ setValue node newa
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "sl-input" node $ const do
          value <- getValue node
          prop value
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
rating :: { label :: String } -> PUI Web { value :: { current :: Number, max :: Int } } { value :: { current :: Number, max :: Int } }
rating config = field @"value" $
  div >>> "style" := "display: inline-flex; flex-direction: column; gap: var(--sl-spacing-3x-small);" $ wrap do
    _ <- unwrap (span >>> "style" := "font-size: var(--sl-input-label-font-size-medium); color: var(--sl-input-label-color);" $ staticText config.label)
    element "sl-rating" (pure unit)
    attribute "label" config.label
    node <- gets _.sibling
    mPropRef <- liftEffect $ Ref.new Nothing
    qRef <- liftEffect $ Ref.new Nothing
    liftEffect $ listenNode node "sl-change" do
      v <- getNumberProp "value" node
      mq <- Ref.read qRef
      for_ mq \q -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop (q { current = v })
    pure
      { toUser: \q -> do
          Ref.write (Just q) qRef
          setAttribute node "max" (show q.max)
          setNumberProp "value" node q.current
          -- leaf echo: announce what was received, so record-merge gates open
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop q
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }

-- | The **slider**: a quantity chosen by feel, where the range matters more
-- | than the exact number.
-- |
-- | The range is part of the quantity, not part of the screen:
-- | `{ current, min, max, step }` travels together as one business datum, so
-- | limits come from the data and can change while the app runs — a slider
-- | is never silently out of range, and a range nobody supplied is a
-- | compile error rather than a wrong screen. A `step` makes it discrete,
-- | no step continuous.
-- |
-- | It reports on **every change**, following the drag — so whatever it
-- | drives should be cheap to redo, or be `debounced` downstream. The
-- | current number shows in the control's own tooltip while dragging.
sliderLive :: { label :: String } -> PUI Web { value :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } { value :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
sliderLive config = field @"value" $ wrap do
  element "sl-range" (pure unit)
  attribute "label" config.label
  attribute "style" "width: 100%; min-width: 240px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  qRef <- liftEffect $ Ref.new Nothing
  -- the value setter could fire input too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "sl-input" do
    busy <- Ref.read busyRef
    unless busy do
      v <- getNumberProp "value" node
      mq <- Ref.read qRef
      for_ mq \q -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop (q { current = v })
  pure
    { toUser: \q -> do
        Ref.write (Just q) qRef
        Ref.write true busyRef
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        case q.step of
          Just s -> setAttribute node "step" (show s)
          Nothing -> removeAttribute node "step"
        setNumberProp "value" node q.current
        Ref.write false busyRef
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop q
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | The label sits beside it and is part of the target, so clicking the
-- | words toggles it too.
toggleSwitch :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
toggleSwitch config = field @"value" $ wrap do
  element "sl-switch" (void $ unwrap (staticText config.label))
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ listenNode node "sl-change" do
    b <- getChecked node
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop b
  pure
    { toUser: \b -> do
        setChecked node b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The **dropdown**: one choice out of a list too long to lay out in the
-- | open. Until the user picks there is nothing to show, so the field
-- | arrives as "maybe a choice" and leaves as the choice itself — say which
-- | with `# optional` or `# required`. The options belong to the control,
-- | not to the model.
select :: forall a. Eq a => { label :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
select config options = field @"value" $ wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic selection could fire change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "sl-change" do
    busy <- Ref.read busyRef
    unless busy do
      picked <- getValue node
      for_ (fromString picked >>= (options !! _)) \o -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop o.value
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setValue node (show idx)
          Nothing -> setValue node ""
        Ref.write false busyRef
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup =
    "<sl-select label=\"" <> config.label <> "\" style=\"min-width: 240px;\">"
      <> foldMapWithIndex optionMarkup options
      <> "</sl-select>"
  optionMarkup idx o = "<sl-option value=\"" <> show idx <> "\">" <> o.label <> "</sl-option>"

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a quota, a share, a
-- | rating out of five — written as `progressBar # projected fraction`,
-- | with the business function deciding what the fraction means.
progressBar :: PUI Web { value :: Number } {}
progressBar = wrap do
  element "sl-progress-bar" (pure unit)
  attribute "style" "width: 100%; min-width: 200px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        -- sl-progress-bar runs 0–100
        setNumberProp "value" node (r.value * 100.0)
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | The **toast**: a brief message at the bottom of the screen that
-- | dismisses itself, for something that has just happened and needs no
-- | reply. It never interrupts.
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the toast is built — `toast # forCase @"submitted" thanksLine` — and
-- | let the event carry the bare facts.
toast :: PUI Web [ event :: String ] {}
toast = wrap do
  w <- unwrap $ el "sl-alert" >>> "variant" := "primary" >>> "duration" := "5000" >>> "closable" := ""
    >>> "style" := "position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000; min-width: 300px;" $ wrap do
    _ <- unwrap (el "sl-icon" >>> "slot" := "icon" >>> "name" := "check2-circle" $ staticText "")
    unwrap (text # projected eventText)
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        showAlert node
    , fromUser: w.fromUser
    }

-- UIOculars

-- | A **card**: a surface holding one subject's content, captioned in its
-- | header. The body stacks its children with even spacing, so a form or a
-- | summary can be dropped in without spacing each row by hand.
card :: { caption :: String } -> Ocular (PUI Web)
card config content = el "sl-card" $ wrap do
  _ <- unwrap (div >>> "slot" := "header" >>> "style" := "font-weight: var(--sl-font-weight-semibold);" $ staticText config.caption)
  unwrap (div >>> "style" := "display: flex; flex-direction: column; align-items: flex-start; gap: var(--sl-spacing-medium);" $ content)

-- announcing statics ({} → {} chrome with a face)

-- | A **divider**: the hairline rule between sections of a surface. Fixed
-- | decoration, carrying no data.
divider :: PUI Web {} {}
divider = staticHTML "<sl-divider style=\"width: 100%;\"></sl-divider>"

-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = Variant.on (Proxy @"event") identity Variant.case_

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import listenNode :: Node -> String -> Effect Unit -> Effect Unit
foreign import showAlert :: Node -> Effect Unit
