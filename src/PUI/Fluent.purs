-- Fluent UI (https://fluent2.microsoft.design) components implemented as
-- PUI Web/Ocular (PUI Web) datatypes — a design-system vocabulary beside
-- `PUI.MDC2`/`PUI.MDC3`/`PUI.Shoelace`, proving the vocabularies
-- interchangeable: built on Microsoft's official `@fluentui/web-components`
-- v3 custom elements (`<fluent-button>`, `<fluent-slider>`, ...),
-- registered by importing the FFI module, so a component leaf is just
-- `element "fluent-..."` plus property/event wiring — exactly the
-- `PUI.MDC3` recipe, and the leaf-echo protocols are the same
-- (focus-guarded text field, per-feed display echo, `Just`-only echo on
-- the type-changing selectors). Fluent associates labels through
-- `<fluent-field>`, so the labeled editors carry that wrapper as chrome.
-- Two-sorted, same citizenship, and — where the concept exists in both
-- catalogs — the same names and signatures:
--
--   * **components** — widgets with a model interface, every one a citizen
--     of exactly one row direction:
--       `×→×` editors — `textField @l`, `toggleSwitch @l`
--         (`<fluent-switch>`), `slider @l` (`<fluent-slider>` — Fluent's
--         slider emits on every value change; the catalog has no
--         commit/live split), and the type-changing `dropdown @l` and
--         `radioGroup @l` (`{ value :: Maybe a } → { value :: a }`);
--       `×→×` displays — `progressBar` (`{ value :: Number } → {}`, the
--         filled fraction 0–1) and `ratingDisplay`
--         (`{ value :: Number } → {}` — Fluent's read-only star display,
--         honest about MD's missing counterpart: a rating *editor* is not
--         in the Fluent catalog);
--       `×→+` events — `button @l` (`<fluent-button appearance="primary">`);
--       `+→×` statuses — `messageBar @l` (`<fluent-message-bar
--         intent="success">` shown on feed, auto-dismissing) — canonical
--         `[ event :: String ]` in, adopted via `# forCase @l`.
--   * **oculars** — shape-preserving decorators: `card { caption }`
--     (hand-rolled over the `--colorNeutral*`/`--shadow*` tokens — the
--     Fluent card is a React-only catalog entry) and the type-ramp
--     typography over `<fluent-text>`: `title3`, `body1`, `caption1`.
--   * plus **announcing statics** (`{} → {}` chrome with a face):
--     `divider` (`<fluent-divider>`).
--
-- Page requirements: none — the design tokens are set globally from the
-- official web light theme at load (`setTheme(webLightTheme)` in the FFI
-- module), and Fluent's type ramp rides the system font stack (Segoe UI
-- where available).
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (`rmap` a total
-- `Model -> Model` after `completed`), not in a leaf bracket.
module PUI.Fluent
  ( body1
  , button
  , caption1
  , card
  , divider
  , dropdown
  , messageBar
  , progressBar
  , radioGroup
  , ratingDisplay
  , slider
  , textField
  , title3
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
import Data.Number.Format (toString)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.TraversableWithIndex (forWithIndex)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (PUI, constantly)
import PUI.HTML (cl, clicked, div, el, staticHTML, staticText, text, (:=))
import PUI.Web (Node, Web, addEventListener, attribute, element, getChecked, getValue, setAttribute, setChecked, setValue)
import Type.Proxy (Proxy(..))

-- UIs

-- | The `×→+` event button (`<fluent-button appearance="primary">`): reads
-- | the whole record it is shown and fires it as event case `l` on click.
button :: forall r. { label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
button config = recordToCase @"clicked" $ eventLeaf $
  el "fluent-button" >>> "appearance" := "primary" $ staticText config.label

-- the click-emitter protocol over any `{} → {}` element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld)
eventLeaf :: forall a. PUI Web {} {} -> PUI Web a a
eventLeaf chrome = clicked (chrome # constantly {})

-- a `<fluent-field>` associating a label with the editor its builder
-- appends (Fluent's label protocol: both are slotted children of the field)
fieldWith :: forall i o. String -> String -> Web { toUser :: i -> Effect Unit, fromUser :: (o -> Effect Unit) -> Effect Unit } -> PUI Web i o
fieldWith position lbl editor = el "fluent-field" >>> "label-position" := position $ wrap do
  _ <- unwrap (el "fluent-label" >>> "slot" := "label" $ staticText lbl)
  editor

-- | The Fluent text input, a `{ value :: String }` editor. Focus-guarded
-- | like `Web.input`: model updates never clobber the field being typed in
-- | (Fluent keeps the real `<input>` in the light DOM, so the guard checks
-- | containment), but still echo so merge gates keep flowing.
textField :: { label :: String } -> PUI Web { value :: String } { value :: String }
textField config = field @"value" $ fieldWith "above" config.label do
  element "fluent-text-input" (pure unit)
  attribute "slot" "input"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \newa -> do
        focused <- containsFocus node
        unless focused $ setValue node newa
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "input" node $ const do
          value <- getValue node
          prop value
    }

-- | The Fluent switch, a `×→×` `Boolean` editor; the label associates
-- | through the field wrapper, after the control.
toggleSwitch :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
toggleSwitch config = field @"value" $ fieldWith "after" config.label do
  element "fluent-switch" (pure unit)
  attribute "slot" "input"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ listenNode node "change" do
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

-- | The `×→×` `Number` editor (`<fluent-slider>`). Emits on every value
-- | change — Fluent's catalog has no commit/live split, so each drag step
-- | is an emission (wrap a consuming stage in `debounced` if it minds).
-- | The label line carries a live numeric readout (the element has no
-- | value indicator of its own — the counterpart of MD's labeled handle),
-- | fed from the channel, so it follows drags through the loop.
slider :: { label :: String, min :: Number, max :: Number, step :: Number } -> PUI Web { value :: Number } { value :: Number }
slider config = field @"value" $ el "fluent-field" >>> "label-position" := "above" $ wrap do
  readout <- unwrap $ (el "fluent-label" >>> "slot" := "label" >>> "style" := "display: flex; justify-content: space-between; width: 100%;" $ wrap do
      _ <- unwrap (staticText config.label)
      unwrap (el "span" >>> "style" := "color: var(--colorNeutralForeground3, #616161);" $ text))
  -- the readout is written, never listened to; text's echo needs a listener
  liftEffect $ readout.fromUser \_ -> pure unit
  element "fluent-slider" (pure unit)
  attribute "slot" "input"
  attribute "min" (show config.min)
  attribute "max" (show config.max)
  attribute "step" (show config.step)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  -- the value setter fires change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "change" do
    busy <- Ref.read busyRef
    unless busy do
      v <- getNumberProp "valueAsNumber" node
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop v
  pure
    { toUser: \v -> do
        Ref.write true busyRef
        setNumberProp "valueAsNumber" node v
        Ref.write false busyRef
        readout.toUser { value: toString v }
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop v
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The Fluent dropdown, a `×→×` editor. Type-changing like `PUI.MDC2`'s
-- | `select @l`: the input field holds the selection state (`Maybe a`),
-- | the output field the bare selection (`a`). Options are design-system
-- | config.
dropdown :: forall a. Eq a => { label :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
dropdown config options = field @"value" $ fieldWith "above" config.label do
  element "fluent-dropdown" (void $ unwrap (staticHTML optionsMarkup))
  attribute "slot" "input"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic selection could fire change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "change" do
    busy <- Ref.read busyRef
    unless busy do
      picked <- getStringProp "value" node
      for_ (fromString picked >>= (options !! _)) \o -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop o.value
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> selectDropdownOption node (show idx)
          Nothing -> selectDropdownOption node ""
        Ref.write false busyRef
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  optionsMarkup = "<fluent-listbox>" <> foldMapWithIndex optionMarkup options <> "</fluent-listbox>"
  optionMarkup idx o = "<fluent-option value=\"" <> show idx <> "\">" <> o.label <> "</fluent-option>"

-- | The Fluent radio group, a `×→×` editor. Type-changing like `dropdown
-- | @l`; one `<fluent-radio>` per option, each labeled through its own
-- | field wrapper (Fluent's documented pattern), exclusivity from the
-- | group.
radioGroup :: forall a. Eq a => { label :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
radioGroup config options = field @"value" $ fieldWith "above" config.label do
  members <- element "fluent-radio-group" do
    forWithIndex options \idx o -> do
      member <- element "fluent-field" do
        _ <- unwrap (el "fluent-label" >>> "slot" := "label" $ staticText o.label)
        element "fluent-radio" (pure unit)
        radioNode <- gets _.sibling
        liftEffect do
          setAttribute radioNode "slot" "input"
          setAttribute radioNode "value" (show idx)
        pure { radioNode, value: o.value }
      fieldNode <- gets _.sibling
      liftEffect $ setAttribute fieldNode "label-position" "after"
      pure member
  groupNode <- gets _.sibling
  liftEffect $ setAttribute groupNode "slot" "input"
  mPropRef <- liftEffect $ Ref.new Nothing
  -- listen per radio (the group's own sync can lag); the model echo below
  -- re-feeds through the group's value setter, which restores exclusivity
  liftEffect $ for_ members \m -> listenNode m.radioNode "change" do
    checked <- getChecked m.radioNode
    when checked do
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop m.value
  pure
    { toUser: \ma -> do
        -- the group's value setter is the sanctioned selection path, and it
        -- needs the group bound (its bind-time sync would clear earlier
        -- per-radio writes)
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> selectGroupValue groupNode (show idx)
          Nothing -> selectGroupValue groupNode ""
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The **determinate** progress display, a `{ value :: Number } → {}`
-- | display citizen: `value` is the filled fraction (0.0–1.0). The gauge
-- | shape: `progressBar # projection fraction`.
progressBar :: PUI Web { value :: Number } {}
progressBar = wrap do
  element "fluent-progress-bar" (pure unit)
  attribute "max" "1"
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setNumberProp "value" node r.value
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | The read-only star display (`<fluent-rating-display>`), a
-- | `{ value :: Number } → {}` display citizen.
ratingDisplay :: PUI Web { value :: Number } {}
ratingDisplay = wrap do
  element "fluent-rating-display" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setNumberProp "value" node r.value
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | The `+→×` status receiver: shows message case `l` in a
-- | `<fluent-message-bar intent="success">` fixed at the bottom, shown on
-- | every feed and auto-dismissing after 5s (re-feeding resets the timer).
-- | Contributes no fields (`text` echoes its `{}`, so it announces).
messageBar :: PUI Web [ event :: String ] {}
messageBar = wrap do
  liftEffect $ ensureStyle "fluent-toast" toastCss
  w <- unwrap $ (el "fluent-message-bar" >>> "intent" := "success" $
    lcmap (\v -> { value: Variant.on (Proxy @"event") identity Variant.case_ v }) text) # cl "fluent-toast"
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        autoDismiss node "fluent-toast--open" 5000
    , fromUser: w.fromUser
    }

toastCss :: String
toastCss = """
.fluent-toast { position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000; visibility: hidden; opacity: 0; transition: opacity .15s; }
.fluent-toast--open { visibility: visible; opacity: 1; }
"""

-- UIOculars

-- the Fluent type ramp over <fluent-text>

title3 :: Ocular (PUI Web)
title3 w = el "fluent-text" >>> "size" := "500" >>> "weight" := "semibold" >>> "block" := "" $ w

body1 :: Ocular (PUI Web)
body1 w = el "fluent-text" >>> "size" := "300" >>> "block" := "" $ w

caption1 :: Ocular (PUI Web)
caption1 w = el "fluent-text" >>> "size" := "200" >>> "block" := "" $ w

-- | A card with a caption — hand-rolled chrome over the Fluent tokens (the
-- | Fluent card is a React-only catalog entry), a flex column supplying
-- | the vertical rhythm between its children.
card :: { caption :: String } -> Ocular (PUI Web)
card config content = wrap do
  liftEffect $ ensureStyle "fluent-card" cardCss
  unwrap $ div >>> "class" := "fluent-card" $ wrap do
    _ <- unwrap (el "fluent-text" >>> "size" := "500" >>> "weight" := "semibold" >>> "block" := "" $ staticText config.caption)
    unwrap content

cardCss :: String
cardCss = """
.fluent-card { background: var(--colorNeutralBackground1, #fff); color: var(--colorNeutralForeground1, #242424); font-family: var(--fontFamilyBase, 'Segoe UI', sans-serif); border-radius: var(--borderRadiusXLarge, 8px); box-shadow: var(--shadow4, 0 2px 4px rgba(0,0,0,.14)); padding: 20px; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; }
"""

-- announcing statics ({} → {} chrome with a face)

divider :: PUI Web {} {}
divider = staticHTML "<fluent-divider style=\"width: 100%;\"></fluent-divider>"

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import getStringProp :: String -> Node -> Effect String
foreign import selectDropdownOption :: Node -> String -> Effect Unit
foreign import selectGroupValue :: Node -> String -> Effect Unit
foreign import listenNode :: Node -> String -> Effect Unit -> Effect Unit
foreign import containsFocus :: Node -> Effect Boolean
foreign import ensureStyle :: String -> String -> Effect Unit
foreign import autoDismiss :: Node -> String -> Int -> Effect Unit
