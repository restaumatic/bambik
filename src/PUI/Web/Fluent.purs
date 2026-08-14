-- | The **Fluent UI** vocabulary (https://fluent2.microsoft.design), over
-- | Microsoft's `@fluentui/web-components` — a non-Material design system
-- | whose names and signatures match the Material modules wherever both
-- | catalogues have the concept, so a screen changes design system by
-- | changing this one import. Fluent's own entry is `ratingDisplay`, a
-- | rating that is shown but not edited: the catalogue has no star
-- | *editor*, and this vocabulary does not invent one.
-- |
-- | **The page needs nothing**: the theme is applied from the bundle at
-- | load, and Fluent's type ramp rides the system font stack.
-- |
-- | The catalogue: `textField`, `toggleSwitch` and `slider` to enter
-- | values, `dropdown` and `radioGroup` to choose one, `button` to act,
-- | `messageBar` to say what happened, `progressBar` and `ratingDisplay` to
-- | show a figure, and `card`, `divider` and the type ramp (`title3`,
-- | `body1`, `caption1`) for structure.
module PUI.Web.Fluent
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
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Number.Format (toString)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.TraversableWithIndex (forWithIndex)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (Ocular, PUI, projected)
import PUI.Web.HTML (cl, clicked, div, el, staticText, text, (:=))
import PUI.Web (Node, Web, staticHTML, addEventListener, attribute, element, getChecked, getValue, removeAttribute, setAttribute, setChecked, setValue)
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons, class Lacks)
import Data.Symbol (class IsSymbol)
import Record (get) as Record

-- Implementation notes — the reference above is the contract.
--
-- Fluent UI (https://fluent2.microsoft.design) components implemented as
-- PUI Web/Ocular (PUI Web) datatypes — a design-system vocabulary beside
-- `PUI.Web.MDC2`/`PUI.Web.MDC3`/`PUI.Web.Shoelace`, proving the vocabularies
-- interchangeable: built on Microsoft's official `@fluentui/web-components`
-- v3 custom elements (`<fluent-button>`, `<fluent-slider>`, ...),
-- registered by importing the FFI module, so a component leaf is just
-- `element "fluent-..."` plus property/event wiring — exactly the
-- `PUI.Web.MDC3` recipe, and the leaf-echo protocols are the same
-- (focus-guarded text field, per-feed display echo, `Just`-only echo on
-- the type-changing selectors). Fluent associates labels through
-- `<fluent-field>`, so the labeled editors carry that wrapper as chrome.
-- Two-sorted, same citizenship, and — where the concept exists in both
-- catalogs — the same names and signatures:
--
--   * **components** — UI components with a model interface, every one a citizen
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
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (`rmap` a total
-- `Model -> Model` after `completed`), not in a leaf bracket.

-- UIs

-- | The **primary button**: the screen's action. It reports on click,
-- | carrying the data it was showing, under the name the app gives the
-- | action — `button { label: "Book" } # asCase @"clicked" @"booked"`.
button :: forall r. { label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
button config = recordToCase @"clicked" $ eventLeaf $
  el "fluent-button" >>> "appearance" := "primary" $ staticText config.label

-- the click-emitter protocol over any `{} → {}` element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld)
eventLeaf :: forall r. PUI Web {} {} -> PUI Web { | r } { | r }
eventLeaf chrome = clicked chrome

-- a `<fluent-field>` associating a label with the editor its builder
-- appends (Fluent's label protocol: both are slotted children of the field)
fieldWith :: forall i o. String -> String -> Web { toUser :: i -> Effect Unit, fromUser :: (o -> Effect Unit) -> Effect Unit } -> PUI Web i o
fieldWith position lbl editor = el "fluent-field" >>> "label-position" := position $ wrap do
  _ <- unwrap (el "fluent-label" >>> "slot" := "label" $ staticText lbl)
  editor

-- | The **text field**: a labelled single-line input. Shows the string it
-- | is given and reports each edit; typing is never interrupted by values
-- | arriving from elsewhere. Attach it to a field of the model with
-- | `# asField @l`.
textField :: forall @l r. IsSymbol l => Lacks l () => Cons l String () r => { label :: String } -> PUI Web { | r } { | r }
textField config = field @l $ fieldWith "above" config.label do
  -- focus-guarded like `Web.input`: model updates never clobber the field
  -- being typed in (Fluent keeps the real `<input>` in the light DOM, so
  -- the guard checks containment), but still echo so merge gates flow
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

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | Its label sits after the control, in Fluent's manner.
toggleSwitch :: forall @l r. IsSymbol l => Lacks l () => Cons l Boolean () r => { label :: String } -> PUI Web { | r } { | r }
toggleSwitch config = field @l $ fieldWith "after" config.label do
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
-- | It reports on **every change**, following the drag — Fluent has no
-- | commit-only slider — so whatever it drives should be cheap to redo, or
-- | be `debounced` downstream. The current number is shown at the end of
-- | the label line, since the control has no readout of its own.
slider :: forall @l r. IsSymbol l => Lacks l () => Cons l { current :: Number, min :: Number, max :: Number, step :: Maybe Number } () r => { label :: String } -> PUI Web { | r } { | r }
slider config = field @l $ el "fluent-field" >>> "label-position" := "above" $ wrap do
  readout <- unwrap $ (el "fluent-label" >>> "slot" := "label" >>> "style" := "display: flex; justify-content: space-between; width: 100%;" $ wrap do
      _ <- unwrap (staticText config.label)
      unwrap (el "span" >>> "style" := "color: var(--colorNeutralForeground3, #616161);" $ text @"value"))
  -- the readout is written, never listened to; text's echo needs a listener
  liftEffect $ readout.fromUser \_ -> pure unit
  element "fluent-slider" (pure unit)
  attribute "slot" "input"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  qRef <- liftEffect $ Ref.new Nothing
  -- the value setter fires change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "change" do
    busy <- Ref.read busyRef
    unless busy do
      v <- getNumberProp "valueAsNumber" node
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
        setNumberProp "valueAsNumber" node q.current
        Ref.write false busyRef
        readout.toUser { value: toString q.current }
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop q
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The **dropdown**: one choice out of a list too long to lay out in the
-- | open. Until the user picks there is nothing to show, so the field
-- | arrives as "maybe a choice" and leaves as the choice itself — say which
-- | with `# optional` (nothing preselected, and whatever needs the choice
-- | stays hidden until it exists) or `# required @"value"`. The options belong to
-- | the control, not to the model.
dropdown :: forall @l a ri ro. IsSymbol l => Lacks l () => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => { label :: String } -> Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
dropdown config options = field @l $ fieldWith "above" config.label do
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

-- | The **radio group**: one choice among a handful, every option visible
-- | and comparable at a glance. Beyond about five options use `dropdown`.
-- | Same picked/unpicked contract as `dropdown`.
radioGroup :: forall @l a ri ro. IsSymbol l => Lacks l () => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => { label :: String } -> Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
radioGroup config options = field @l $ fieldWith "above" config.label do
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

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a quota, a share, a
-- | rating out of five — written as `progressBar # projected @"value" fraction`,
-- | with the business function deciding what the fraction means.
progressBar :: forall @l r. IsSymbol l => Cons l Number () r => PUI Web { | r } {}
progressBar = wrap do
  element "fluent-progress-bar" (pure unit)
  attribute "max" "1"
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setNumberProp "value" node (Record.get (Proxy @l) r)
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | A **read-only star rating**: someone else's score, shown but not
-- | editable — Fluent's catalog has the display and not the editor, so
-- | there is deliberately no star *editor* here (Shoelace's `rating` is
-- | the one).
ratingDisplay :: forall @l r. IsSymbol l => Cons l Number () r => PUI Web { | r } {}
ratingDisplay = wrap do
  element "fluent-rating-display" (pure unit)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setNumberProp "value" node (Record.get (Proxy @l) r)
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | The **message bar**: a brief success message at the bottom of the
-- | screen that dismisses itself after a few seconds, for something that
-- | has just happened and needs no reply. It never interrupts.
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the message bar is built — `messageBar # forCase @"event" @"booked" bookedLine`
-- | — and let the event carry the bare facts.
messageBar :: PUI Web [ event :: String ] {}
messageBar = wrap do
  liftEffect $ ensureStyle "fluent-toast" toastCss
  w <- unwrap $ (el "fluent-message-bar" >>> "intent" := "success" $
    text @"value" # projected @"value" eventText) # cl "fluent-toast"
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

-- | A **title**: the heading of a screen region or a card, in Fluent's
-- | semibold title size.
title3 :: Ocular (PUI Web)
title3 w = el "fluent-text" >>> "size" := "500" >>> "weight" := "semibold" >>> "block" := "" $ w

-- | **Body** text — the default for a line or a paragraph the user reads.
body1 :: Ocular (PUI Web)
body1 w = el "fluent-text" >>> "size" := "300" >>> "block" := "" $ w

-- | A **caption**: the smallest type, for annotations and fine print
-- | beside the content.
caption1 :: Ocular (PUI Web)
caption1 w = el "fluent-text" >>> "size" := "200" >>> "block" := "" $ w

-- | A **card**: a surface holding one subject's content, captioned at the
-- | top. It stacks its children with even spacing, so a form or a summary
-- | can be dropped in without spacing each row by hand.
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

-- | A **divider**: the hairline rule between sections of a surface. Fixed
-- | decoration, carrying no data.
divider :: PUI Web {} {}
divider = staticHTML "<fluent-divider style=\"width: 100%;\"></fluent-divider>"

-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = Variant.on (Proxy @"event") identity Variant.case_

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
