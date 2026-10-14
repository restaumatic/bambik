-- | The **Fluent UI** vocabulary (https://fluent2.microsoft.design), over
-- | Microsoft's `@fluentui/web-components` — a non-Material design system
-- | whose names and signatures match the Material modules wherever both
-- | catalogues have the concept, so a screen changes design system by
-- | changing this one import. Fluent's own entry is `ratingDisplay`, a
-- | rating that is shown but not edited: the catalogue has no star
-- | *editor*, and this vocabulary does not invent one.
-- |
-- | **The page needs nothing**: the theme is applied from the bundle by this
-- | vocabulary's `body` at mount, and Fluent's type ramp rides the system
-- | font stack.
-- |
-- | The catalogue: `textField`, `toggleSwitch` and `slider` to enter
-- | values, `dropdown` and `radioGroup` to choose one, `button` to act,
-- | `messageBar` to say what happened, `progressBar` and `ratingDisplay` to
-- | show a figure, `card`, `divider` and the type ramp (`title3`, `body1`,
-- | `caption1`) for structure, and `body` to mount the app.
module PUI.Web.Fluent
  ( body
  , body1
  , button
  , caption1
  , card
  , divider
  , dropdown
  , dropdownUnpicked
  , dropdownOptional
  , messageBar
  , indeterminateLinearProgress
  , progressBar
  , radioGroup
  , radioGroupUnpicked
  , radioGroupOptional
  , ratingDisplay
  , slider
  , textField
  , title3
  , toggleSwitch
  ) where

import Data.Profunctor.Row.Structural (withStructuralEq)
import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.Array (findIndex)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foldable (for_, traverse_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Number.Format (toString)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row (atCase)
import Data.TraversableWithIndex (forWithIndex)
import Data.Variant (match) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (folding, Ocular, PUI)
import PUI.Web.HTML (div)
import PUI.Web.HTML (body) as HTML
import PUI.Web (clearedOnRepress, selectedAt, selectedOptionalAt, selectedUnpickedAt, Node, Web, attribute, autoDismiss, cl, displayWiring, editorWiring, el, element, ensureStyle, escaped, eventLeaf, getChecked, getValue, listenNode, removeAttribute, selectorWiring, setAttribute, setChecked, setValue, staticHTML, staticText, statusWiring, text, statusText, separating, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)

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
-- (focus-guarded text field, per-feed display echo, selection
-- editors answering every feed with their row). Fluent associates labels through
-- `<fluent-field>`, so the labeled editors carry that wrapper as chrome.
-- Two-sorted, same citizenship, and — where the concept exists in both
-- catalogs — the same names and signatures:
--
--   * **components** — UI components with a model interface, every one a citizen
--     of exactly one row shape:
--       `×→×` editors — `textField @l`, `toggleSwitch @l`
--         (`<fluent-switch>`), `slider @l` (`<fluent-slider>` — Fluent's
--         slider emits on every value change; the catalog has no
--         commit/live split), and the type-changing `dropdown @l` and
--         `radioGroup @l` (`{ l :: f } → { l :: f }`, `f` the option itself, or a
--         variant around it for the `…Unpicked`/`…Optional` siblings);
--       `×→×` displays — `progressBar` (`{ value :: Number } → {}`, the
--         filled fraction 0–1) and `ratingDisplay`
--         (`{ value :: Number } → {}` — Fluent's read-only star display,
--         honest about MD's missing counterpart: a rating *editor* is not
--         in the Fluent catalog);
--       `×→+` events — `button @l` (`<fluent-button appearance="primary">`);
--       `+→×` statuses — `messageBar` (`<fluent-message-bar
--         intent="success">` shown on feed, auto-dismissing) — each taking
--         its business case and fold, the case label its copy (`messageBar @"Room booked" identity`).
--   * **oculars** — shape-preserving decorators: `card`
--     (hand-rolled over the `--colorNeutral*`/`--shadow*` tokens — the
--     Fluent card is a React-only catalog entry) and the type-ramp
--     typography over `<fluent-text>`: `title3`, `body1`, `caption1`.
--   * plus **announcing statics** (`{}`-output chrome with a face, at any row):
--     `divider` (`<fluent-divider>`).
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (a `settled`
-- normalization on the whole-row stage), not in a leaf bracket.

-- UIs

-- | The **primary button**: the screen's action. It reports on click,
-- | carrying the data it was showing, under the name the app gives the
-- | action — `button @"Book the room"`. The label defaults to
-- | the case label verbatim (`label:` overrides with real copy).
button :: forall @l r v. IsSymbol l => Cons l { | r } () v => PUI Web { | r } [ | v ]
button = let config = { label: reflectSymbol (Proxy @l) } :: { label :: String } in eventLeaf @l $
  el "fluent-button" >>> "appearance" := "primary" $ staticText config.label

-- a `<fluent-field>` associating a label with the editor its builder
-- appends (Fluent's label protocol: both are slotted children of the field)
fieldWith :: forall i o. String -> String -> Web { toUser :: i -> Effect Unit, fromUser :: (o -> Effect Unit) -> Effect Unit } -> PUI Web i o
fieldWith position lbl editor = el "fluent-field" >>> "label-position" := position $ wrap do
  _ <- unwrap (el "fluent-label" >>> "slot" := "label" $ staticText lbl)
  editor

-- | The **text field**: a labelled single-line input. Shows the string it
-- | is given and reports each edit; typing is never interrupted by values
-- | arriving from elsewhere. A whole-row citizen: fed the wide row, it
-- | edits field `l` and carries the rest.
textField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textField = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ fieldWith "above" config.label do
  -- Fluent keeps the real `<input>` in the light DOM, so the focus guard
  -- checks containment
  element "fluent-text-input" (pure unit)
  attribute "slot" "input"
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: setValue node
    , held: containsFocus node
    , reports: listenNode node "input"
    , read: \_ -> getValue node
    }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | Its label sits after the control, in Fluent's manner.
toggleSwitch :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
toggleSwitch = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ fieldWith "after" config.label do
  element "fluent-switch" (pure unit)
  attribute "slot" "input"
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: setChecked node
    , held: pure false
    , reports: listenNode node "change"
    , read: \_ -> getChecked node
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
-- | It reports on **every change**, following the drag — Fluent has no
-- | commit-only slider — so whatever it drives should be cheap to redo, or
-- | be `debounced` downstream. The current number is shown at the end of
-- | the label line, since the control has no readout of its own.
slider :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
slider = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ el "fluent-field" >>> "label-position" := "above" $ wrap do
  readout <- unwrap $ (el "fluent-label" >>> "slot" := "label" >>> "style" := "display: flex; justify-content: space-between; width: 100%;" $ wrap do
      _ <- unwrap (staticText config.label)
      unwrap (el "span" >>> "style" := "color: var(--colorNeutralForeground3, #616161);" $ text _.readout))
  -- the readout is written, never listened to; text's echo needs a listener
  liftEffect $ readout.fromUser \_ -> pure unit
  element "fluent-slider" (pure unit)
  attribute "slot" "input"
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: \q -> do
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        Variant.match { discrete: \s -> setAttribute node "step" (show s), continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "valueAsNumber" node q.current
        readout.toUser { readout: toString q.current }
    , held: pure false
    , reports: listenNode node "change"
    , read: \q -> getNumberProp "valueAsNumber" node <#> \v -> q { current = v }
    }

-- | The **dropdown**: one choice out of a list too long to lay out in the
-- | open.
-- | The field holds the option itself, so the model always has one.
-- | Two siblings hold a variant instead: `dropdownUnpicked @l @c` for a
-- | choice owed but not yet made, `dropdownOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- | The options belong to the control, not to the model.
dropdown :: forall @l a b r. IsSymbol l => Cons l a b r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
dropdown options = withStructuralEq @a (dropdownOf @l false (selectedAt @l) options)

-- | `dropdown` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
dropdownUnpicked :: forall @l @c a b1 v b2 r. IsSymbol l => IsSymbol c => Cons c a b1 v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
dropdownUnpicked options = withStructuralEq @a (dropdownOf @l false (selectedUnpickedAt @l @c) options)

-- | `dropdown` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | an empty first option clears it, storing `n` again.
dropdownOptional :: forall @l @c @n a b1 t v b2 r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} t v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
dropdownOptional options = withStructuralEq @a (dropdownOf @l true (selectedOptionalAt @l @c @n) options)

dropdownOf :: forall @l a i o. IsSymbol l => Eq a => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> Array { value :: a, label :: String } -> PUI Web i o
dropdownOf clearable lift options = let config = { label: reflectSymbol (Proxy @l) } in lift $ "name" := reflectSymbol (Proxy @l) $ fieldWith "above" config.label do
  element "fluent-dropdown" (void $ unwrap (staticHTML optionsMarkup))
  attribute "slot" "input"
  node <- gets _.sibling
  liftEffect $ selectorWiring options
    { show: selectDropdownOption node <<< maybe "" show
    , reports: listenNode node "change"
    , read: fromString <$> getStringProp "value" node
    }
  where
  optionsMarkup = "<fluent-listbox>" <> (if clearable then "<fluent-option value=\"\"></fluent-option>" else "") <> foldMapWithIndex optionMarkup options <> "</fluent-listbox>"
  optionMarkup idx o = "<fluent-option value=\"" <> show idx <> "\">" <> escaped o.label <> "</fluent-option>"

-- | The **radio group**: one choice among a handful, every option visible
-- | and comparable at a glance. Beyond about five options use `dropdown`.
-- | Same selection contract as `dropdown`.
radioGroup :: forall @l a b r. IsSymbol l => Cons l a b r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
radioGroup options = withStructuralEq @a (radioGroupOf @l false (selectedAt @l) options)

-- | `radioGroup` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
radioGroupUnpicked :: forall @l @c a b1 v b2 r. IsSymbol l => IsSymbol c => Cons c a b1 v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
radioGroupUnpicked options = withStructuralEq @a (radioGroupOf @l false (selectedUnpickedAt @l @c) options)

-- | `radioGroup` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | pressing the checked option again clears it, storing `n` again.
radioGroupOptional :: forall @l @c @n a b1 t v b2 r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} t v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
radioGroupOptional options = withStructuralEq @a (radioGroupOf @l true (selectedOptionalAt @l @c @n) options)

radioGroupOf :: forall @l a i o. IsSymbol l => Eq a => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> Array { value :: a, label :: String } -> PUI Web i o
radioGroupOf clearable lift options = let config = { label: reflectSymbol (Proxy @l) } in lift $ "name" := reflectSymbol (Proxy @l) $ fieldWith "above" config.label do
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
      pure { press: fieldNode, click: member.radioNode, radioNode: member.radioNode, value: member.value }
  groupNode <- gets _.sibling
  liftEffect $ setAttribute groupNode "slot" "input"
  mPropRef <- liftEffect $ Ref.new Nothing
  selRef <- liftEffect $ Ref.new Nothing
  let
    -- the group's value setter is the sanctioned selection path, and it
    -- needs the group bound (its bind-time sync would clear earlier
    -- per-radio writes)
    render ma = do
      Ref.write ma selRef
      case ma of
        Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> selectGroupValue groupNode (show idx)
        Nothing -> selectGroupValue groupNode ""
    emit ma = Ref.read mPropRef >>= traverse_ (_ $ ma)
  -- listen per radio (the group's own sync can lag); the model echo below
  -- re-feeds through the group's value setter, which restores exclusivity
  liftEffect $ for_ members \m -> listenNode m.radioNode "change" do
    checked <- getChecked m.radioNode
    sel <- Ref.read selRef
    when (checked && sel /= Just m.value) do
      Ref.write (Just m.value) selRef
      emit (Just m.value)
  liftEffect $ when clearable $ clearedOnRepress selRef members (render Nothing *> emit Nothing)
  pure
    { toUser: \ma -> do
        render ma
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a quota, a share, a
-- | rating out of five — `progressBar progressFraction`: the
-- | value is a **read function** like every display's, since a fraction is
-- | derived (a ratio of source fields), not state.
progressBar
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
progressBar f = wrap do
  element "fluent-progress-bar" (pure unit)
  attribute "max" "1"
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  liftEffect $ displayWiring \r -> setNumberProp "value" node (f r)

-- | A **read-only star rating**: someone else's score, shown but not
-- | editable — Fluent's catalog has the display and not the editor, so
-- | there is deliberately no star *editor* here (Shoelace's `rating` is
-- | the one).
ratingDisplay
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
ratingDisplay f = wrap do
  element "fluent-rating-display" (pure unit)
  node <- gets _.sibling
  liftEffect $ displayWiring \r -> setNumberProp "value" node (f r)

-- | The **message bar**: a brief success message at the bottom of the
-- | screen that dismisses itself after a few seconds, for something that
-- | has just happened and needs no reply. It never interrupts.
-- |
-- | A status **tells its case**: its caption is the case label verbatim,
-- | so the outcome is named as what it tells (`messageBar @"Room booked"
-- | identity`) and the payload is read by nothing but the fold. A `+→×`
-- | leaf like `fold @l f`, it takes its fold of the payload.
messageBar
  :: forall @l a v r
   . IsSymbol l
  => Cons l a () v
  => (a -> { | r })
  -> PUI Web [ | v ] { | r }
messageBar f = messageBarFace (reflectSymbol (Proxy @l)) # atCase @l # folding @l f

messageBarFace :: forall a r. String -> PUI Web a { | r }
messageBarFace caseLabel = wrap do
  liftEffect $ ensureStyle "fluent-toast" toastCss
  w <- unwrap $ (el "fluent-message-bar" >>> "intent" := "success" $
    statusText caseLabel) # cl "fluent-toast"
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

-- | A **card**: a surface holding one subject's content. It stacks its
-- | children with even spacing, so a form or a summary can be dropped in
-- | without spacing each row by hand.
-- |
-- | A plain ocular, with no config of its own: a card is a *surface*, and a
-- | heading of its own is ordinary typography placed in its content
-- | (`title3`, `body1`).
card :: Ocular (PUI Web)
card content = wrap do
  liftEffect $ ensureStyle "fluent-card" cardCss
  unwrap $ div >>> "class" := "fluent-card" $ content

cardCss :: String
cardCss = """
.fluent-card { background: var(--colorNeutralBackground1, #fff); color: var(--colorNeutralForeground1, #242424); font-family: var(--fontFamilyBase, 'Segoe UI', sans-serif); border-radius: var(--borderRadiusXLarge, 8px); box-shadow: var(--shadow4, 0 2px 4px rgba(0,0,0,.14)); padding: 20px; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; }
"""

-- statics (chrome with a face, the wire at any row)

-- | A **divider**: the hairline rule between sections of a surface. Fixed
-- | decoration, carrying no data.
dividerExact :: PUI Web {} {}
dividerExact = staticHTML "<fluent-divider style=\"width: 100%;\"></fluent-divider>"

divider :: Ocular (PUI Web)
divider = separating dividerExact

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import getStringProp :: String -> Node -> Effect String
foreign import selectDropdownOption :: Node -> String -> Effect Unit
foreign import selectGroupValue :: Node -> String -> Effect Unit
foreign import containsFocus :: Node -> Effect Boolean

-- Entry point

-- | Mount the app in the page's `<body>`, dressed for Fluent: the web light
-- | theme's tokens (`--colorNeutral*`, `--fontFamilyBase`, …) are set on the
-- | document here, and the app then mounts exactly as `PUI.Web.HTML.body`
-- | does — same signature, same closed-app demand (`PUI Web {} o`). The
-- | entry is a word of the vocabulary like any other, so a screen changes
-- | design system at its first line by changing the import, and nothing
-- | runs at import time.
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptTheme
  HTML.body ui

foreign import adoptTheme :: Effect Unit

-- | The **indeterminate progress bar**: a status fed a run's `started` and
-- | `ended`, shown between them — what `PUI.action`'s slot dispatches
-- | (`indeterminateLinearProgress # action submit`). No label: it names no
-- | field and no case.
indeterminateLinearProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateLinearProgress = wrap do
  element "fluent-progress-bar" (pure unit)
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ statusWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "width: 100%; min-width: 200px;"
  hiddenStyle = "width: 100%; min-width: 200px; visibility: hidden;"
