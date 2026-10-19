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
-- | values, `dropdown` and `radioGroup` (over `option` and `radio` leaves)
-- | to choose one, `button` to act, `messageBar` to say what happened,
-- | `progressBar` and `ratingDisplay` to show a figure, `card`, `divider`
-- | and the type ramp (`title3`, `body1`, `caption1`) for structure, and
-- | `body` to mount the app.
module PUI.Web.Fluent
  ( acknowledged
  , confirmed
  , body
  , body1
  , button
  , caption1
  , card
  , divider
  , dropdown
  , dropdownFrom
  , dropdownUnpicked
  , dropdownOptional
  , option
  , messageBar
  , indeterminateLinearProgress
  , progressBar
  , radioGroup
  , radioGroupFrom
  , radioGroupUnpicked
  , radioGroupOptional
  , radio
  , ratingDisplay
  , slider
  , slider'
  , textField
  , textFieldFrom
  , textField'
  , title3
  , toggleSwitch
  ) where

import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toString)
import Data.Variant (match) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Profunctor.Row.VariantToVariant (focusCase)
import PUI (introduced, fromHandles, handles, Ocular, PUI, recordToRecordWiring, variantToRecordWiring, variantToVariantWiring, optionWiring, statusOver, confirmedOver, acknowledgedOver, eventLeaf)
import PUI.Web.HTML (div)
import PUI.Web.HTML (body) as HTML
import PUI.Web (chrome, fieldLeaf, grouping, member, clearedByRepress, Node, Web, attribute, autoDismiss, cl, el, element, ensureStyle, getChecked, getValue, listenNode, removeAttribute, setAttribute, setChecked, setValue, staticHTML, staticText, text, statusText, separating, invokeMethod, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons, class Lacks)
import Data.Variant (inj)
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
--         commit/live split), and the selector groups `dropdown @l` and
--         `radioGroup @l` (`{ l :: f } → { l :: f }`, `f` the case of one of
--         their `option`/`radio` leaves, or a variant around it for the
--         `…Unpicked`/`…Optional` siblings), each over a
--         `VariantToVariant.do` block of `+→+` option leaves;
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
button :: forall @l r b v. IsSymbol l => Cons l { | r } b v => PUI Web { | r } [ | v ]
button = let config = { label: reflectSymbol (Proxy @l) } :: { label :: String } in eventLeaf @l $
  el "fluent-button" >>> "appearance" := "primary" $ staticText config.label

-- a `<fluent-field>` associating a label with the editor its builder
-- appends (Fluent's label protocol: both are slotted children of the field)
fieldWith :: forall i o. String -> String -> Web { toUser :: i -> Effect Unit, fromUser :: (o -> Effect Unit) -> Effect Unit } -> PUI Web i o
fieldWith position lbl editor = el "fluent-field" >>> "label-position" := position $ fromHandles do
  chrome (el "fluent-label" >>> "slot" := "label" $ staticText lbl)
  editor

-- | The **text field**: a labelled single-line input. Shows the string it
-- | is given and reports each edit; typing is never interrupted by values
-- | arriving from elsewhere. A whole-row citizen: fed the wide row, it
-- | edits field `l` and carries the rest.
textField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textField = fieldLeaf @l $ textInputLeaf (reflectSymbol (Proxy @l)) ""

-- | `textField` in its **introducing form**: the field is not in the row fed
-- | but stated here, `init` of the rest of the row its blank, for a
-- | knotless form with no seed. See `PUI.introduced`.
textFieldFrom :: forall @l r b. IsSymbol l => Lacks l b => Cons l String b r => ({ | b } -> String) -> PUI Web { | b } { | r }
textFieldFrom init = introduced @l init (textField @l)

-- | `textField` with a `hint` under it — a format or an example the label
-- | is not (`textField' @"Start date" { hint: "DD.MM.YYYY" }`), so the
-- | label stays the field's name. Shown as the field's message.
textField' :: forall @l r b. IsSymbol l => Cons l String b r => { hint :: String } -> PUI Web { | r } { | r }
textField' { hint } = fieldLeaf @l $ textInputLeaf (reflectSymbol (Proxy @l)) hint

textInputLeaf :: String -> String -> PUI Web String String
textInputLeaf lbl hint = el "fluent-field" >>> "label-position" := "above" $ fromHandles do
  chrome (el "fluent-label" >>> "slot" := "label" $ staticText lbl)
  editor <- textInput
  when (hint /= "") $
    chrome (el "fluent-text" >>> "slot" := "message" >>> "size" := "200" $ staticText hint)
  pure editor
  where
  textInput = do
    -- Fluent keeps the real `<input>` in the light DOM, so the focus guard
    -- checks containment
    element "fluent-text-input" (pure unit)
    attribute "slot" "input"
    node <- gets _.sibling
    liftEffect $ recordToRecordWiring
      { show: setValue node
      , held: containsFocus node
      , reports: listenNode node "input"
      , read: \_ -> getValue node
      }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | Its label sits after the control, in Fluent's manner.
toggleSwitch :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
toggleSwitch = let config = { label: reflectSymbol (Proxy @l) } in fieldLeaf @l $ fieldWith "after" config.label do
  element "fluent-switch" (pure unit)
  attribute "slot" "input"
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
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
slider = fieldLeaf @l $ sliderLeaf (reflectSymbol (Proxy @l)) ""

-- | `slider` whose value is read in a `unit` (`slider' @"Amount" { unit:
-- | "€" }`), shown after the number in the label line's readout, so the
-- | label stays the quantity's name.
slider' :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => { unit :: String } -> PUI Web { | r } { | r }
slider' config = fieldLeaf @l $ sliderLeaf (reflectSymbol (Proxy @l)) config.unit

sliderLeaf :: String -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
sliderLeaf lbl measure = el "fluent-field" >>> "label-position" := "above" $ fromHandles do
  readout <- handles $ (el "fluent-label" >>> "slot" := "label" >>> "style" := "display: flex; justify-content: space-between; width: 100%;" $ fromHandles do
      chrome (staticText lbl)
      handles (el "span" >>> "style" := "color: var(--colorNeutralForeground3, #616161);" $ text _.readout))
  -- the readout is written, never listened to; text's echo needs a listener
  liftEffect $ readout.fromUser \_ -> pure unit
  element "fluent-slider" (pure unit)
  attribute "slot" "input"
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: \q -> do
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        Variant.match { discrete: \s -> setAttribute node "step" (show s), continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "valueAsNumber" node q.current
        readout.toUser { readout: measured q.current }
    , held: pure false
    , reports: listenNode node "change"
    , read: \q -> getNumberProp "valueAsNumber" node <#> \v -> q { current = v }
    }
  where
  measured n = if measure == "" then toString n else toString n <> " " <> measure

-- | The **dropdown**: one choice out of a list too long to lay out in the
-- | open.
-- |
-- | The options are `option` leaves, one per case of the field, merged
-- | `VariantToVariant.do` in the order they are listed:
-- | `dropdown @"Room" $ VariantToVariant.do option @"Focus pod"; option @"Boardroom"`.
-- | The block closes the field's cases; the dropdown lifts it onto field
-- | `l`, so the field holds the option itself and the model always has one.
-- | Two siblings hold a variant instead: `dropdownUnpicked @l @c` for a
-- | choice owed but not yet made, `dropdownOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- | The options belong to the control, not to the model.
dropdown :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
dropdown options = fieldLeaf @l $ dropdownOf @l false options

-- | `dropdown` in its introducing form, as `textFieldFrom`.
dropdownFrom :: forall @l w b r. IsSymbol l => Lacks l b => Cons l [ | w ] b r => ({ | b } -> [ | w ]) -> PUI Web [ | w ] [ | w ] -> PUI Web { | b } { | r }
dropdownFrom init options = introduced @l init (dropdown @l options)

-- | `dropdown` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
dropdownUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
dropdownUnpicked options = fieldLeaf @l $ dropdownOf @l false (focusCase @c options)

-- | `dropdown` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | an empty first option clears it, storing `n` again.
dropdownOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
dropdownOptional options = fieldLeaf @l $ dropdownOf @l true (blankEmits @n (focusCase @c options))

-- A selector group in three forms, over one clearing of its options:
-- every feed first shows nothing picked, then the one option owning the
-- fed case shows itself, so a case no option owns (an unpicked one) shows
-- nothing. Both writes wait for the dropdown to bind (`whenBoundDo`), in
-- the order they were made.
dropdownOf :: forall @l a. IsSymbol l => Boolean -> PUI Web a a -> PUI Web a a
dropdownOf blank options = fieldWith "above" (reflectSymbol (Proxy @l)) do
  inner <- element "fluent-dropdown" do
    element "fluent-listbox" do
      when blank $ chrome (staticHTML "<fluent-option value=\"\"></fluent-option>")
      handles options
  node <- gets _.sibling
  liftEffect $ setAttribute node "slot" "input"
  pure
    { toUser: \a -> selectDropdownOption node "" *> inner.toUser a
    , fromUser: inner.fromUser
    }

-- The clearing of an optional dropdown: picking the empty option emits `n`.
blankEmits :: forall @n b v. IsSymbol n => Cons n {} b v => PUI Web [ | v ] [ | v ] -> PUI Web [ | v ] [ | v ]
blankEmits options = fromHandles do
  dropdownNode <- gets _.parent >>= liftEffect <<< enclosingDropdown
  inner <- handles options
  pure
    { toUser: inner.toUser
    , fromUser: \prop -> do
        inner.fromUser prop
        listenNode dropdownNode "change" do
          value <- getStringProp "value" dropdownNode
          when (value == "") $ prop (inj (Proxy @n) {})
    }

-- | One option of a `dropdown`: a `<fluent-option>` reading its case.
option :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
option = fromHandles do
  dropdownNode <- gets _.parent >>= liftEffect <<< enclosingDropdown
  element "fluent-option" (chrome (staticText (reflectSymbol (Proxy @c))))
  attribute "value" (reflectSymbol (Proxy @c))
  liftEffect $ optionWiring @c
    { show: selectDropdownOption dropdownNode (reflectSymbol (Proxy @c))
    , reports: listenNode dropdownNode "change"
    , picked: (_ == reflectSymbol (Proxy @c)) <$> getStringProp "value" dropdownNode
    }

-- | The **radio group**: one choice among a handful, every option visible
-- | and comparable at a glance. Beyond about five options use `dropdown`.
-- | Same selection contract as `dropdown`; its options are `radio` leaves:
-- | `radioGroup @"Duration" $ VariantToVariant.do radio @"30 min"; radio @"1 hour"`.
radioGroup :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioGroup options = fieldLeaf @l $ radioGroupOf @l options

-- | `radioGroup` in its introducing form, as `textFieldFrom`.
radioGroupFrom :: forall @l w b r. IsSymbol l => Lacks l b => Cons l [ | w ] b r => ({ | b } -> [ | w ]) -> PUI Web [ | w ] [ | w ] -> PUI Web { | b } { | r }
radioGroupFrom init options = introduced @l init (radioGroup @l options)

-- | `radioGroup` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
radioGroupUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioGroupUnpicked options = fieldLeaf @l $ radioGroupOf @l (focusCase @c options)

-- | `radioGroup` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | pressing the checked option again clears it, storing `n` again.
radioGroupOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioGroupOptional options = fieldLeaf @l $ radioGroupOf @l (clearedByRepress @n radioBeforeClick (focusCase @c options))

-- The group's value setter is the sanctioned selection path, and it needs
-- the group bound (its bind-time sync would clear earlier per-radio
-- writes), so the clearing and an option's showing both go through it.
radioGroupOf :: forall @l a. IsSymbol l => PUI Web a a -> PUI Web a a
radioGroupOf options = fieldWith "above" (reflectSymbol (Proxy @l)) do
  inner <- element "fluent-radio-group" (_.result <$> grouping (handles options))
  groupNode <- gets _.sibling
  liftEffect $ setAttribute groupNode "slot" "input"
  pure
    { toUser: \a -> selectGroupValue groupNode "" *> inner.toUser a
    , fromUser: inner.fromUser
    }

radioBeforeClick :: Node -> Effect Unit -> Effect Unit
radioBeforeClick group snapshot = do
  listenNode group "pointerdown" snapshot
  listenNode group "keydown" snapshot

-- | One option of a `radioGroup`: a `<fluent-radio>` labelled by its case.
radio :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
radio = fromHandles do
  groupNode <- gets _.parent
  radioNode <- element "fluent-field" do
    chrome (el "fluent-label" >>> "slot" := "label" $ staticText (reflectSymbol (Proxy @c)))
    element "fluent-radio" (pure unit)
    gets _.sibling
  fieldNode <- gets _.sibling
  _ <- member { node: radioNode, picked: getChecked radioNode, unpick: selectGroupValue groupNode "" }
  liftEffect do
    setAttribute radioNode "slot" "input"
    setAttribute radioNode "value" (reflectSymbol (Proxy @c))
    setAttribute fieldNode "label-position" "after"
    -- the group re-reports the checked radio when a press focuses it; the
    -- press says whether it was checked, so only a new pick leaves
    wasCheckedRef <- Ref.new false
    listenNode fieldNode "pointerdown" (getChecked radioNode >>= flip Ref.write wasCheckedRef)
    listenNode fieldNode "click" (Ref.write false wasCheckedRef)
    optionWiring @c
      { show: selectGroupValue groupNode (reflectSymbol (Proxy @c))
      , reports: \k -> listenNode radioNode "change" do
          wasChecked <- Ref.read wasCheckedRef
          unless wasChecked k
      , picked: getChecked radioNode
      }

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a quota, a share, a
-- | rating out of five — `progressBar progressFraction`: the
-- | value is a **read function** like every display's, since a fraction is
-- | derived (a ratio of source fields), not state.
progressBar
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
progressBar f = fromHandles do
  element "fluent-progress-bar" (pure unit)
  attribute "max" "1"
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring { show: \r -> setNumberProp "value" node (f r), held: pure false, reports: \_ -> pure unit, read: pure }

-- | A **read-only star rating**: someone else's score, shown but not
-- | editable — Fluent's catalog has the display and not the editor, so
-- | there is deliberately no star *editor* here (Shoelace's `rating` is
-- | the one).
ratingDisplay
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
ratingDisplay f = fromHandles do
  element "fluent-rating-display" (pure unit)
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring { show: \r -> setNumberProp "value" node (f r), held: pure false, reports: \_ -> pure unit, read: pure }

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
messageBar = statusOver @l messageBarFace

messageBarFace :: forall a r. String -> PUI Web a { | r }
messageBarFace caseLabel = fromHandles do
  liftEffect $ ensureStyle "fluent-toast" toastCss
  w <- handles $ (el "fluent-message-bar" >>> "intent" := "success" $
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

-- | The **witness rung** — see `PUI.Web.MDC2.confirmed`: `focusCase @l`
-- | over the modal, the `+→+` stage of case `l`, its confirm button
-- | captioned by the case and re-emitting it with the row its content
-- | answered, a cancel consuming it, every other case passing. Over `<fluent-dialog>`.
confirmed
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
confirmed = confirmedOver @l modal

-- | The **dismissed rung** — see `PUI.Web.MDC2.acknowledged`: `onCase @l`
-- | over the modal, the `+→+` stage consuming case `l`, telling it as its
-- | title and showing its payload until the user closes it, dismissed
-- | by its "OK"; the row less `l` leaves.
acknowledged
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | b ]
acknowledged = acknowledgedOver @l modal

-- the modal both dialog leaves are built over: `variantToVariantWiring`
-- over `<fluent-dialog>` and its `<fluent-dialog-body>` (v3's markup
-- contract: title, content and action slots), an optional title, an
-- optional cancel button and the deciding button in the action slot,
-- named by the title or else by the decision; Escape closes the element
-- by itself, releasing nothing
modal
  :: forall r
   . { title :: Maybe String, decide :: String, cancel :: Maybe String }
  -> PUI Web { | r } { | r }
  -> PUI Web { | r } { | r }
modal { title, decide, cancel } content = fromHandles do
  decideRef <- liftEffect $ Ref.new Nothing
  cancelRef <- liftEffect $ Ref.new Nothing
  shown <- handles $ el "fluent-dialog" >>> "aria-label" := fromMaybe decide title $ el "fluent-dialog-body" $ fromHandles do
    for_ title \t -> handles (div >>> "slot" := "title" $ staticText t)
    w <- handles (div $ content)
    for_ cancel \c -> do
      chrome (el "fluent-button" >>> "slot" := "action" $ staticText c)
      node <- gets _.sibling
      liftEffect $ Ref.write (Just node) cancelRef
    chrome (el "fluent-button" >>> "slot" := "action" >>> "appearance" := "primary" $ staticText decide)
    node <- gets _.sibling
    liftEffect $ Ref.write (Just node) decideRef
    pure w
  dialogNode <- gets _.sibling
  decideNode <- liftEffect $ Ref.read decideRef
  cancelNode <- liftEffect $ Ref.read cancelRef
  liftEffect $ variantToVariantWiring
    { open: invokeMethod "show" dialogNode
    , close: invokeMethod "hide" dialogNode
    , content: shown
    , reports: \decided -> for_ decideNode \node -> listenNode node "click" decided
    , cancels: \cancelled -> for_ cancelNode \node -> listenNode node "click" cancelled
    }

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
card content = fromHandles do
  liftEffect $ ensureStyle "fluent-card" cardCss
  handles $ div >>> "class" := "fluent-card" $ content

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
foreign import enclosingDropdown :: Node -> Effect Node
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
indeterminateLinearProgress = fromHandles do
  element "fluent-progress-bar" (pure unit)
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ variantToRecordWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "width: 100%; min-width: 200px;"
  hiddenStyle = "width: 100%; min-width: 200px; visibility: hidden;"
