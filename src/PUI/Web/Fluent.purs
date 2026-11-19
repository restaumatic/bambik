-- | The **Fluent UI** vocabulary (https://fluent2.microsoft.design), over
-- | Microsoft's `@fluentui/web-components` v3: the same names and
-- | signatures as the Material modules wherever both catalogues have the
-- | concept (guardrails L20). Fluent's own entries are `ratingDisplay`, a
-- | rating shown but not edited — the catalogue has no star *editor*, and
-- | this vocabulary invents none — and `messageBar`.
-- |
-- | **The page needs nothing**: `body` applies the web light theme from
-- | the bundle at mount, and the type ramp rides the system font stack.
-- |
-- | The catalogue: `textField`, `toggleSwitch` and `slider` to enter
-- | values, `dropdown` and `radioGroup` (over `option` and `radio` leaves)
-- | to choose one, `button` to act, `messageBar` to say what happened,
-- | `progressBar` and `ratingDisplay` to show a figure, `card`, `divider`
-- | and the type ramp (`title3`, `body1`, `caption1`) for structure, and
-- | `body` to mount the app.
module PUI.Web.Fluent
  ( confirmed
  , confirmed'
  , modal
  , body
  , body1
  , button
  , caption1
  , card
  , group
  , divider
  , dropdown
  , dropdownUnpicked
  , dropdownOptional
  , option
  , messageBar
  , indeterminateLinearProgress
  , progressBar
  , radioGroup
  , radioGroupUnpicked
  , radioGroupOptional
  , radio
  , ratingDisplay
  , slider
  , slider'
  , textField
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
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import PUI (fromHandles, handles, Ocular, PUI, recordToRecordWiring, variantToRecordWiring, variantToVariantWiring, optionWiring, statusOver, confirmedOver, confirmedOver', modalOver, eventLeaf)
import PUI.Web.HTML (div)
import PUI.Web.HTML (body) as HTML
import PUI.Web (chrome, fieldLeaf, grouping, member, clearedByRepress, Node, Web, attribute, autoDismiss, cl, el, element, ensureStyle, getChecked, getValue, listenNode, removeAttribute, setAttribute, setChecked, setValue, staticHTML, staticText, text, statusText, separating, invokeMethod, uniqueId, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Data.Variant (inj)
import Data.Symbol (class IsSymbol, reflectSymbol)

-- Implementation notes — the reference above is the contract.
--
-- The `PUI.Web.MDC3` recipe over Fluent's custom elements, registered by
-- importing the FFI module: a leaf is `element "fluent-..."` plus
-- property/event wiring, and the leaf protocols are the shared ones
-- (focus-guarded text field, per-feed display echo, selection editors
-- answering every feed with their row). Fluent associates labels through
-- `<fluent-field>`, so the labelled editors carry that wrapper as chrome.
-- FAST binds a component a beat after insertion and its update queue is
-- rAF-driven (starved in frameless headless sessions), so the dropdown
-- and the radio group defer their writes on a timer poll (`whenBoundDo`
-- in Fluent.js). The `dimap` round-trip contract for editors holds as in
-- `PUI.Web.MDC2`.

-- UIs

-- | The **primary button** (`<fluent-button appearance="primary">`): on
-- | click it emits the row it was showing as its case, which captions it
-- | — `button @"Book the room"`.
button :: forall @l r v. IsSymbol l => Cons l { | r } () v => PUI Web { | r } [ | v ]
button = let config = { label: reflectSymbol (Proxy @l) } :: { label :: String } in eventLeaf @l $
  el "fluent-button" >>> "appearance" := "primary" $ staticText config.label

-- a `<fluent-field>` associating a label with the editor its builder
-- appends (Fluent's label protocol: both are slotted children of the field)
fieldWith :: forall i o. String -> String -> Web { toUser :: i -> Effect Unit, fromUser :: (o -> Effect Unit) -> Effect Unit } -> PUI Web i o
fieldWith position lbl editor = el "fluent-field" >>> "label-position" := position $ fromHandles do
  chrome (el "fluent-label" >>> "slot" := "label" $ staticText lbl)
  editor

-- | The **text field** (`<fluent-text-input>` in a `<fluent-field>`): a
-- | labelled single-line input editing field `l`. Shows the string it is
-- | given and reports each edit; typing is never interrupted by values
-- | arriving from elsewhere.
textField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textField = fieldLeaf @l $ textInputLeaf (reflectSymbol (Proxy @l)) ""

-- | `textField` with a `hint` under it — a format or an example the label
-- | is not (`textField' @"Start date" { hint: "DD.MM.YYYY" }`), shown as
-- | the field's message.
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

-- | The **switch** (`<fluent-switch>`): a Boolean setting that takes
-- | effect the moment it is flipped. Its label sits after the control, in
-- | Fluent's manner.
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

-- | The **slider** (`<fluent-slider>`): a quantity chosen by feel. The
-- | field is the bounded quantity `{ current, min, max, step }`, one
-- | business datum, so the limits come from the data and may change while
-- | the app runs; `step` is `.discrete n` or `.continuous {}`. It reports
-- | on **every change**, following the drag — Fluent has no commit-only
-- | slider — so what it drives should be cheap to redo or `debounced`.
-- | The current number is shown at the end of the label line, since the
-- | control has no readout of its own.
slider :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
slider = fieldLeaf @l $ sliderLeaf (reflectSymbol (Proxy @l)) ""

-- | `slider` whose value is read in a `unit` (`slider' @"Amount" { unit:
-- | "€" }`), shown after the number in the readout.
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

-- | The **dropdown** (`<fluent-dropdown>` over a `<fluent-listbox>`,
-- | which its options must sit in): one choice out of a list too long to
-- | lay out in the open. Its options are `option` leaves, one per case of
-- | the field, merged `VariantToVariant.do` in the order listed:
-- | `dropdown @"Room" $ VariantToVariant.do option @"Focus pod"; option
-- | @"Boardroom"`. The block closes the field's cases and the dropdown
-- | lifts it onto field `l`, which holds the option itself. Two siblings
-- | hold a variant instead: `dropdownUnpicked @l @c` for a choice owed
-- | but not yet made, `dropdownOptional @l @c @n` for one the user may
-- | leave unmade. All three are editors: every feed is answered with the
-- | row, every pick stored.
dropdown :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
dropdown options = fieldLeaf @l $ dropdownOf @l false options

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

-- | The **radio group** (`<fluent-radio-group>`): one choice among a
-- | handful, every option visible; beyond about five options use
-- | `dropdown`. The selection contract is `dropdown`'s, its options
-- | `radio` leaves: `radioGroup @"Duration" $ VariantToVariant.do radio
-- | @"30 min"; radio @"1 hour"`.
radioGroup :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioGroup options = fieldLeaf @l $ radioGroupOf @l options

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
radioBeforeClick groupNode snapshot = do
  listenNode groupNode "pointerdown" snapshot
  listenNode groupNode "keydown" snapshot

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

-- | The **progress bar** (`<fluent-progress-bar>`): a fraction 0–1 of how
-- | far along something is — as much a gauge as a progress indicator.
-- | Takes a read function and no label (guardrails L17):
-- | `progressBar progressFraction`.
progressBar
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
progressBar f = fromHandles do
  element "fluent-progress-bar" (pure unit)
  attribute "max" "1"
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring { show: \r -> setNumberProp "value" node (f r), held: pure false, reports: \_ -> pure unit, read: pure }

-- | A **read-only star rating** (`<fluent-rating-display>`): a score
-- | shown, not edited, from a read function. Fluent's catalogue has the
-- | display and not the editor, so there is deliberately no star *editor*
-- | here (Shoelace's `rating` is the one).
ratingDisplay
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
ratingDisplay f = fromHandles do
  element "fluent-rating-display" (pure unit)
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring { show: \r -> setNumberProp "value" node (f r), held: pure false, reports: \_ -> pure unit, read: pure }

-- | The **message bar** (`<fluent-message-bar intent="success">`, fixed
-- | at the bottom): a brief message that dismisses itself after a few
-- | seconds and never interrupts. A status, it tells its case — the case
-- | label is its whole copy — and takes its fold, of the event and the
-- | model it lands on (`messageBar @"Room booked" kept`; see
-- | `PUI.Web.MDC2.snackbar`).
messageBar
  :: forall @l a v i o
   . IsSymbol l
  => Cons l a () v
  => ({ event :: a, model :: i } -> o)
  -> PUI Web [ | v ] (i -> o)
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

-- | `confirmed` — see `PUI.Web.MDC2.confirmed`; over `<fluent-dialog>`.
confirmed
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
confirmed = confirmedOver @l modalFace

-- | `confirmed` with options: `cancel:` captions its Cancel
-- | (`confirmed' @"Usuń danie" { cancel: "Nie, nie usuwaj dania" } $ …`,
-- | diner); the Cancel still consumes the case.
confirmed'
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => { cancel :: String }
  -> PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
confirmed' { cancel } = confirmedOver' @l cancel modalFace

-- | The **modal** — see `PUI.Web.MDC2.modal`; over `<fluent-dialog>`.
modal
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
modal = modalOver @l modalFace

-- the modal the dialog leaves are built over: `variantToVariantWiring`
-- over `<fluent-dialog>` and its `<fluent-dialog-body>` (v3's markup
-- contract: title, content and action slots), an optional title, an
-- optional cancel button and the deciding button in the action slot,
-- named by the title or else by the decision; Escape closes the element
-- by itself, releasing nothing
modalFace
  :: forall r
   . { title :: Maybe String, decide :: String, cancel :: Maybe String }
  -> PUI Web { | r } { | r }
  -> PUI Web { | r } { | r }
modalFace { title, decide, cancel } content = fromHandles do
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

-- | A **card**: a surface holding one subject's content, stacking its
-- | children with even spacing. Hand-rolled over the `--colorNeutral*`
-- | and `--shadow*` tokens, since Fluent's card is a React-only entry. A
-- | plain ocular with no config: a heading is typography in its content
-- | (`title3`, `body1`).
card :: Ocular (PUI Web)
card content = fromHandles do
  liftEffect $ ensureStyle "fluent-card" cardCss
  handles $ div >>> "class" := "fluent-card" $ content

-- | The **labelled group** — the contract is `PUI.Web.MDC2.group`'s:
-- | `w` on a `card` headed by the label verbatim (`title3`), its model
-- | nested under field `l`, the label the accessible group name.
-- | Derived: `group @l w = card (heading >>> focusField @l w)` plus the
-- | a11y stamp.
group
  :: forall @l f f' b s t
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b t
  => PUI Web f f'
  -> PUI Web { | s } { | t }
group w = fromHandles do
  headingId <- liftEffect uniqueId
  handles $ "role" := "group" $ "aria-labelledby" := headingId $ card $
    ("id" := headingId $ title3 $ staticText (reflectSymbol (Proxy @l))) >>> focusField @l w

cardCss :: String
cardCss = """
.fluent-card { background: var(--colorNeutralBackground1, #fff); color: var(--colorNeutralForeground1, #242424); font-family: var(--fontFamilyBase, 'Segoe UI', sans-serif); border-radius: var(--borderRadiusXLarge, 8px); box-shadow: var(--shadow4, 0 2px 4px rgba(0,0,0,.14)); padding: 20px; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; }
"""

-- statics (chrome with a face, the wire at any row)

-- the divider's face, a `{}`-static
dividerExact :: PUI Web {} {}
dividerExact = staticHTML "<fluent-divider style=\"width: 100%;\"></fluent-divider>"

-- | A **divider** (`<fluent-divider>`): the hairline rule between
-- | sections, shown only between things (`separating`). Carries no data.
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

-- | Mount the app in the page's `<body>` as `PUI.Web.HTML.body` does (same
-- | signature, same closed-app demand), after setting the web light
-- | theme's tokens (`--colorNeutral*`, `--fontFamilyBase`, …) on the
-- | document. Nothing runs at import time (guardrails L11).
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptTheme
  HTML.body ui

foreign import adoptTheme :: Effect Unit

-- | The **indeterminate progress bar**: the action slot, fed a run's
-- | `started` and `ended` and shown between them
-- | (`indeterminateLinearProgress # action @l f`). No label: it names no
-- | field and no case.
indeterminateLinearProgress :: PUI Web [ started :: {}, ended :: {} ] {}
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
