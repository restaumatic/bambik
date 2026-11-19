-- | The **Bootstrap** vocabulary (https://getbootstrap.com) — the CSS-only
-- | member of the family: Bootstrap ships a stylesheet, not components, so
-- | every control here is a native element (`<input>`, `<select>`,
-- | `<button>`) wearing the documented classes, with no custom elements,
-- | no foundation instances and no FFI beyond the toast's dismissal timer
-- | (`autoDismiss`, the one behaviour Bootstrap's own JS plugin would
-- | supply). The same names and signatures as the Material modules
-- | wherever both catalogues have the concept (guardrails L20); its gaps
-- | stay gaps: `sliderLive` only, no dialog.
-- |
-- | **The page must load** the Bootstrap 5 stylesheet. No scripts and no
-- | fonts — the design system rides the system font stack. Its `body` is
-- | `PUI.Web.HTML.body` under this vocabulary's name: the reboot needs
-- | nothing on the page body.
-- |
-- | The catalogue: `textField`, `sliderLive` and `toggleSwitch` to enter
-- | values, `select` to choose one, `button` to act, `toast` to say what
-- | happened, `progress` to show a figure, `card`,
-- | `listGroup`/`listGroupItem` and `badge` for structure, `body` to mount
-- | the app. Typography is deliberately absent: Bootstrap styles plain HTML,
-- | so the `PUI.Web.HTML` elements are the type scale.
module PUI.Web.Bootstrap
  ( badge
  , body
  , button
  , card
  , group
  , listGroup
  , listGroupItem
  , option
  , indeterminateLinearProgress
  , progress
  , select
  , selectUnpicked
  , selectOptional
  , sliderLive
  , sliderLive'
  , textField
  , textField'
  , toast
  , toggleSwitch
  ) where

import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.Int (round)
import Data.Maybe (fromMaybe)
import Data.Number (fromString) as Number
import Data.Number.Format (toString)
import Data.Variant (match) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import PUI (fromHandles, handles, Ocular, PUI, recordToRecordWiring, variantToRecordWiring, statusOver, eventLeaf)
import Data.Profunctor.Row.RecordToRecord (focusField)
import PUI.Web.HTML (div, h5, label, span)
import PUI.Web.HTML (body, option) as HTML
import PUI.Web (chrome, fieldLeaf, Web, attribute, autoDismiss, cl, el, element, getChecked, getValue, isFocused, listenNode, setAttribute, setChecked, setValue, staticText, text, statusText, uniqueId, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Data.Variant (inj)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import Data.Symbol (class IsSymbol, reflectSymbol)

-- Implementation notes — the reference above is the contract.
--
-- The leaf protocols are the MDC modules' (focus-guarded text field,
-- per-feed display echo, a selection editor answering every feed with its
-- row), over native elements. The `dimap` round-trip contract for editors
-- holds as in `PUI.Web.MDC2`.

-- UIs

-- | The **primary button** (`.btn.btn-primary`): on click it emits the
-- | row it was showing as its case, which captions it — `button @"Apply
-- | for the loan"`.
button :: forall @l r v. IsSymbol l => Cons l { | r } () v => PUI Web { | r } [ | v ]
button = let config = { label: reflectSymbol (Proxy @l) } :: { label :: String } in eventLeaf @l $
  (el "button" >>> "type" := "button" $ staticText config.label) # cl "btn" # cl "btn-primary"

-- | The **text field** (`.form-control` under its `.form-label`): a
-- | single-line input editing field `l`. Shows the string it is given and
-- | reports each edit; typing is never interrupted by values arriving
-- | from elsewhere.
textField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textField = fieldLeaf @l $ inputLeaf (reflectSymbol (Proxy @l)) ""

-- | `textField` with a `hint` under it — a format or an example the label
-- | is not (`textField' @"Start date" { hint: "DD.MM.YYYY" }`), shown as
-- | Bootstrap's form text describing the input.
textField' :: forall @l r b. IsSymbol l => Cons l String b r => { hint :: String } -> PUI Web { | r } { | r }
textField' { hint } = fieldLeaf @l $ inputLeaf (reflectSymbol (Proxy @l)) hint

inputLeaf :: String -> String -> PUI Web String String
inputLeaf lbl hint = div >>> "style" := "width: 100%;" $ fromHandles do
  hintId <- liftEffect uniqueId
  chrome ((label $ staticText lbl) # cl "form-label")
  element "input" (pure unit)
  attribute "type" "text"
  attribute "class" "form-control"
  when (hint /= "") $ attribute "aria-describedby" hintId
  node <- gets _.sibling
  wiring <- liftEffect $ recordToRecordWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "input"
    , read: \_ -> getValue node
    }
  when (hint /= "") $
    chrome ((div >>> "id" := hintId $ staticText hint) # cl "form-text")
  pure wiring

-- | The **live slider** (`.form-range`): a quantity chosen by feel. The
-- | field is the bounded quantity `{ current, min, max, step }`, one
-- | business datum, so the limits come from the data and may change while
-- | the app runs; `step` is `.discrete n` or `.continuous {}`. It reports
-- | on **every drag step** — the native range input has no commit-only
-- | behaviour, hence the name — so what it drives should be cheap to redo
-- | or `debounced`. The current number is shown at the end of the label
-- | line, since the control has no readout of its own.
sliderLive :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
sliderLive = fieldLeaf @l $ rangeLeaf (reflectSymbol (Proxy @l)) ""

-- | `sliderLive` whose value is read in a `unit` (`sliderLive' @"Amount"
-- | { unit: "€" }`), shown after the number in the readout.
sliderLive' :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => { unit :: String } -> PUI Web { | r } { | r }
sliderLive' config = fieldLeaf @l $ rangeLeaf (reflectSymbol (Proxy @l)) config.unit

rangeLeaf :: String -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
rangeLeaf lbl measure = div >>> "style" := "width: 100%;" $ fromHandles do
  readout <- handles $ (label $ fromHandles do
      chrome (span $ staticText lbl)
      handles ((span $ text _.readout) # cl "text-body-secondary")
    ) # cl "form-label" # cl "d-flex" # cl "justify-content-between"
  -- the readout is written, never listened to; text's echo needs a listener
  liftEffect $ readout.fromUser \_ -> pure unit
  element "input" (pure unit)
  attribute "type" "range"
  attribute "class" "form-range"
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: \q -> do
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        setAttribute node "step" (Variant.match { discrete: show, continuous: \_ -> "any" } q.step)
        setValue node (show q.current)
        readout.toUser { readout: measured q.current }
    , held: pure false
    , reports: listenNode node "input"
    , read: \q -> getValue node <#> \value -> q { current = fromMaybe q.current (Number.fromString value) }
    }
  where
  measured n = if measure == "" then toString n else toString n <> " " <> measure

-- | The **select** (`.form-select` under its label): one choice out of a
-- | list. Its options are `option` leaves (the native `<option>`), one
-- | per case of the field, merged `VariantToVariant.do` in the order
-- | written; the block closes the field's cases and the select lifts it
-- | onto field `l`, which holds the option itself. Two siblings hold a
-- | variant instead: `selectUnpicked @l @c` for a choice owed but not yet
-- | made, `selectOptional @l @c @n` for one the user may leave unmade.
-- | All three are editors: every feed is answered with the row, every
-- | pick stored.
select :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
select options = fieldLeaf @l $ selectGroupOf @l false options

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
selectUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectUnpicked options = fieldLeaf @l $ selectGroupOf @l false (focusCase @c options)

-- | `select` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | an empty first option clears it, storing `n` again.
selectOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectOptional options = fieldLeaf @l $ selectGroupOf @l true (blankEmits @n (focusCase @c options))

-- The label and the `.form-select` around the options, cleared before every
-- feed so a case no option owns shows nothing picked; `blank` leads with an
-- empty option, the one a cleared optional select shows.
selectGroupOf :: forall @l a. IsSymbol l => Boolean -> PUI Web a a -> PUI Web a a
selectGroupOf blank options = div >>> "style" := "width: 100%;" $ fromHandles do
  chrome ((label $ staticText (reflectSymbol (Proxy @l))) # cl "form-label")
  handles $ el "select" >>> "class" := "form-select" $ fromHandles do
    selectNode <- gets _.parent
    when blank do
      element "option" (pure unit)
      noneNode <- gets _.sibling
      liftEffect $ setAttribute noneNode "value" ""
    inner <- handles options
    pure
      { toUser: \fed -> setValue selectNode "" *> inner.toUser fed
      , fromUser: inner.fromUser
      }

-- | One option of a `select`: the native `<option>` showing its case.
option :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
option = HTML.option @c

-- The clearing of an optional select: picking the empty option emits `n`.
blankEmits :: forall @n b v. IsSymbol n => Cons n {} b v => PUI Web [ | v ] [ | v ] -> PUI Web [ | v ] [ | v ]
blankEmits options = fromHandles do
  selectNode <- gets _.parent
  inner <- handles options
  pure
    { toUser: inner.toUser
    , fromUser: \prop -> do
        inner.fromUser prop
        listenNode selectNode "change" do
          value <- getValue selectNode
          when (value == "") $ prop (inj (Proxy @n) {})
    }

-- | The **switch** (`.form-check.form-switch`): a Boolean setting that
-- | takes effect the moment it is flipped. The label is part of the
-- | target, so clicking the words toggles it too.
toggleSwitch :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
toggleSwitch = let config = { label: reflectSymbol (Proxy @l) } in fieldLeaf @l $ (div $ fromHandles do
  inputId <- liftEffect uniqueId
  element "input" (pure unit)
  node <- gets _.sibling
  liftEffect do
    setAttribute node "class" "form-check-input"
    setAttribute node "type" "checkbox"
    setAttribute node "role" "switch"
    setAttribute node "id" inputId
  chrome ((label >>> "for" := inputId $ staticText config.label) # cl "form-check-label")
  liftEffect $ recordToRecordWiring
    { show: setChecked node
    , held: pure false
    , reports: listenNode node "change"
    , read: \_ -> getChecked node
    }) # cl "form-check" # cl "form-switch"

-- | The **progress bar** (`.progress` over `.progress-bar`): a fraction
-- | 0–1 of how far along something is — as much a gauge as a progress
-- | indicator. Takes a read function and no label (guardrails L17):
-- | `progress progressFraction`.
progress
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
progress f = fromHandles do
  barNode <- element "div" do
    element "div" (pure unit)
    bar <- gets _.sibling
    liftEffect $ setAttribute bar "class" "progress-bar"
    pure bar
  node <- gets _.sibling
  liftEffect do
    setAttribute node "class" "progress"
    setAttribute node "role" "progressbar"
    setAttribute node "style" "width: 100%; min-width: 200px;"
  liftEffect $ recordToRecordWiring { show: \r -> setAttribute barNode "style" ("width: " <> show (round (f r * 100.0)) <> "%;"), held: pure false, reports: \_ -> pure unit, read: pure }

-- | The **toast** (`.toast` fixed at the bottom, dismissed after a few
-- | seconds by the hand-wired `autoDismiss` timer): a brief message that
-- | never interrupts. A status, it tells its case — the case label is its
-- | whole copy — and takes its fold, of the event and the model it lands
-- | on (`toast @"Application received" kept`; see
-- | `PUI.Web.MDC2.snackbar`).
toast
  :: forall @l a v i o
   . IsSymbol l
  => Cons l a () v
  => ({ event :: a, model :: i } -> o)
  -> PUI Web [ | v ] (i -> o)
toast = statusOver @l toastFace

toastFace :: forall a r. String -> PUI Web a { | r }
toastFace caseLabel = fromHandles do
  w <- handles $ (el "div" >>> "role" := "status"
    >>> "style" := "position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000;" $
      (div $ statusText caseLabel) # cl "toast-body")
    # cl "toast" # cl "text-bg-primary" # cl "border-0"
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        autoDismiss node "show" 5000
    , fromUser: w.fromUser
    }

-- UIOculars

-- | A **card** (`.card`): a surface holding one subject's content, its
-- | body stacking the children with even spacing. A plain ocular with no
-- | config: a heading is typography in its content, styled with the
-- | documented `card-title` where a card wants one (`h5 … # cl
-- | "card-title"`).
card :: Ocular (PUI Web)
card content =
  (div $ (div $ content)
    # cl "card-body" # cl "d-flex" # cl "flex-column" # cl "align-items-start" # cl "gap-3"
  ) # cl "card"

-- | The **labelled group** — the contract is `PUI.Web.MDC2.group`'s:
-- | `w` on a `card` headed by the label verbatim as its `card-title`, its
-- | model nested under field `l`, the label the accessible group name.
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
    ("id" := headingId $ h5 >>> cl "card-title" $ staticText (reflectSymbol (Proxy @l))) >>> focusField @l w

-- | A **list group** (`.list-group`): rows of `listGroupItem`s sharing
-- | one bordered surface — a readout of figures, a set of related lines.
listGroup :: Ocular (PUI Web)
listGroup w = (el "ul" $ w) # cl "list-group" # cl "w-100"

-- | One row of a `listGroup`.
listGroupItem :: Ocular (PUI Web)
listGroupItem w = (el "li" $ w) # cl "list-group-item"

-- | A **badge** (`.badge.text-bg-*`): a value called out inline — a
-- | count, a figure, a status word. `variant` is the contextual colour
-- | ("primary", "success", "danger", …).
badge :: String -> Ocular (PUI Web)
badge variant w = span w # cl "badge" # cl ("text-bg-" <> variant)

-- Entry point

-- | Mount the app in the page's `<body>`: `PUI.Web.HTML.body` unchanged,
-- | under this vocabulary's name (same signature, same closed-app
-- | demand), since Bootstrap's reboot needs nothing on the page body.
body :: forall o. PUI Web {} o -> Effect Unit
body = HTML.body

-- | The **indeterminate progress bar** (the striped, animated
-- | `.progress-bar`): the action slot, fed a run's `started` and `ended`
-- | and shown between them (`indeterminateLinearProgress # action @l
-- | f`). No label: it names no field and no case.
indeterminateLinearProgress :: PUI Web [ started :: {}, ended :: {} ] {}
indeterminateLinearProgress = fromHandles do
  _ <- element "div" do
    element "div" (pure unit)
    bar <- gets _.sibling
    liftEffect $ setAttribute bar "class" "progress-bar progress-bar-striped progress-bar-animated w-100"
  node <- gets _.sibling
  liftEffect do
    setAttribute node "class" "progress"
    setAttribute node "role" "progressbar"
    setAttribute node "style" hiddenStyle
  pure $ variantToRecordWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "width: 100%; min-width: 200px;"
  hiddenStyle = "width: 100%; min-width: 200px; visibility: hidden;"
