-- | The **Bootstrap** vocabulary (https://getbootstrap.com) — the CSS-only
-- | member of the family: Bootstrap ships a stylesheet rather than
-- | components, so every control here is a plain HTML element wearing
-- | Bootstrap's documented classes. Names and signatures match the Material
-- | modules wherever both catalogues have the concept, so a screen changes
-- | design system by changing this one import.
-- |
-- | **The page must load** the Bootstrap 5 stylesheet. No scripts and no
-- | fonts — the design system rides the system font stack. Its `body` is
-- | `PUI.Web.HTML.body` under this vocabulary's name: the reboot needs
-- | nothing on the page body, so the entry line reads the same and does no
-- | more.
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
  , listGroup
  , listGroupItem
  , indeterminateLinearProgress
  , progress
  , select
  , selectUnpicked
  , selectOptional
  , sliderLive
  , textField
  , toast
  , toggleSwitch
  ) where

import Data.Profunctor.Row.Structural (withStructuralEq)
import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (fromString) as Int
import Data.Int (round)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Number (fromString) as Number
import Data.Number.Format (toString)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row (atCase)
import Data.Variant (match) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import PUI (folding, Ocular, PUI)
import PUI.Web.HTML (div, label, span)
import PUI.Web.HTML (body) as HTML
import PUI.Web (selectedAt, selectedOptionalAt, selectedUnpickedAt, Web, attribute, autoDismiss, cl, displayWiring, editorWiring, el, element, eventLeaf, getChecked, getValue, isFocused, listenNode, selectorWiring, setAttribute, setChecked, setValue, staticText, statusWiring, text, statusText, uniqueId, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)

-- Implementation notes — the reference above is the contract.
--
-- Bootstrap (https://getbootstrap.com) components implemented as
-- PUI Web/Ocular (PUI Web) datatypes — a design-system vocabulary beside
-- `PUI.Web.MDC2`/`PUI.Web.MDC3`/`PUI.Web.Shoelace`/`PUI.Web.Fluent`, proving the
-- vocabularies interchangeable, and the **CSS-only** member of the family:
-- Bootstrap is a stylesheet, not a component runtime, so every leaf is a
-- native element (`<input>`, `<select>`, `<button>`) dressed in the
-- documented classes (`form-control`, `form-select`, `btn btn-primary`) —
-- no custom elements, no foundation instances, and no FFI beyond the
-- toast's dismissal timer (the one behavior Bootstrap's own JS plugin
-- would supply). The leaf-echo protocols are the same as the MDC modules'
-- (focus-guarded text field, per-feed display echo, a selection
-- editor answering every feed with its row). Two-sorted, same citizenship, and — where
-- the concept exists in both catalogs — the same names and signatures:
--
--   * **components** — UI components with a model interface, every one a citizen
--     of exactly one row shape:
--       `×→×` editors — `textField @l` (`.form-control`), `sliderLive @l`
--         (`.form-range` — the native range input emits per drag step;
--         Bootstrap has no commit-only slider; the label line carries a
--         live numeric readout, the counterpart of MD's labeled handle),
--         `toggleSwitch @l`
--         (`.form-check.form-switch`), and the type-changing `select @l`
--         (`.form-select`, `{ l :: f } → { l :: f }`, `f` the option itself,
--         or a variant around it for the `…Unpicked`/`…Optional` siblings);
--       `×→×` displays — `progress` (`{ value :: Number } → {}`, the
--         filled fraction 0–1 — `.progress` over `.progress-bar`);
--       `×→+` events — `button @l` (`.btn.btn-primary`);
--       `+→×` statuses — `toast` (`.toast` fixed at the bottom, shown
--         on feed and dismissed by the hand-wired timer) — each taking
--         its business case and fold, the case label its copy (`toast @"Application received" identity`).
--   * **oculars** — shape-preserving decorators: `card`
--     (`.card`, a plain surface), `listGroup`/`listGroupItem`
--     (`.list-group`), `badge variant` (`.badge.text-bg-*`).
--     Typography is deliberately absent: Bootstrap styles plain HTML, so
--     the `PUI.Web.HTML` element oculars are the typography.
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (a `settled`
-- normalization on the whole-row stage), not in a leaf bracket.

-- UIs

-- | The **primary button**: the screen's action. It reports on click,
-- | carrying the data it was showing, under the name the app gives the
-- | action — `button @"Apply for the loan"`. The label defaults to
-- | the case label verbatim (`label:` overrides with real copy).
button :: forall @l r v. IsSymbol l => Cons l { | r } () v => PUI Web { | r } [ | v ]
button = let config = { label: reflectSymbol (Proxy @l) } :: { label :: String } in eventLeaf @l $
  (el "button" >>> "type" := "button" $ staticText config.label) # cl "btn" # cl "btn-primary"

-- | The **text field**: a single-line input under its label. Shows the
-- | string it is given and reports each edit; typing is never interrupted
-- | by values arriving from elsewhere. A whole-row citizen: fed the wide
-- | row, it edits field `l` and carries the rest.
textField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textField = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ div >>> "style" := "width: 100%;" $ wrap do
  _ <- unwrap ((label $ staticText config.label) # cl "form-label")
  element "input" (pure unit)
  attribute "type" "text"
  attribute "class" "form-control"
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "input"
    , read: \_ -> getValue node
    }

-- | The **slider**: a quantity chosen by feel, where the range matters more
-- | than the exact number — a rate, a term, an amount.
-- |
-- | The range is part of the quantity, not part of the screen:
-- | `{ current, min, max, step }` travels together as one business datum, so
-- | limits come from the data and can change while the app runs — a slider
-- | is never silently out of range, and a range nobody supplied is a
-- | compile error rather than a wrong screen. `step` is `.discrete n`
-- | or `.continuous {}`, named like every other two-state field.
-- |
-- | It reports on **every drag step**, following the thumb — the plain
-- | range input has no commit-only behaviour, hence the name — so whatever
-- | it drives should be cheap to redo, or be `debounced` downstream. The
-- | current number is shown at the end of the label line, since the control
-- | has no readout of its own.
sliderLive :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
sliderLive = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ div >>> "style" := "width: 100%;" $ wrap do
  readout <- unwrap $ (label $ wrap do
      _ <- unwrap (span $ staticText config.label)
      unwrap ((span $ text _.readout) # cl "text-body-secondary")
    ) # cl "form-label" # cl "d-flex" # cl "justify-content-between"
  -- the readout is written, never listened to; text's echo needs a listener
  liftEffect $ readout.fromUser \_ -> pure unit
  element "input" (pure unit)
  attribute "type" "range"
  attribute "class" "form-range"
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: \q -> do
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        setAttribute node "step" (Variant.match { discrete: show, continuous: \_ -> "any" } q.step)
        setValue node (show q.current)
        readout.toUser { readout: toString q.current }
    , held: pure false
    , reports: listenNode node "input"
    , read: \q -> getValue node <#> \value -> q { current = fromMaybe q.current (Number.fromString value) }
    }

-- | The **select**: one choice out of a list, under its label.
-- | The field holds the option itself, so the model always has one.
-- | Two siblings hold a variant instead: `selectUnpicked @l @c` for a
-- | choice owed but not yet made, `selectOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- | The options belong to the control, not to the model.
select :: forall @l a b r. IsSymbol l => Cons l a b r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
select options = withStructuralEq @a (selectOf @l false (selectedAt @l) options)

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
selectUnpicked :: forall @l @c a b1 v b2 r. IsSymbol l => IsSymbol c => Cons c a b1 v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectUnpicked options = withStructuralEq @a (selectOf @l false (selectedUnpickedAt @l @c) options)

-- | `select` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | an empty first option clears it, storing `n` again.
selectOptional :: forall @l @c @n a b1 t v b2 r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} t v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectOptional options = withStructuralEq @a (selectOf @l true (selectedOptionalAt @l @c @n) options)

selectOf :: forall @l a i o. IsSymbol l => Eq a => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> Array { value :: a, label :: String } -> PUI Web i o
selectOf clearable lift options = let config = { label: reflectSymbol (Proxy @l) } in lift $ "name" := reflectSymbol (Proxy @l) $ div >>> "style" := "width: 100%;" $ wrap do
  _ <- unwrap ((label $ staticText config.label) # cl "form-label")
  element "select" (void $ unwrap (optionLeaves))
  node <- gets _.sibling
  liftEffect $ setAttribute node "class" "form-select"
  liftEffect $ selectorWiring options
    { show: setValue node <<< maybe "" show
    , reports: listenNode node "change"
    , read: Int.fromString <$> getValue node
    }
  where
  optionLeaves :: PUI Web {} {}
  optionLeaves = wrap do
    when clearable do
      element "option" (pure unit)
      noneNode <- gets _.sibling
      liftEffect $ setAttribute noneNode "value" ""
    forWithIndex_ options \idx o -> do
      element "option" (void $ unwrap (staticText o.label))
      optionNode <- gets _.sibling
      liftEffect $ setAttribute optionNode "value" (show idx)
    pure { toUser: mempty, fromUser: \prop -> prop {} }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | The label is part of the target, so clicking the words toggles it too.
toggleSwitch :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
toggleSwitch = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (div $ wrap do
  inputId <- liftEffect uniqueId
  element "input" (pure unit)
  node <- gets _.sibling
  liftEffect do
    setAttribute node "class" "form-check-input"
    setAttribute node "type" "checkbox"
    setAttribute node "role" "switch"
    setAttribute node "id" inputId
  _ <- unwrap ((label >>> "for" := inputId $ staticText config.label) # cl "form-check-label")
  liftEffect $ editorWiring
    { show: setChecked node
    , held: pure false
    , reports: listenNode node "change"
    , read: \_ -> getChecked node
    }) # cl "form-check" # cl "form-switch"

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a share, a quota, a
-- | ratio — `progress progressFraction`: the value is a **read
-- | function** like every display's, since a fraction is derived (a ratio
-- | of source fields), not state.
progress
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
progress f = wrap do
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
  liftEffect $ displayWiring \r -> setAttribute barNode "style" ("width: " <> show (round (f r * 100.0)) <> "%;")

-- | The **toast**: a brief message at the bottom of the screen that
-- | dismisses itself after a few seconds, for something that has just
-- | happened and needs no reply. It never interrupts.
-- |
-- | A status **tells its case**: its caption is the case label verbatim,
-- | so the outcome is named as what it tells (`toast @"Application
-- | received" identity`) and the payload is read by nothing but the fold. A
-- | `+→×` leaf like `fold @l f`, it takes its fold of the payload.
toast
  :: forall @l a v r
   . IsSymbol l
  => Cons l a () v
  => (a -> { | r })
  -> PUI Web [ | v ] { | r }
toast f = toastFace (reflectSymbol (Proxy @l)) # atCase @l # folding @l f

toastFace :: forall a r. String -> PUI Web a { | r }
toastFace caseLabel = wrap do
  w <- unwrap $ (el "div" >>> "role" := "status"
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

-- | A **card**: a surface holding one subject's content. It stacks its
-- | children with even spacing, so a form or a summary can be dropped in
-- | without spacing each row by hand.
-- |
-- | A plain ocular, with no config of its own: a card is a *surface*, and a
-- | heading of its own is ordinary typography placed in its content.
-- | Bootstrap does document a `card-title` class for that heading — style a
-- | content heading with it where a card wants one (`h5 … # cl "card-title"`).
card :: Ocular (PUI Web)
card content =
  (div $ (div $ content)
    # cl "card-body" # cl "d-flex" # cl "flex-column" # cl "align-items-start" # cl "gap-3"
  ) # cl "card"

-- | A **list group**: rows of `listGroupItem`s sharing one bordered
-- | surface — a readout of figures, a set of related lines.
listGroup :: Ocular (PUI Web)
listGroup w = (el "ul" $ w) # cl "list-group" # cl "w-100"

-- | One row of a `listGroup`.
listGroupItem :: Ocular (PUI Web)
listGroupItem w = (el "li" $ w) # cl "list-group-item"

-- | A **badge**: a value called out inline — a count, a figure, a status
-- | word. `variant` is the contextual colour ("primary", "success",
-- | "danger", ...), so the badge carries meaning as well as emphasis.
badge :: String -> Ocular (PUI Web)
badge variant w = span w # cl "badge" # cl ("text-bg-" <> variant)

-- Entry point

-- | Mount the app in the page's `<body>` — `PUI.Web.HTML.body` unchanged,
-- | under this vocabulary's name: Bootstrap's reboot needs nothing on the
-- | page body, so there is no dressing to do, and the entry line still
-- | reads the same as in every other vocabulary (same signature, same
-- | closed-app demand), switching design system with the rest of the import.
body :: forall o. PUI Web {} o -> Effect Unit
body = HTML.body

-- | The **indeterminate progress bar**: a status fed a run's `started` and
-- | `ended`, shown between them — what `PUI.action`'s slot dispatches
-- | (`indeterminateLinearProgress # action submit`). No label: it names no
-- | field and no case.
indeterminateLinearProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateLinearProgress = wrap do
  _ <- element "div" do
    element "div" (pure unit)
    bar <- gets _.sibling
    liftEffect $ setAttribute bar "class" "progress-bar progress-bar-striped progress-bar-animated w-100"
  node <- gets _.sibling
  liftEffect do
    setAttribute node "class" "progress"
    setAttribute node "role" "progressbar"
    setAttribute node "style" hiddenStyle
  pure $ statusWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "width: 100%; min-width: 200px;"
  hiddenStyle = "width: 100%; min-width: 200px; visibility: hidden;"
