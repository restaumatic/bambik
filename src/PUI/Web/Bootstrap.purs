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
import Data.Array ((!!), findIndex)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Foldable (for_)
import Data.Int (fromString) as Int
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Number (fromString) as Number
import Data.Number.Format (toString)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row.VariantToVariant (forCase)
import Data.Variant (case_, match, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Data.Profunctor.Row (widenRecordInput)
import PUI (Ocular, PUI)
import PUI.Web.HTML (div, label, span)
import PUI.Web.HTML (body) as HTML
import PUI.Web (selectedAt, selectedOptionalAt, selectedUnpickedAt, Node, OptCaption(..), Web, addEventListener, attribute, cl, clicked, el, element, getChecked, getValue, isFocused, setAttribute, setChecked, setValue, staticString, text, textOf, uniqueId, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)

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
--         its business case and copy function (`toast @"booked" bookedLine`).
--   * **oculars** — shape-preserving decorators: `card { caption }`
--     (`.card` with a `.card-title`), `listGroup`/`listGroupItem`
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
-- | action — `button @"Apply for the loan" {}`. The label defaults to
-- | the case label verbatim (`label:` overrides with real copy).
button :: forall @l provided r v. IsSymbol l => Cons l { | r } () v => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } [ | v ]
button provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { label :: String } in eventLeaf @l $
  (el "button" >>> "type" := "button" $ staticString config.label) # cl "btn" # cl "btn-primary"

-- the click-emitter protocol over any `{}`-output element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld)
eventLeaf :: forall @l r v. IsSymbol l => Cons l { | r } () v => PUI Web {} {} -> PUI Web { | r } [ | v ]
eventLeaf chrome = clicked @l identity (widenRecordInput chrome)

-- | The **text field**: a single-line input under its label. Shows the
-- | string it is given and reports each edit; typing is never interrupted
-- | by values arriving from elsewhere. Attach it to a field of the model
-- | with `# asField @l`.
textField :: forall @l r b provided. IsSymbol l => Cons l String b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
textField provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ div >>> "style" := "width: 100%;" $ wrap do
  -- focus-guarded like `Web.input`: model updates never clobber the field
  -- being typed in, but still echo so the channel stays live
  _ <- unwrap ((label $ staticString config.label) # cl "form-label")
  element "input" (pure unit)
  attribute "type" "text"
  attribute "class" "form-control"
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
        void $ addEventListener "input" node $ const do
          value <- getValue node
          prop value
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
sliderLive :: forall @l r b provided. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
sliderLive provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ div >>> "style" := "width: 100%;" $ wrap do
  readout <- unwrap $ (label $ wrap do
      _ <- unwrap (span $ staticString config.label)
      unwrap ((span $ text _.readout) # cl "text-body-secondary")
    ) # cl "form-label" # cl "d-flex" # cl "justify-content-between"
  -- the readout is written, never listened to; text's echo needs a listener
  liftEffect $ readout.fromUser \_ -> pure unit
  element "input" (pure unit)
  attribute "type" "range"
  attribute "class" "form-range"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  qRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \q -> do
        Ref.write (Just q) qRef
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        setAttribute node "step" (Variant.match { discrete: show, continuous: \_ -> "any" } q.step)
        setValue node (show q.current)
        readout.toUser { readout: toString q.current }
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop q
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "input" node $ const do
          value <- getValue node
          mq <- Ref.read qRef
          for_ mq \q -> for_ (Number.fromString value) \v -> prop (q { current = v })
    }

-- | The **select**: one choice out of a list, under its label.
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
-- | an empty first option clears it, storing `n` again.
selectOptional :: forall @l @c @n a b1 t v b2 r provided. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} t v => Cons l [ | v ] b2 r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectOptional provided options = withStructuralEq @a (selectWith @l true (selectedOptionalAt @l @c @n) provided options)

selectWith :: forall @l a i o provided. IsSymbol l => Eq a => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> { | provided } -> Array { value :: a, label :: String } -> PUI Web i o
selectWith clearable lift provided options = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in lift $ "name" := reflectSymbol (Proxy @l) $ div >>> "style" := "width: 100%;" $ wrap do
  _ <- unwrap ((label $ staticString config.label) # cl "form-label")
  element "select" (void $ unwrap (optionLeaves))
  node <- gets _.sibling
  liftEffect $ setAttribute node "class" "form-select"
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ void $ addEventListener "change" node $ const do
    picked <- getValue node
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop (_.value <$> (Int.fromString picked >>= (options !! _)))
  pure
    { toUser: \ma -> do
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setValue node (show idx)
          Nothing -> setValue node ""
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  optionLeaves :: PUI Web {} {}
  optionLeaves = wrap do
    when clearable do
      element "option" (pure unit)
      noneNode <- gets _.sibling
      liftEffect $ setAttribute noneNode "value" ""
    forWithIndex_ options \idx o -> do
      element "option" (void $ unwrap (staticString o.label))
      optionNode <- gets _.sibling
      liftEffect $ setAttribute optionNode "value" (show idx)
    pure { toUser: mempty, fromUser: \prop -> prop {} }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | The label is part of the target, so clicking the words toggles it too.
toggleSwitch :: forall @l r b provided. IsSymbol l => Cons l Boolean b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
toggleSwitch provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (div $ wrap do
  inputId <- liftEffect uniqueId
  element "input" (pure unit)
  node <- gets _.sibling
  liftEffect do
    setAttribute node "class" "form-check-input"
    setAttribute node "type" "checkbox"
    setAttribute node "role" "switch"
    setAttribute node "id" inputId
  _ <- unwrap ((label >>> "for" := inputId $ staticString config.label) # cl "form-check-label")
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ void $ addEventListener "change" node $ const do
    b <- getChecked node
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop b
  pure
    { toUser: \b -> do
        setChecked node b
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }) # cl "form-check" # cl "form-switch"

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a share, a quota, a
-- | ratio — `progress @"Progress" progressFraction`: the value is a **read
-- | function** like every display's, since a fraction is derived (a ratio
-- | of source fields), not state. The label is the accessible name only,
-- | so it is copy, never a field reference.
progress
  :: forall @l reads
   . IsSymbol l
  => ({ | reads } -> Number) -> PUI Web { | reads } {}
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
    setAttribute node "aria-label" (reflectSymbol (Proxy @l))
    setAttribute node "style" "width: 100%; min-width: 200px;"
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setAttribute barNode "style" ("width: " <> show (round (f r * 100.0)) <> "%;")
        -- display echo (like `text`): the feed's answer, inert to gates
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The **toast**: a brief message at the bottom of the screen that
-- | dismisses itself after a few seconds, for something that has just
-- | happened and needs no reply. It never interrupts.
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the toast is built — `toast @"applied" appliedLine` — and let
-- | the event carry the bare facts.
toast
  :: forall @l a v r
   . IsSymbol l
  => Cons l a () v
  => (a -> String)
  -> PUI Web [ | v ] { | r }
toast copy = toastFace # forCase @l copy

toastFace :: forall r. PUI Web [ event :: String ] { | r }
toastFace = wrap do
  w <- unwrap $ (el "div" >>> "role" := "status"
    >>> "style" := "position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000;" $
      (div $ textOf eventText) # cl "toast-body")
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

-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = Variant.on (Proxy @"event") identity Variant.case_

-- Private

foreign import autoDismiss :: Node -> String -> Int -> Effect Unit

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
-- | field and no case (2026-10-07, with the Material twins).
indeterminateLinearProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateLinearProgress = wrap do
  _ <- element "div" do
    element "div" (pure unit)
    bar <- gets _.sibling
    liftEffect $ setAttribute bar "class" "progress-bar progress-bar-striped progress-bar-animated w-100"
  node0 <- gets _.sibling
  liftEffect do
    setAttribute node0 "class" "progress"
    setAttribute node0 "role" "progressbar"
    setAttribute node0 "style" hiddenStyle
  node <- gets _.sibling
  pure
    { toUser: Variant.match
        { started: \_ -> setAttribute node "style" visibleStyle
        , ended: \_ -> setAttribute node "style" hiddenStyle }
    , fromUser: \_ -> pure unit
    }
  where
  visibleStyle = "width: 100%; min-width: 200px;"
  hiddenStyle = "width: 100%; min-width: 200px; visibility: hidden;"
