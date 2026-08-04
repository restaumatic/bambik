-- | The **Bootstrap** vocabulary (https://getbootstrap.com) — the CSS-only
-- | member of the family: Bootstrap ships a stylesheet rather than
-- | components, so every control here is a plain HTML element wearing
-- | Bootstrap's documented classes. Names and signatures match the Material
-- | modules wherever both catalogues have the concept, so a screen changes
-- | design system by changing this one import.
-- |
-- | **The page must load** the Bootstrap 5 stylesheet. No scripts and no
-- | fonts — the design system rides the system font stack.
-- |
-- | The catalogue: `textField`, `sliderLive` and `toggleSwitch` to enter
-- | values, `select` to choose one, `button` to act, `toast` to say what
-- | happened, `progress` to show a figure, and `card`,
-- | `listGroup`/`listGroupItem` and `badge` for structure. Typography is
-- | deliberately absent: Bootstrap styles plain HTML, so the `PUI.Web.HTML`
-- | elements are the type scale.
module PUI.Web.Bootstrap
  ( badge
  , button
  , card
  , listGroup
  , listGroupItem
  , progress
  , select
  , sliderLive
  , textField
  , toast
  , toggleSwitch
  ) where

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
import Data.Profunctor.Row.RecordToRecord (field, projected)
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (PUI, Ocular, constantly)
import PUI.Web.HTML (cl, clicked, div, el, h5, label, span, staticText, text, (:=))
import PUI.Web (Node, Web, addEventListener, attribute, element, getChecked, getValue, isFocused, setAttribute, setChecked, setValue, uniqueId)
import Type.Proxy (Proxy(..))

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
-- (focus-guarded text field, per-feed display echo, `Just`-only echo on
-- the type-changing selector). Two-sorted, same citizenship, and — where
-- the concept exists in both catalogs — the same names and signatures:
--
--   * **components** — widgets with a model interface, every one a citizen
--     of exactly one row direction:
--       `×→×` editors — `textField @l` (`.form-control`), `sliderLive @l`
--         (`.form-range` — the native range input emits per drag step;
--         Bootstrap has no commit-only slider; the label line carries a
--         live numeric readout, the counterpart of MD's labeled handle),
--         `toggleSwitch @l`
--         (`.form-check.form-switch`), and the type-changing `select @l`
--         (`.form-select`, `{ value :: Maybe a } → { value :: a }`);
--       `×→×` displays — `progress` (`{ value :: Number } → {}`, the
--         filled fraction 0–1 — `.progress` over `.progress-bar`);
--       `×→+` events — `button @l` (`.btn.btn-primary`);
--       `+→×` statuses — `toast @l` (`.toast` fixed at the bottom, shown
--         on feed and dismissed by the hand-wired timer) — canonical
--         `[ event :: String ]` in, adopted via `# forCase @l`.
--   * **oculars** — shape-preserving decorators: `card { caption }`
--     (`.card` with a `.card-title`), `listGroup`/`listGroupItem`
--     (`.list-group`), `badge { variant }` (`.badge.text-bg-*`).
--     Typography is deliberately absent: Bootstrap styles plain HTML, so
--     the `PUI.Web.HTML` element oculars are the typography.
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (`rmap` a total
-- `Model -> Model` after `completed`), not in a leaf bracket.

-- UIs

-- | The **primary button**: the screen's action. It reports on click,
-- | carrying the data it was showing, under the name the app gives the
-- | action — `button { label: "Apply" } # asCase @"applied"`.
button :: forall r. { label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
button config = recordToCase @"clicked" $ eventLeaf $
  (el "button" >>> "type" := "button" $ staticText config.label) # cl "btn" # cl "btn-primary"

-- the click-emitter protocol over any `{} → {}` element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld)
eventLeaf :: forall a. PUI Web {} {} -> PUI Web a a
eventLeaf chrome = clicked (chrome # constantly {})

-- | The **text field**: a single-line input under its label. Shows the
-- | string it is given and reports each edit; typing is never interrupted
-- | by values arriving from elsewhere. Attach it to a field of the model
-- | with `# asField @l`.
textField :: { label :: String } -> PUI Web { value :: String } { value :: String }
textField config = field @"value" $ div >>> "style" := "width: 100%;" $ wrap do
  -- focus-guarded like `Web.input`: model updates never clobber the field
  -- being typed in, but still echo so merge gates keep flowing
  _ <- unwrap ((label $ staticText config.label) # cl "form-label")
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
-- | compile error rather than a wrong screen. A `step` makes it discrete,
-- | no step continuous.
-- |
-- | It reports on **every drag step**, following the thumb — the plain
-- | range input has no commit-only behaviour, hence the name — so whatever
-- | it drives should be cheap to redo, or be `debounced` downstream. The
-- | current number is shown at the end of the label line, since the control
-- | has no readout of its own.
sliderLive :: { label :: String } -> PUI Web { value :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } { value :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
sliderLive config = field @"value" $ div >>> "style" := "width: 100%;" $ wrap do
  readout <- unwrap $ (label $ wrap do
      _ <- unwrap (span $ staticText config.label)
      unwrap ((span $ text) # cl "text-body-secondary")
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
        setAttribute node "step" (case q.step of
          Just s -> show s
          Nothing -> "any")
        setValue node (show q.current)
        readout.toUser { value: toString q.current }
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop q
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        void $ addEventListener "input" node $ const do
          value <- getValue node
          mq <- Ref.read qRef
          for_ mq \q -> for_ (Number.fromString value) \v -> prop (q { current = v })
    }

-- | The **select**: one choice out of a list, under its label. Until the
-- | user picks there is nothing to show, so the field arrives as "maybe a
-- | choice" and leaves as the choice itself — say which with `# optional`
-- | or `# required`. The options belong to the control, not to the model.
select :: forall a. Eq a => { label :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
select config options = field @"value" $ div >>> "style" := "width: 100%;" $ wrap do
  _ <- unwrap ((label $ staticText config.label) # cl "form-label")
  element "select" (void $ unwrap (optionLeaves))
  node <- gets _.sibling
  liftEffect $ setAttribute node "class" "form-select"
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ void $ addEventListener "change" node $ const do
    picked <- getValue node
    for_ (Int.fromString picked >>= (options !! _)) \o -> do
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop o.value
  pure
    { toUser: \ma -> do
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setValue node (show idx)
          Nothing -> setValue node ""
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  optionLeaves :: PUI Web {} {}
  optionLeaves = wrap do
    forWithIndex_ options \idx o -> do
      element "option" (void $ unwrap (staticText o.label))
      optionNode <- gets _.sibling
      liftEffect $ setAttribute optionNode "value" (show idx)
    pure { toUser: mempty, fromUser: \prop -> prop {} }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | The label is part of the target, so clicking the words toggles it too.
toggleSwitch :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
toggleSwitch config = field @"value" $ (div $ wrap do
  inputId <- liftEffect uniqueId
  element "input" (pure unit)
  node <- gets _.sibling
  liftEffect do
    setAttribute node "class" "form-check-input"
    setAttribute node "type" "checkbox"
    setAttribute node "role" "switch"
    setAttribute node "id" inputId
  _ <- unwrap ((label >>> "for" := inputId $ staticText config.label) # cl "form-check-label")
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ void $ addEventListener "change" node $ const do
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
    }) # cl "form-check" # cl "form-switch"

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a share, a quota, a
-- | ratio — written as `progress # projected fraction`, with the business
-- | function deciding what the fraction means.
progress :: PUI Web { value :: Number } {}
progress = wrap do
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
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setAttribute barNode "style" ("width: " <> show (round (r.value * 100.0)) <> "%;")
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | The **toast**: a brief message at the bottom of the screen that
-- | dismisses itself after a few seconds, for something that has just
-- | happened and needs no reply. It never interrupts.
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the toast is built — `toast # forCase @"applied" appliedLine` — and let
-- | the event carry the bare facts.
toast :: PUI Web [ event :: String ] {}
toast = wrap do
  w <- unwrap $ (el "div" >>> "role" := "status"
    >>> "style" := "position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000;" $
      (div $ text # projected eventText) # cl "toast-body")
    # cl "toast" # cl "text-bg-primary" # cl "border-0"
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        autoDismiss node "show" 5000
    , fromUser: w.fromUser
    }

-- UIOculars

-- | A **card**: a surface holding one subject's content, captioned at the
-- | top. It stacks its children with even spacing, so a form or a summary
-- | can be dropped in without spacing each row by hand.
card :: { caption :: String } -> Ocular (PUI Web)
card config content =
  (div $ (div $ wrap do
      _ <- unwrap ((h5 $ staticText config.caption) # cl "card-title")
      unwrap content
    ) # cl "card-body" # cl "d-flex" # cl "flex-column" # cl "align-items-start" # cl "gap-3"
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
badge :: { variant :: String } -> Ocular (PUI Web)
badge config w = span w # cl "badge" # cl ("text-bg-" <> config.variant)

-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = Variant.on (Proxy @"event") identity Variant.case_

-- Private

foreign import autoDismiss :: Node -> String -> Int -> Effect Unit
