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
-- | CDN, whose base path this vocabulary's `body` sets at mount. No webfont
-- | is needed — Shoelace uses the system font stack.
-- |
-- | The catalogue: `textField`/`textArea`, `rating`, `sliderLive` and
-- | `toggleSwitch` to enter values, `select` to choose one, `button` to
-- | act, `toast` to say what happened, `progressBar` to show a figure,
-- | `card` and `divider` for structure, `body` to mount the app.
-- | Typography is deliberately absent: Shoelace styles plain HTML, so the
-- | `PUI.Web.HTML` elements are the type scale.
module PUI.Web.Shoelace
  ( acknowledged
  , confirmed
  , body
  , button
  , card
  , divider
  , indeterminateLinearProgress
  , option
  , progressBar
  , rating
  , select
  , selectFrom
  , selectUnpicked
  , selectOptional
  , sliderLive
  , sliderLive'
  , textArea
  , textField
  , textFieldFrom
  , textField'
  , toast
  , toggleSwitch
  ) where

import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Variant (inj, match) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import Effect.Ref as Ref
import Data.Foldable (for_)
import PUI (introduced, fromHandles, handles, Ocular, PUI, optionWiring, recordToRecordWiring, variantToRecordWiring, variantToVariantWiring, statusOver, confirmedOver, acknowledgedOver, eventLeaf)
import PUI.Web.HTML (div, span)
import PUI.Web.HTML (body) as HTML
import PUI.Web (chrome, fieldLeaf, Node, OptCaption(..), Web, attribute, el, element, getChecked, getValue, isFocused, listenNode, removeAttribute, setAttribute, setChecked, setValue, staticHTML, staticText, statusText, separating, invokeMethod, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons, class Lacks)
import Data.Symbol (class IsSymbol, reflectSymbol)
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)

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
-- fields, per-feed display echo, a selection editor answering every feed with its row). Two-sorted, same citizenship, and — where the concept exists
-- in both catalogs — the same names and signatures (`textField` carries
-- Shoelace's plain `label` instead of MD's `floatingLabel`; the catalog has
-- no fill/outline split), so a demo switches design systems by switching
-- the import:
--
--   * **components** — UI components with a model interface, every one a citizen
--     of exactly one row shape:
--       `×→×` editors — `textField @l`, `textArea @l`, `rating @l` (the
--         star editor, `{ value :: Number }` — Shoelace's distinctive
--         catalog entry), `sliderLive @l` (`<sl-range>` — reports per drag
--         step, the value shown by the control's own tooltip),
--         `toggleSwitch @l` (`<sl-switch>`), and the
--         type-changing `select @l` (`{ l :: f } → { l :: f }`, `f` the option itself, or a
--         variant around it for the `…Unpicked`/`…Optional` siblings), a
--         group over `option @c` leaves (`<sl-option>`, `+→+` over its one
--         case) merged `VariantToVariant.do`;
--       `×→×` displays — `progressBar` (`<sl-progress-bar>`,
--         `{ value :: Number } → {}`, the filled fraction 0–1);
--       `×→+` events — `button @l` (`<sl-button variant="primary">`);
--       `+→×` statuses — `toast` (`<sl-alert>` shown on feed,
--         auto-dismissing via its own `duration`) — each taking
--         its business case and fold, the case label its copy (`toast @"Review submitted" identity`).
--   * **oculars** — shape-preserving decorators: `card` (`<sl-card>`, a
--     plain surface). Typography is deliberately absent:
--     Shoelace styles plain HTML through its tokens, so the `PUI.Web.HTML`
--     element oculars are the typography.
--   * plus **announcing statics** (`{}`-output chrome with a face, at any row):
--     `divider` (`<sl-divider>`).
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (a `settled`
-- normalization on the whole-row stage), not in a leaf bracket.

-- UIs

-- | The **primary button**: the screen's action. It reports on click,
-- | carrying the data it was showing, under the name the app gives the
-- | action — `button @"Submit the review"`. The label defaults to
-- | the case label verbatim (`label:` overrides with real copy).
button :: forall @l r b v. IsSymbol l => Cons l { | r } b v => PUI Web { | r } [ | v ]
button = let config = { label: reflectSymbol (Proxy @l) } :: { label :: String } in eventLeaf @l $
  el "sl-button" >>> "variant" := "primary" $ staticText config.label

-- | The **text field**: a labelled single-line input. Shows the string it
-- | is given and reports each edit; typing is never interrupted by values
-- | arriving from elsewhere. A whole-row citizen: fed the wide row, it
-- | edits field `l` and carries the rest.
textField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textField = fieldLeaf @l $ inputLeaf (reflectSymbol (Proxy @l)) ""

-- | `textField` in its **introducing form**: the field is not in the row fed
-- | but stated here, `init` of the rest of the row its blank, for a
-- | knotless form with no seed. See `PUI.introduced`.
textFieldFrom :: forall @l r b. IsSymbol l => Lacks l b => Cons l String b r => ({ | b } -> String) -> PUI Web { | b } { | r }
textFieldFrom init = introduced @l init (textField @l)

-- | `textField` with a `hint` under it — a format or an example the label
-- | is not (`textField' @"Start date" { hint: "DD.MM.YYYY" }`), so the
-- | label stays the field's name. Shown as Shoelace's help text.
textField' :: forall @l r b. IsSymbol l => Cons l String b r => { hint :: String } -> PUI Web { | r } { | r }
textField' { hint } = fieldLeaf @l $ inputLeaf (reflectSymbol (Proxy @l)) hint

inputLeaf :: String -> String -> PUI Web String String
inputLeaf lbl hint = fromHandles do
  -- the shadow input keeps the host as `activeElement`, so the focus guard
  -- reads the host
  element "sl-input" (pure unit)
  attribute "label" lbl
  when (hint /= "") $ attribute "help-text" hint
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "sl-input"
    , read: \_ -> getValue node
    }

-- | The **multi-line text field**, `rows` lines tall — a note, a review, a
-- | message. Otherwise `textField`.
textArea :: forall @l r b provided. IsSymbol l => Cons l String b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String, rows :: Int } => { | provided } -> PUI Web { | r } { | r }
textArea provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in fieldLeaf @l $ fromHandles do
  element "sl-textarea" (pure unit)
  attribute "label" config.label
  attribute "rows" (show config.rows)
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "sl-input"
    , read: \_ -> getValue node
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
rating :: forall @l r b. IsSymbol l => Cons l { current :: Number, max :: Int } b r => PUI Web { | r } { | r }
rating = let config = { label: reflectSymbol (Proxy @l) } in fieldLeaf @l $
  div >>> "style" := "display: inline-flex; flex-direction: column; gap: var(--sl-spacing-3x-small);" $ fromHandles do
    chrome (span >>> "style" := "font-size: var(--sl-input-label-font-size-medium); color: var(--sl-input-label-color);" $ staticText config.label)
    element "sl-rating" (pure unit)
    attribute "label" config.label
    node <- gets _.sibling
    liftEffect $ recordToRecordWiring
      { show: \q -> do
          setAttribute node "max" (show q.max)
          setNumberProp "value" node q.current
      , held: pure false
      , reports: listenNode node "sl-change"
      , read: \q -> getNumberProp "value" node <#> \v -> q { current = v }
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
-- | It reports on **every change**, following the drag — so whatever it
-- | drives should be cheap to redo, or be `debounced` downstream. The
-- | current number shows in the control's own tooltip while dragging.
sliderLive :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
sliderLive = fieldLeaf @l $ rangeLeaf (reflectSymbol (Proxy @l)) ""

-- | `sliderLive` whose value is read in a `unit` (`sliderLive' @"Amount"
-- | { unit: "€" }`), shown after the number in the control's tooltip, so
-- | the label stays the quantity's name.
sliderLive' :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => { unit :: String } -> PUI Web { | r } { | r }
sliderLive' config = fieldLeaf @l $ rangeLeaf (reflectSymbol (Proxy @l)) config.unit

rangeLeaf :: String -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
rangeLeaf lbl measure = fromHandles do
  element "sl-range" (pure unit)
  attribute "label" lbl
  attribute "style" "width: 100%; min-width: 240px;"
  node <- gets _.sibling
  liftEffect $ when (measure /= "") $ setTooltipUnit node measure
  liftEffect $ recordToRecordWiring
    { show: \q -> do
        setAttribute node "min" (show q.min)
        setAttribute node "max" (show q.max)
        Variant.match { discrete: \s -> setAttribute node "step" (show s), continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "value" node q.current
    , held: pure false
    , reports: listenNode node "sl-input"
    , read: \q -> getNumberProp "value" node <#> \v -> q { current = v }
    }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | The label sits beside it and is part of the target, so clicking the
-- | words toggles it too.
toggleSwitch :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
toggleSwitch = let config = { label: reflectSymbol (Proxy @l) } in fieldLeaf @l $ fromHandles do
  element "sl-switch" (chrome (staticText config.label))
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: setChecked node
    , held: pure false
    , reports: listenNode node "sl-change"
    , read: \_ -> getChecked node
    }

-- | The **dropdown**: one choice out of a list too long to lay out in the
-- | open. Its options are `option` leaves, one per case of the field,
-- | merged `VariantToVariant.do` in the order they are shown:
-- | `select @"Owned for" $ VariantToVariant.do option @"Under a month"; option @"Over a year"`.
-- | The block closes the field's cases; the select lifts it onto field `l`,
-- | which holds the option itself, so the model always has one.
-- | Two siblings hold a variant instead: `selectUnpicked @l @c` for a
-- | choice owed but not yet made, `selectOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- A selector group over one clearing of its options: every feed first
-- shows nothing picked, then the one option owning the fed case shows
-- itself, so a case no option owns (an unpicked one) shows nothing.
select :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
select options = fieldLeaf @l $ selectGroupOf @l false options

-- | `select` in its introducing form, as `textFieldFrom`.
selectFrom :: forall @l w b r. IsSymbol l => Lacks l b => Cons l [ | w ] b r => ({ | b } -> [ | w ]) -> PUI Web [ | w ] [ | w ] -> PUI Web { | b } { | r }
selectFrom init options = introduced @l init (select @l options)

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` holds the made choice, seeded at an unpicked case;
-- | nothing is checked until the user picks, and a pick cannot be taken
-- | back.
selectUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectUnpicked options = fieldLeaf @l $ selectGroupOf @l false (focusCase @c options)

-- | `select` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` holds the made choice and case `n` none, seeded at `n`;
-- | its clear button clears it, storing `n` again.
selectOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectOptional options = fieldLeaf @l $ selectGroupOf @l true (clearEmits @n (focusCase @c options))

selectGroupOf :: forall @l a. IsSymbol l => Boolean -> PUI Web a a -> PUI Web a a
selectGroupOf clearable options = el "sl-select" >>> "label" := reflectSymbol (Proxy @l) >>> clearing >>> "style" := "min-width: 240px;" $ fromHandles do
  selectNode <- gets _.parent
  inner <- handles options
  pure
    { toUser: \a -> setValue selectNode "" *> inner.toUser a
    , fromUser: inner.fromUser
    }
  where
  clearing :: Ocular (PUI Web)
  clearing = if clearable then "clearable" := "" else identity

-- The clearing of an optional select: its clear button empties the value,
-- which emits `n`.
clearEmits :: forall @n b v. IsSymbol n => Cons n {} b v => PUI Web [ | v ] [ | v ] -> PUI Web [ | v ] [ | v ]
clearEmits options = fromHandles do
  selectNode <- gets _.parent
  inner <- handles options
  pure
    { toUser: inner.toUser
    , fromUser: \prop -> do
        inner.fromUser prop
        listenNode selectNode "sl-change" do
          value <- getValue selectNode
          when (value == "") $ prop (Variant.inj (Proxy @n) {})
    }

-- | One option of a `select`: an `sl-option` captioned by its case. A
-- | Shoelace option value holds no space (values are space-separated for
-- | multiple selection), so the case rides it URI-encoded.
option :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
option = fromHandles do
  selectNode <- gets _.parent
  element "sl-option" (chrome (staticText (reflectSymbol (Proxy @c))))
  attribute "value" value
  liftEffect $ optionWiring @c
    { show: setValue selectNode value
    , reports: listenNode selectNode "sl-change"
    , picked: (_ == value) <$> getValue selectNode
    }
  where
  value = optionValue (reflectSymbol (Proxy @c))

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a quota, a share, a
-- | rating out of five — `progressBar progressFraction`: the
-- | value is a **read function** like every display's, since a fraction is
-- | derived (a ratio of source fields), not state.
progressBar
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
progressBar f = fromHandles do
  element "sl-progress-bar" (pure unit)
  attribute "style" "width: 100%; min-width: 200px;"
  node <- gets _.sibling
  -- sl-progress-bar runs 0–100
  liftEffect $ recordToRecordWiring { show: \r -> setNumberProp "value" node (f r * 100.0), held: pure false, reports: \_ -> pure unit, read: pure }

-- | The **toast**: a brief message at the bottom of the screen that
-- | dismisses itself, for something that has just happened and needs no
-- | reply. It never interrupts.
-- |
-- | A status **tells its case**: its caption is the case label verbatim,
-- | so the outcome is named as what it tells (`toast @"Review submitted"
-- | identity`) and the payload is read by nothing but the fold. A `+→×`
-- | leaf like `fold @l f`, it takes its fold of the payload.
toast
  :: forall @l a v r
   . IsSymbol l
  => Cons l a () v
  => (a -> { | r })
  -> PUI Web [ | v ] { | r }
toast = statusOver @l toastFace

toastFace :: forall a r. String -> PUI Web a { | r }
toastFace caseLabel = fromHandles do
  w <- handles $ el "sl-alert" >>> "variant" := "primary" >>> "duration" := "5000" >>> "closable" := ""
    >>> "style" := "position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000; min-width: 300px;" $ fromHandles do
    chrome (el "sl-icon" >>> "slot" := "icon" >>> "name" := "check2-circle" $ staticText "")
    handles (statusText caseLabel)
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        showAlert node
    , fromUser: w.fromUser
    }

-- UIOculars

-- | The **witness rung** — see `PUI.Web.MDC2.confirmed`: `focusCase @l`
-- | over the modal, the `+→+` stage of case `l`, its confirm button
-- | captioned by the case and re-emitting it with the row its content
-- | answered, a cancel consuming it, every other case passing. Over `<sl-dialog>`.
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
-- over `<sl-dialog>`, its title the `label` (its header hidden when there
-- is none, the label naming it still), the content stage, an optional
-- cancel button and the deciding button in the footer; Escape and the
-- overlay close the element by itself, releasing nothing
modal
  :: forall r
   . { title :: Maybe String, decide :: String, cancel :: Maybe String }
  -> PUI Web { | r } { | r }
  -> PUI Web { | r } { | r }
modal { title, decide, cancel } content = fromHandles do
  decideRef <- liftEffect $ Ref.new Nothing
  cancelRef <- liftEffect $ Ref.new Nothing
  shown <- handles $ el "sl-dialog" >>> "label" := fromMaybe decide title >>> maybe ("no-header" := "") (const identity) title $ fromHandles do
    w <- handles (div $ content)
    for_ cancel \c -> do
      chrome (el "sl-button" >>> "slot" := "footer" $ staticText c)
      node <- gets _.sibling
      liftEffect $ Ref.write (Just node) cancelRef
    chrome (el "sl-button" >>> "slot" := "footer" >>> "variant" := "primary" $ staticText decide)
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

-- | A **card**: a surface holding one subject's content. The body stacks
-- | its children with even spacing, so a form or a summary can be dropped
-- | in without spacing each row by hand.
-- |
-- | A plain ocular, with no config of its own: a card is a *surface*, and a
-- | heading of its own is ordinary typography placed in its content —
-- | Shoelace styles plain HTML, so the HTML oculars are that type scale.
card :: Ocular (PUI Web)
card content = el "sl-card" $
  div >>> "style" := "display: flex; flex-direction: column; align-items: flex-start; gap: var(--sl-spacing-medium);" $ content

-- statics (chrome with a face, the wire at any row)

-- | A **divider**: the hairline rule between sections of a surface. Fixed
-- | decoration, carrying no data.
dividerExact :: PUI Web {} {}
dividerExact = staticHTML "<sl-divider style=\"width: 100%;\"></sl-divider>"

divider :: Ocular (PUI Web)
divider = separating dividerExact

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import setTooltipUnit :: Node -> String -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import showAlert :: Node -> Effect Unit
foreign import optionValue :: String -> String

-- Entry point

-- | Mount the app in the page's `<body>`, dressed for Shoelace: the icon
-- | base path — where `sl-rating`'s stars and the alert icons are fetched
-- | from, the CDN release matching the bundled components — is set here,
-- | and the app then mounts exactly as `PUI.Web.HTML.body` does — same
-- | signature, same closed-app demand (`PUI Web {} o`). The entry is a
-- | word of the vocabulary like any other, so a screen changes design
-- | system at its first line by changing the import, and nothing runs at
-- | import time.
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptIconBasePath
  HTML.body ui

foreign import adoptIconBasePath :: Effect Unit

-- | The **indeterminate progress bar**: a status fed a run's `started` and
-- | `ended`, shown between them — what `PUI.action`'s slot dispatches
-- | (`indeterminateLinearProgress # action submit`). No label: it names no
-- | field and no case.
indeterminateLinearProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateLinearProgress = fromHandles do
  element "sl-progress-bar" (pure unit)
  attribute "indeterminate" ""
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ variantToRecordWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "width: 100%; min-width: 200px;"
  hiddenStyle = "width: 100%; min-width: 200px; visibility: hidden;"
