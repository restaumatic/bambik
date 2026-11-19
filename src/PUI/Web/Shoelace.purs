-- | The **Shoelace** vocabulary (https://shoelace.style, continued as Web
-- | Awesome), over the `@shoelace-style/shoelace` custom elements: the
-- | same names and signatures as the Material modules wherever both
-- | catalogues have the concept (guardrails L20), and its own names where
-- | the concept differs (`textField` with Shoelace's plain `label`,
-- | `toast`, `progressBar`). Its exclusive is the star `rating`, which
-- | Material has no counterpart for.
-- |
-- | **The page must load** the Shoelace light theme stylesheet from the
-- | same release as the bundled components; icons load from the matching
-- | CDN, whose base path `body` sets at mount. No webfont: Shoelace uses
-- | the system font stack.
-- |
-- | The catalogue: `textField`/`textArea`, `rating`, `sliderLive` and
-- | `toggleSwitch` to enter values, `select` to choose one, `button` to
-- | act, `toast` to say what happened, `progressBar` to show a figure,
-- | `card` and `divider` for structure, `body` to mount the app.
-- | Typography is deliberately absent: Shoelace styles plain HTML, so the
-- | `PUI.Web.HTML` elements are the type scale.
module PUI.Web.Shoelace
  ( confirmed
  , confirmed'
  , modal
  , body
  , button
  , card
  , group
  , divider
  , indeterminateLinearProgress
  , option
  , progressBar
  , rating
  , select
  , selectUnpicked
  , selectOptional
  , sliderLive
  , sliderLive'
  , textArea
  , textField
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
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import Effect.Ref as Ref
import Data.Foldable (for_)
import PUI (fromHandles, handles, Ocular, PUI, optionWiring, recordToRecordWiring, variantToRecordWiring, variantToVariantWiring, statusOver, confirmedOver, confirmedOver', modalOver, eventLeaf)
import PUI.Web.HTML (div, span)
import PUI.Web.HTML (body) as HTML
import PUI.Web (chrome, fieldLeaf, Node, OptCaption(..), Web, attribute, el, element, getChecked, getValue, isFocused, listenNode, removeAttribute, setAttribute, setChecked, setValue, staticHTML, staticText, statusText, separating, invokeMethod, uniqueId, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)

-- Implementation notes — the reference above is the contract.
--
-- The `PUI.Web.MDC3` recipe over Shoelace's custom elements, registered by
-- importing the FFI module: a leaf is `element "sl-..."` plus
-- property/event wiring, and the leaf protocols are the shared ones
-- (focus-guarded text fields, per-feed display echo, a selection editor
-- answering every feed with its row). The catalogue has no fill/outline
-- split, so there is one `textField`. The `dimap` round-trip contract for
-- editors holds as in `PUI.Web.MDC2`.

-- UIs

-- | The **primary button** (`<sl-button variant="primary">`): on click it
-- | emits the row it was showing as its case, which captions it —
-- | `button @"Submit the review"`.
button :: forall @l r v. IsSymbol l => Cons l { | r } () v => PUI Web { | r } [ | v ]
button = let config = { label: reflectSymbol (Proxy @l) } :: { label :: String } in eventLeaf @l $
  el "sl-button" >>> "variant" := "primary" $ staticText config.label

-- | The **text field** (`<sl-input>`): a labelled single-line input
-- | editing field `l`. Shows the string it is given and reports each
-- | edit; typing is never interrupted by values arriving from elsewhere.
textField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
textField = fieldLeaf @l $ inputLeaf (reflectSymbol (Proxy @l)) ""

-- | `textField` with a `hint` under it — a format or an example the label
-- | is not (`textField' @"Start date" { hint: "DD.MM.YYYY" }`), shown as
-- | Shoelace's help text.
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

-- | The **multi-line text field** (`<sl-textarea>`), `rows` lines tall
-- | — a note, a review, a message. Otherwise `textField`.
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

-- | The **star rating** (`<sl-rating>`) — Shoelace's exclusive, the star
-- | *editor* Material lacks: a judgement given by picking a point on a
-- | scale. The field is `{ current, max }`, one business datum, so how
-- | many stars there are comes from the data and may differ between
-- | contexts. The label is drawn above the stars.
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

-- | The **live slider** (`<sl-range>`): a quantity chosen by feel. The
-- | field is the bounded quantity `{ current, min, max, step }`, one
-- | business datum, so the limits come from the data and may change while
-- | the app runs; `step` is `.discrete n` or `.continuous {}`. It reports
-- | on **every change**, following the drag — hence the name; the
-- | catalogue has no commit-only slider — so what it drives should be
-- | cheap to redo or `debounced`. The current number shows in the
-- | control's own tooltip while dragging.
sliderLive :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
sliderLive = fieldLeaf @l $ rangeLeaf (reflectSymbol (Proxy @l)) ""

-- | `sliderLive` whose value is read in a `unit` (`sliderLive' @"Amount"
-- | { unit: "€" }`), shown after the number in the tooltip.
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

-- | The **switch** (`<sl-switch>`): a Boolean setting that takes effect
-- | the moment it is flipped. The label sits beside it and is part of the
-- | target, so clicking the words toggles it too.
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

-- | The **select** (`<sl-select>`): one choice out of a list too long to
-- | lay out in the open. Its options are `option` leaves, one per case of
-- | the field, merged `VariantToVariant.do` in the order shown: `select
-- | @"Owned for" $ VariantToVariant.do option @"Under a month"; option
-- | @"Over a year"`. The block closes the field's cases and the select
-- | lifts it onto field `l`, which holds the option itself. Two siblings
-- | hold a variant instead: `selectUnpicked @l @c` for a choice owed but
-- | not yet made, `selectOptional @l @c @n` for one the user may leave
-- | unmade. All three are editors: every feed is answered with the row,
-- | every pick stored.
-- A selector group over one clearing of its options: every feed first
-- shows nothing picked, then the one option owning the fed case shows
-- itself, so a case no option owns (an unpicked one) shows nothing.
select :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
select options = fieldLeaf @l $ selectGroupOf @l false options

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

-- | One option of a `select`: an `<sl-option>` captioned by its case. A
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

-- | The **progress bar** (`<sl-progress-bar>`): a fraction 0–1 of how
-- | far along something is — as much a gauge as a progress indicator.
-- | Takes a read function and no label (guardrails L17):
-- | `progressBar progressFraction`.
progressBar
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
progressBar f = fromHandles do
  element "sl-progress-bar" (pure unit)
  attribute "style" "width: 100%; min-width: 200px;"
  node <- gets _.sibling
  -- sl-progress-bar runs 0–100
  liftEffect $ recordToRecordWiring { show: \r -> setNumberProp "value" node (f r * 100.0), held: pure false, reports: \_ -> pure unit, read: pure }

-- | The **toast** (`<sl-alert>`, fixed at the bottom, dismissing itself
-- | through its own `duration`): a brief message that never interrupts.
-- | A status, it tells its case — the case label is its whole copy — and
-- | takes its fold, of the event and the model it lands on (`toast
-- | @"Review submitted" captured`; see `PUI.Web.MDC2.snackbar`).
toast
  :: forall @l a v i o
   . IsSymbol l
  => Cons l a () v
  => ({ event :: a, model :: i } -> o)
  -> PUI Web [ | v ] (i -> o)
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

-- | `confirmed` — see `PUI.Web.MDC2.confirmed`; over `<sl-dialog>`.
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

-- | The **modal** — see `PUI.Web.MDC2.modal`; over `<sl-dialog>`.
modal
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
modal = modalOver @l modalFace

-- the modal the dialog leaves are built over: `variantToVariantWiring`
-- over `<sl-dialog>`, its title the `label` (its header hidden when there
-- is none, the label naming it still), the content stage, an optional
-- cancel button and the deciding button in the footer; Escape and the
-- overlay close the element by itself, releasing nothing
modalFace
  :: forall r
   . { title :: Maybe String, decide :: String, cancel :: Maybe String }
  -> PUI Web { | r } { | r }
  -> PUI Web { | r } { | r }
modalFace { title, decide, cancel } content = fromHandles do
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

-- | A **card** (`<sl-card>`): a surface holding one subject's content,
-- | its body stacking the children with even spacing. A plain ocular with
-- | no config: a heading is typography in its content (the HTML oculars,
-- | Shoelace's type scale).
card :: Ocular (PUI Web)
card content = el "sl-card" $ div >>> "style" := cardBodyStyle $ content

cardBodyStyle :: String
cardBodyStyle = "display: flex; flex-direction: column; align-items: flex-start; gap: var(--sl-spacing-medium);"

-- | The **labelled group** — the contract is `PUI.Web.MDC2.group`'s:
-- | `w` on an `<sl-card>` whose `header` slot holds the label verbatim,
-- | its model nested under field `l`, the label the accessible group
-- | name. Derived: `group @l w = card (heading >>> focusField @l w)` plus
-- | the a11y stamp.
group
  :: forall @l f f' b s t
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b t
  => PUI Web f f'
  -> PUI Web { | s } { | t }
group w = fromHandles do
  headingId <- liftEffect uniqueId
  handles $ "role" := "group" $ "aria-labelledby" := headingId $ el "sl-card" $
    (div >>> "slot" := "header" >>> "id" := headingId $ staticText (reflectSymbol (Proxy @l)))
      >>> (div >>> "style" := cardBodyStyle $ focusField @l w)

-- statics (chrome with a face, the wire at any row)

-- the divider's face, a `{}`-static
dividerExact :: PUI Web {} {}
dividerExact = staticHTML "<sl-divider style=\"width: 100%;\"></sl-divider>"

-- | A **divider** (`<sl-divider>`): the hairline rule between sections,
-- | shown only between things (`separating`). Carries no data.
divider :: Ocular (PUI Web)
divider = separating dividerExact

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import setTooltipUnit :: Node -> String -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import showAlert :: Node -> Effect Unit
foreign import optionValue :: String -> String

-- Entry point

-- | Mount the app in the page's `<body>` as `PUI.Web.HTML.body` does (same
-- | signature, same closed-app demand), after setting the icon base path
-- | — where `sl-rating`'s stars and the alert icons are fetched from, the
-- | CDN release matching the bundled components. Nothing runs at import
-- | time (guardrails L11).
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptIconBasePath
  HTML.body ui

foreign import adoptIconBasePath :: Effect Unit

-- | The **indeterminate progress bar**: the action slot, fed a run's
-- | `started` and `ended` and shown between them
-- | (`indeterminateLinearProgress # action @l f`). No label: it names no
-- | field and no case.
indeterminateLinearProgress :: PUI Web [ started :: {}, ended :: {} ] {}
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
