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
  ( body
  , button
  , card
  , divider
  , progressBar
  , rating
  , select
  , selectUnpicked
  , selectOptional
  , sliderLive
  , textArea
  , textField
  , toast
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
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row.VariantToVariant (forCase)
import Data.Variant (case_, match, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (Ocular, PUI)
import PUI.Web.HTML (div, span)
import PUI.Web.HTML (body) as HTML
import PUI.Web (selectedAt, selectedOptionalAt, selectedUnpickedAt, Node, OptCaption(..), Web, addEventListener, attribute, clicked, el, element, getChecked, getValue, isFocused, removeAttribute, setAttribute, setChecked, setValue, staticHTML, staticString, textOf, (:=))
import Type.Proxy (Proxy(..))
import Prim.Row (class Cons)
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
--         variant around it for the `…Unpicked`/`…Optional` siblings);
--       `×→×` displays — `progressBar` (`<sl-progress-bar>`,
--         `{ value :: Number } → {}`, the filled fraction 0–1);
--       `×→+` events — `button @l` (`<sl-button variant="primary">`);
--       `+→×` statuses — `toast` (`<sl-alert>` shown on feed,
--         auto-dismissing via its own `duration`) — each taking
--         its business case and copy function (`toast @"booked" bookedLine`).
--   * **oculars** — shape-preserving decorators: `card { caption }`
--     (`<sl-card>` with a header slot). Typography is deliberately absent:
--     Shoelace styles plain HTML through its tokens, so the `PUI.Web.HTML`
--     element oculars are the typography.
--   * plus **announcing statics** (`{} → {}` chrome with a face):
--     `divider` (`<sl-divider>`).
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (a `settled`
-- normalization on the whole-row stage), not in a leaf bracket.

-- UIs

-- | The **primary button**: the screen's action. It reports on click,
-- | carrying the data it was showing, under the name the app gives the
-- | action — `button @"Submit the review" {}`. The label defaults to
-- | the case label verbatim (`label:` overrides with real copy).
button :: forall @l provided r cl. IsSymbol l => Cons l { | r } () cl => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } [ | cl ]
button provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { label :: String } in eventLeaf @l $
  el "sl-button" >>> "variant" := "primary" $ staticString config.label

-- the click-emitter protocol over any `{} → {}` element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld)
eventLeaf :: forall @l r s. IsSymbol l => Cons l { | r } () s => PUI Web {} {} -> PUI Web { | r } [ | s ]
eventLeaf chrome = clicked @l identity chrome

-- | The **text field**: a labelled single-line input. Shows the string it
-- | is given and reports each edit; typing is never interrupted by values
-- | arriving from elsewhere. Attach it to a field of the model with
-- | `# asField @l`.
textField :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
textField provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  -- focus-guarded like `Web.input`: model updates never clobber the field
  -- being typed in (the shadow input keeps the host as `activeElement`),
  -- but still echo so the channel stays live
  element "sl-input" (pure unit)
  attribute "label" config.label
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
        void $ addEventListener "sl-input" node $ const do
          value <- getValue node
          prop value
    }

-- | The **multi-line text field**, `rows` lines tall — a note, a review, a
-- | message. Otherwise `textField`.
textArea :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String, rows :: Int } => { | provided } -> PUI Web { | r } { | r }
textArea provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "sl-textarea" (pure unit)
  attribute "label" config.label
  attribute "rows" (show config.rows)
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
        void $ addEventListener "sl-input" node $ const do
          value <- getValue node
          prop value
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
rating :: forall @l r rest provided. IsSymbol l => Cons l { current :: Number, max :: Int } rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
rating provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $
  div >>> "style" := "display: inline-flex; flex-direction: column; gap: var(--sl-spacing-3x-small);" $ wrap do
    _ <- unwrap (span >>> "style" := "font-size: var(--sl-input-label-font-size-medium); color: var(--sl-input-label-color);" $ staticString config.label)
    element "sl-rating" (pure unit)
    attribute "label" config.label
    node <- gets _.sibling
    mPropRef <- liftEffect $ Ref.new Nothing
    qRef <- liftEffect $ Ref.new Nothing
    liftEffect $ listenNode node "sl-change" do
      v <- getNumberProp "value" node
      mq <- Ref.read qRef
      for_ mq \q -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop (q { current = v })
    pure
      { toUser: \q -> do
          Ref.write (Just q) qRef
          setAttribute node "max" (show q.max)
          setNumberProp "value" node q.current
          -- leaf echo: announce what was received, so the lifted stage releases
          -- the row and any enclosing merge gate opens
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop q
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
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
sliderLive :: forall @l r rest provided. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
sliderLive provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "sl-range" (pure unit)
  attribute "label" config.label
  attribute "style" "width: 100%; min-width: 240px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  qRef <- liftEffect $ Ref.new Nothing
  -- the value setter could fire input too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "sl-input" do
    busy <- Ref.read busyRef
    unless busy do
      v <- getNumberProp "value" node
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
        Variant.match { discrete: \s -> setAttribute node "step" (show s), continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "value" node q.current
        Ref.write false busyRef
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop q
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The **switch**: a setting that takes effect the moment it is flipped.
-- | The label sits beside it and is part of the target, so clicking the
-- | words toggles it too.
toggleSwitch :: forall @l r rest provided. IsSymbol l => Cons l Boolean rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
toggleSwitch provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "sl-switch" (void $ unwrap (staticString config.label))
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ listenNode node "sl-change" do
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
    }

-- | The **dropdown**: one choice out of a list too long to lay out in the
-- | open.
-- | The field holds the option itself, so the model always has one.
-- | Two siblings hold a variant instead: `selectUnpicked @l @c` for a
-- | choice owed but not yet made, `selectOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- | The options belong to the control, not to the model.
select :: forall @l a rest r provided. IsSymbol l => Cons l a rest r => Eq a => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | r } { | r }
select provided options = selectWith @l false (selectedAt @l) provided options

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
selectUnpicked :: forall @l @c a b s rest r provided. IsSymbol l => IsSymbol c => Cons c a b s => Cons l [ | s ] rest r => Eq a => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectUnpicked provided options = selectWith @l false (selectedUnpickedAt @l @c) provided options

-- | `select` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | its clear button clears it, storing `n` again.
selectOptional :: forall @l @c @n a b t s rest r provided. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b s => Cons n {} t s => Cons l [ | s ] rest r => Eq a => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectOptional provided options = selectWith @l true (selectedOptionalAt @l @c @n) provided options

selectWith :: forall @l a i o provided. IsSymbol l => Eq a => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> { | provided } -> Array { value :: a, label :: String } -> PUI Web i o
selectWith clearable lift provided options = lift $ "name" := reflectSymbol (Proxy @l) $ wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic selection could fire change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "sl-change" do
    busy <- Ref.read busyRef
    unless busy do
      picked <- getValue node
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop (_.value <$> (fromString picked >>= (options !! _)))
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setValue node (show idx)
          Nothing -> setValue node ""
        Ref.write false busyRef
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided
  markup =
    "<sl-select label=\"" <> config.label <> "\"" <> (if clearable then " clearable" else "") <> " style=\"min-width: 240px;\">"
      <> foldMapWithIndex optionMarkup options
      <> "</sl-select>"
  optionMarkup idx o = "<sl-option value=\"" <> show idx <> "\">" <> o.label <> "</sl-option>"

-- | The **progress bar**: how far along something is, `value` running 0 to
-- | 1. As much a gauge as a progress indicator — a quota, a share, a
-- | rating out of five — `progressBar @"Progress" progressFraction`: the
-- | value is a **read function** like every display's, since a fraction is
-- | derived (a ratio of source fields), not state. The label is the
-- | accessible name only — a bar showing 42% must announce *what* is 42% —
-- | so it is copy, never a field reference.
progressBar
  :: forall @l reads
   . IsSymbol l
  => ({ | reads } -> Number) -> PUI Web { | reads } {}
progressBar f = wrap do
  element "sl-progress-bar" (pure unit)
  attribute "aria-label" (reflectSymbol (Proxy @l))
  attribute "style" "width: 100%; min-width: 200px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        -- sl-progress-bar runs 0–100
        setNumberProp "value" node (f r * 100.0)
        -- display echo (like `text`): the feed's answer, inert to gates
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The **toast**: a brief message at the bottom of the screen that
-- | dismisses itself, for something that has just happened and needs no
-- | reply. It never interrupts.
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the toast is built — `toast @"submitted" thanksLine` — and
-- | let the event carry the bare facts.
toast
  :: forall @l a s
   . IsSymbol l
  => Cons l a () s
  => (a -> String)
  -> PUI Web [ | s ] {}
toast copy = toastFace # forCase @l copy

toastFace :: PUI Web [ event :: String ] {}
toastFace = wrap do
  w <- unwrap $ el "sl-alert" >>> "variant" := "primary" >>> "duration" := "5000" >>> "closable" := ""
    >>> "style" := "position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000; min-width: 300px;" $ wrap do
    _ <- unwrap (el "sl-icon" >>> "slot" := "icon" >>> "name" := "check2-circle" $ staticString "")
    unwrap (textOf eventText)
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        showAlert node
    , fromUser: w.fromUser
    }

-- UIOculars

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

-- announcing statics ({} → {} chrome with a face)

-- | A **divider**: the hairline rule between sections of a surface. Fixed
-- | decoration, carrying no data.
divider :: PUI Web {} {}
divider = staticHTML "<sl-divider style=\"width: 100%;\"></sl-divider>"

-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = Variant.on (Proxy @"event") identity Variant.case_

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import listenNode :: Node -> String -> Effect Unit -> Effect Unit
foreign import showAlert :: Node -> Effect Unit

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
