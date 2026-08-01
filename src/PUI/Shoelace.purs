-- Shoelace (https://shoelace.style — the design system continued as Web
-- Awesome) components implemented as PUI Web/Ocular (PUI Web) datatypes —
-- a design-system vocabulary beside `PUI.MDC2`/`PUI.MDC3`, proving the
-- vocabularies interchangeable: built on the framework-agnostic
-- `@shoelace-style/shoelace` custom elements (`<sl-button>`, `<sl-rating>`,
-- ...), registered by importing the FFI module, so a component leaf is just
-- `element "sl-..."` plus property/event wiring — exactly the `PUI.MDC3`
-- recipe, and the leaf-echo protocols are the same (focus-guarded text
-- fields, per-feed display echo, `Just`-only echo on the type-changing
-- selector). Two-sorted, same citizenship, and — where the concept exists
-- in both catalogs — the same names and signatures (`textField` carries
-- Shoelace's plain `label` instead of MD's `floatingLabel`; the catalog has
-- no fill/outline split), so a demo switches design systems by switching
-- the import:
--
--   * **components** — widgets with a model interface, every one a citizen
--     of exactly one row direction:
--       `×→×` editors — `textField @l`, `textArea @l`, `rating @l` (the
--         star editor, `{ value :: Number }` — Shoelace's distinctive
--         catalog entry), `toggleSwitch @l` (`<sl-switch>`), and the
--         type-changing `select @l` (`{ value :: Maybe a } → { value :: a }`);
--       `×→+` events — `button @l` (`<sl-button variant="primary">`);
--       `+→×` statuses — `toast @l` (`<sl-alert>` shown on feed,
--         auto-dismissing via its own `duration`) — canonical
--         `[ event :: String ]` in, adopted via `# forCase @l`.
--   * **oculars** — shape-preserving decorators: `card { caption }`
--     (`<sl-card>` with a header slot). Typography is deliberately absent:
--     Shoelace styles plain HTML through its tokens, so the `PUI.HTML`
--     element oculars are the typography.
--   * plus **announcing statics** (`{} → {}` chrome with a face):
--     `divider` (`<sl-divider>`).
--
-- Page requirements: the Shoelace light theme stylesheet
-- (`themes/light.css` from the same release as the bundled components);
-- default-library icons (the rating's stars, the toast's icon) load from
-- the matching CDN base path set by the FFI module. Fonts are the system
-- stack — Shoelace ships no webfont requirement.
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (`rmap` a total
-- `Model -> Model` after `completed`), not in a leaf bracket.
module PUI.Shoelace
  ( button
  , card
  , divider
  , rating
  , select
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
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (PUI, constantly)
import PUI.HTML (clicked, div, el, span, staticHTML, staticText, text, (:=))
import PUI.Web (Node, Web, addEventListener, attribute, element, getChecked, getValue, isFocused, setAttribute, setChecked, setValue)
import Type.Proxy (Proxy(..))

-- UIs

-- | The `×→+` event button (`<sl-button variant="primary">`): reads the
-- | whole record it is shown and fires it as event case `l` on click.
button :: forall r. { label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
button config = recordToCase @"clicked" $ eventLeaf $
  el "sl-button" >>> "variant" := "primary" $ staticText config.label

-- the click-emitter protocol over any `{} → {}` element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld)
eventLeaf :: forall a. PUI Web {} {} -> PUI Web a a
eventLeaf chrome = clicked (chrome # constantly {})

-- | The Shoelace text input, a `{ value :: String }` editor. Focus-guarded
-- | like `Web.input`: model updates never clobber the field being typed in
-- | (the shadow input keeps the host as `activeElement`), but still echo so
-- | merge gates keep flowing.
textField :: { label :: String } -> PUI Web { value :: String } { value :: String }
textField config = field @"value" $ wrap do
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

-- | `textField`'s multi-line sibling (`<sl-textarea>`).
textArea :: { label :: String, rows :: Int } -> PUI Web { value :: String } { value :: String }
textArea config = field @"value" $ wrap do
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

-- | The Shoelace star rating, the `×→×` editor of a **bounded quantity**
-- | `{ current, max }` — the catalog entry Material has no counterpart
-- | for. The scale is the business half of the datum: it rides the
-- | canonical row, never a UI literal (guardrail A8's channel-fed
-- | resolution), so it arrives from the seed — pointedness makes a missing
-- | scale a compile error at `body` — and may change at runtime. Emits the
-- | whole quantity with `current` replaced (an editor cannot invent its
-- | own scale). `<sl-rating>` carries only an accessible label of its own,
-- | so a non-empty config label renders visibly above it, like a text
-- | field's label.
rating :: { label :: String } -> PUI Web { value :: { current :: Number, max :: Int } } { value :: { current :: Number, max :: Int } }
rating config = field @"value" $
  div >>> "style" := "display: inline-flex; flex-direction: column; gap: var(--sl-spacing-3x-small);" $ wrap do
    _ <- unwrap (span >>> "style" := "font-size: var(--sl-input-label-font-size-medium); color: var(--sl-input-label-color);" $ staticText config.label)
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
          -- leaf echo: announce what was received, so record-merge gates open
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop q
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }

-- | The Shoelace switch, a `×→×` `Boolean` editor; the label is the
-- | element's own slot content, so clicking the text toggles it.
toggleSwitch :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
toggleSwitch config = field @"value" $ wrap do
  element "sl-switch" (void $ unwrap (staticText config.label))
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ listenNode node "sl-change" do
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
    }

-- | The Shoelace select, a `×→×` editor. Type-changing like `PUI.MDC2`'s:
-- | the input field holds the selection state (`Maybe a`), the output field
-- | the bare selection (`a`). Options are design-system config.
select :: forall a. Eq a => { label :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
select config options = field @"value" $ wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic selection could fire change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "sl-change" do
    busy <- Ref.read busyRef
    unless busy do
      picked <- getValue node
      for_ (fromString picked >>= (options !! _)) \o -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop o.value
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setValue node (show idx)
          Nothing -> setValue node ""
        Ref.write false busyRef
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup =
    "<sl-select label=\"" <> config.label <> "\" style=\"min-width: 240px;\">"
      <> foldMapWithIndex optionMarkup options
      <> "</sl-select>"
  optionMarkup idx o = "<sl-option value=\"" <> show idx <> "\">" <> o.label <> "</sl-option>"

-- | The `+→×` status receiver: shows message case `l` in a toast — an
-- | `<sl-alert>` fixed at the bottom, (re)opened on every feed and closed
-- | again by its own `duration` (re-feeding restarts the timer). Contributes
-- | no fields (`text` echoes its `{}`, so it announces).
toast :: PUI Web [ event :: String ] {}
toast = wrap do
  w <- unwrap $ el "sl-alert" >>> "variant" := "primary" >>> "duration" := "5000" >>> "closable" := ""
    >>> "style" := "position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); z-index: 1000; min-width: 300px;" $ wrap do
    _ <- unwrap (el "sl-icon" >>> "slot" := "icon" >>> "name" := "check2-circle" $ staticText "")
    unwrap (lcmap (\v -> { value: Variant.on (Proxy @"event") identity Variant.case_ v }) text)
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        showAlert node
    , fromUser: w.fromUser
    }

-- UIOculars

-- | A card with a caption in the header slot (`<sl-card>`); the body is a
-- | flex column supplying the vertical rhythm between its children.
card :: { caption :: String } -> Ocular (PUI Web)
card config content = el "sl-card" $ wrap do
  _ <- unwrap (div >>> "slot" := "header" >>> "style" := "font-weight: var(--sl-font-weight-semibold);" $ staticText config.caption)
  unwrap (div >>> "style" := "display: flex; flex-direction: column; align-items: flex-start; gap: var(--sl-spacing-medium);" $ content)

-- announcing statics ({} → {} chrome with a face)

divider :: PUI Web {} {}
divider = staticHTML "<sl-divider style=\"width: 100%;\"></sl-divider>"

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import listenNode :: Node -> String -> Effect Unit -> Effect Unit
foreign import showAlert :: Node -> Effect Unit
