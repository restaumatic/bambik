-- Material Design 2 (https://m2.material.io) components implemented as
-- PUI Web/UIOcular (PUI Web) datatypes, dogfooding intentional.
-- The vocabulary is two-sorted:
--
--   * **components** — widgets with a model interface, every one a citizen
--     of exactly one row direction:
--       `×→×` editors — `filledTextField @l`, `filledTextArea @l`,
--         `checkbox @l`, `radioButton @l`, `toggleSwitch @l` (the MD2
--         Switch), `slider @l`, `select @l` (the MD2 exposed dropdown),
--         `segmentedButton @l`, `tabBar @l` (the same-type selector — the
--         `looped`-ensemble citizen), `filterChip @l`, `iconToggle @l`;
--       `×→×` displays — `indeterminateLinearProgress`,
--         `indeterminateCircularProgress` (both `{ busy } → {}`, the shape
--         `PUI.action`'s progress slot expects);
--       `×→+` events — `button @l`, `fab @l`, `iconButton @l`,
--         `menuItem @l`;
--       `+→×` statuses — `snackbar @l`, `banner @l`.
--     No scalar or polymorphic component interfaces. Variant *editing* has
--     no `+→+` component citizens: it goes through record-shaped editor
--     state (`dimap`-bracketed `looped` merges of a selection component
--     plus `shownWhen` panes — see the demos); `+→+` remains the dispatch
--     direction (`VariantToVariant.do` of action stages).
--   * **oculars** — shape-preserving decorators (`card`, `dialog`, `menu`,
--     `chipSet`, `list`/`listItem`, `dataTable`/`dataRow`/
--     `dataCell`, `imageList`, `layoutGrid`/`layoutCell`, `topAppBar`,
--     `drawer`, `tooltip`, typography, elevations): they have no model of
--     their own, so they wrap any polarity and impose none.
--   * plus **announcing statics** (`{} → {}` chrome with a face, like
--     `Web.staticText`): `divider`, `imageListItem`.
--
-- MD2 catalog entries with no MDC Web implementation (backdrop, bottom app
-- bar, bottom navigation, date pickers, navigation rail, sheets) are
-- absent here too.
--
-- Internally the live leaf of a compound is `field @l`-lifted (the
-- closed-singleton form: `dimap`-only, and runtime-exact as the record
-- merges require) and its chrome is hand-fused in the `Web` monad
-- (decoration as implementation technique — and a necessity: abstract
-- labels cannot flow through the merges' `Nub`, so a skolem-labeled
-- operand can't be merged); all-chrome groups (button content, progress
-- bars) have concrete rows and stay literal `RecordToRecord.do` merges of
-- announcing chrome (`staticText`/`staticHTML`/`pempty` at `{} → {}`).
-- Code order = DOM order throughout.
module PUI.MDC
  ( banner
  , body1
  , body2
  , button
  , caption
  , card
  , checkbox
  , chipSet
  , dataCell
  , dataRow
  , dataTable
  , dialog
  , divider
  , drawer
  , elevation1
  , elevation10
  , elevation20
  , fab
  , filledTextArea
  , filledTextField
  , debouncedTextField
  , filterChip
  , headline1
  , headline2
  , headline3
  , headline4
  , headline5
  , headline6
  , iconButton
  , iconToggle
  , imageList
  , imageListItem
  , indeterminateCircularProgress
  , indeterminateLinearProgress
  , layoutCell
  , layoutGrid
  , list
  , listItem
  , menu
  , menuItem
  , overline
  , radioButton
  , segmentedButton
  , select
  , simpleDialog
  , slider
  , snackbar
  , subtitle1
  , subtitle2
  , tabBar
  , toggleSwitch
  , tooltip
  , topAppBar
  )
  where

import Prelude hiding (div)

import Control.Monad.State (gets)
import Data.Array (findIndex, (!!))
import Data.Default (class Default)
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (field, pempty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Time.Duration (Milliseconds)
import Data.Symbol (class IsSymbol)
import Data.Traversable (for)
import Data.Variant (case_, on) as Variant
import Type.Proxy (Proxy(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import PUI (PUI, effAdapter)
import PUI.HTML (aside, checkboxInput, cl, clDyn, div, h1, h2, h3, h4, h5, h6, i, init, input, inputDebounced, label, li, p, span, staticHTML, staticText, table, tbody, td, text, textArea, th, thead, tr, ul, (:=))
import PUI.HTML (button) as HTML
import PUI.Web (Node, Web, uniqueId)

-- UIs

-- | The `×→+` event button: reads the whole record it is shown and fires
-- | it as event case `l` on click (`recordToCase` over the raw button).
button :: forall r. { label :: Maybe String, icon :: Maybe String } -> PUI Web { | r } [ event :: { | r } ]
button config = recordToCase @"event" (containedButton config)

-- | The MD2 tab bar, a `×→×` editor like `segmentedButton @l` but
-- | **same-type** (`Cons l a () s`): the selection is always known from the
-- | input, so it echoes unconditionally and sits happily inside `looped`
-- | ensembles (selection field + `shownWhen` panes). One tab per option;
-- | `MDCTab` drives the activation indicator.
tabBar :: forall a. Eq a => Array { value :: a, label :: String, icon :: Maybe String } -> PUI Web { value :: a } { value :: a }
tabBar options = field @"value" (tabBarLeaf options)

tabBarLeaf :: forall a. Eq a => Array { value :: a, label :: String, icon :: Maybe String } -> PUI Web a a
tabBarLeaf options =
  div >>> cl "mdc-tab-bar" >>> "role" := "tablist" $
    div >>> cl "mdc-tab-scroller" $
      div >>> cl "mdc-tab-scroller__scroll-area" $
        div >>> cl "mdc-tab-scroller__scroll-content" $ wrap do
          tabs <- for options \o -> do
            _ <- unwrap (staticHTML (tabMarkup o.label o.icon))
            node <- gets _.sibling
            comp <- liftEffect $ newComponent material.tab."MDCTab" node
            pure { node, comp, value: o.value }
          mPropRef <- liftEffect $ Ref.new Nothing
          let render sel = for_ tabs \t -> setTabActive t.comp (t.value == sel)
          liftEffect $ for_ tabs \t -> listenNode t.node "click" do
            render t.value
            mProp <- Ref.read mPropRef
            for_ mProp \prop -> void $ prop t.value
          pure
            { toUser: \a -> do
                render a
                -- leaf echo: the selection is always known, so always announce
                mProp <- Ref.read mPropRef
                for_ mProp \prop -> void $ prop a
            , fromUser: \prop -> Ref.write (Just prop) mPropRef
            }
  where
  tabMarkup lbl mIcon =
    "<button class=\"mdc-tab\" role=\"tab\">"
      <> "<span class=\"mdc-tab__content\">"
      <> (case mIcon of
            Just icon' -> "<span class=\"mdc-tab__icon material-icons\" aria-hidden=\"true\">" <> icon' <> "</span>"
            Nothing -> "")
      <> "<span class=\"mdc-tab__text-label\">" <> lbl <> "</span>"
      <> "</span>"
      <> "<span class=\"mdc-tab-indicator\"><span class=\"mdc-tab-indicator__content mdc-tab-indicator__content--underline\"></span></span>"
      <> "<span class=\"mdc-tab__ripple\"></span>"
      <> "</button>"

-- the raw MDC button — scalar, so private: components expose it only in a
-- shaped role (`button @l`)
containedButton :: forall a. { label :: Maybe String, icon :: Maybe String } -> PUI Web a a
containedButton { label, icon } =
  HTML.button >>> cl "mdc-button" >>> cl "mdc-button--raised" >>> cl "initAside-button" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    div >>> cl "mdc-button__ripple" $ pempty
    case icon of
      Just icon' -> i >>> cl "material-icons" >>> cl "mdc-button__icon" >>> "aria-hidden" := "true" $ staticText icon'
      Nothing -> pempty
    case label of
      Just label' -> span >>> cl "mdc-button__label" $ staticText label'
      Nothing -> pempty

-- | The `×→+` event FAB: like `button @l`, reads the whole record it is
-- | shown and fires it as event case `l` on click. A `label` makes it the
-- | extended FAB.
fab :: forall r. { icon :: String, label :: Maybe String } -> PUI Web { | r } [ event :: { | r } ]
fab config = recordToCase @"event" $
  HTML.button >>> cl "mdc-fab" >>> extended >>> "aria-label" := fromMaybe config.icon config.label >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    div >>> cl "mdc-fab__ripple" $ pempty
    span >>> cl "mdc-fab__icon" >>> cl "material-icons" $ staticText config.icon
    case config.label of
      Just label' -> span >>> cl "mdc-fab__label" $ staticText label'
      Nothing -> pempty
  where
  extended = case config.label of
    Just _ -> cl "mdc-fab--extended"
    Nothing -> identity

-- | The `×→+` event icon button (the MD2 icon button; for the toggling
-- | variant see the `×→×` editor `iconToggle @l`).
iconButton :: forall r. { icon :: String, label :: String } -> PUI Web { | r } [ event :: { | r } ]
iconButton config = recordToCase @"event" $
  HTML.button >>> cl "mdc-icon-button" >>> cl "material-icons" >>> "aria-label" := config.label >>> "data-mdc-ripple-is-unbounded" := "" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    div >>> cl "mdc-icon-button__ripple" $ pempty
    staticText config.icon

-- | The `×→+` event list item for the `menu` ocular: fires the record it
-- | is shown as event case `l` on click (the menu closes itself).
menuItem :: forall r. { label :: String } -> PUI Web { | r } [ event :: { | r } ]
menuItem config = recordToCase @"event" (menuItemLeaf config.label)

-- the raw list-item button — scalar, so private (same wiring as
-- `HTML.button`: replay the last value fed on click, `li` chrome)
menuItemLeaf :: forall a. String -> PUI Web a a
menuItemLeaf lbl = wrap do
  _ <- unwrap (li >>> cl "mdc-deprecated-list-item" >>> "role" := "menuitem" $ RecordToRecord.do
    span >>> cl "mdc-deprecated-list-item__ripple" $ pempty
    span >>> cl "mdc-deprecated-list-item__text" $ staticText lbl)
  node <- gets _.sibling
  mARef <- liftEffect $ Ref.new Nothing
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ listenNode node "click" do
    mProp <- Ref.read mPropRef
    mA <- Ref.read mARef
    for_ mProp \prop -> for_ mA \a' -> void $ prop a'
  pure
    { toUser: \a' -> Ref.write (Just a') mARef
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- TODO support input types: email, text, password, number, search, tel, url
filledTextField :: { floatingLabel :: String } -> PUI Web { value :: String } { value :: String }
filledTextField = textFieldWith @"value" (input "text")

-- | `filledTextField` over the debounced input leaf: keystrokes coalesce
-- | at the DOM boundary (`Web.inputDebounced`), so the field is loop-safe
-- | to debounce — the wire itself stays synchronous.
debouncedTextField :: { floatingLabel :: String, millis :: Milliseconds } -> PUI Web { value :: String } { value :: String }
debouncedTextField { floatingLabel, millis } = textFieldWith @"value" (inputDebounced millis "text") { floatingLabel }

textFieldWith :: forall @l s. IsSymbol l => Cons l String () s => PUI Web String String -> { floatingLabel :: String } -> PUI Web { | s } { | s }
textFieldWith leaf { floatingLabel } =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--label-floating" >>> init (\node -> do
      comp <- newComponent material.textField."MDCTextField" node
      useNativeValidation comp false
      pure comp) mempty (\node validationStatus -> do
        setValid node (isNothing validationStatus)
        setContent node (fromMaybe "" validationStatus)) $ wrap do
    _ <- unwrap (span >>> cl "mdc-text-field__ripple" $ pempty)
    floating <- unwrap (span >>> cl "mdc-floating-label" >>> "id" := id >>> clDyn "mdc-floating-label--float-above" isJust $ staticText floatingLabel)
    w <- unwrap (field @l $ leaf # cl "mdc-text-field__input" # "aria-labelledby" := id # "aria-controls" := helperId # "aria-describedby" := helperId)
    _ <- unwrap (div >>> cl "mdc-text-field-helper-line" $
      div >>> cl "mdc-text-field-helper-text" >>> "id" := helperId >>> "aria-hidden" := "true" >>> init mdcTextFieldHelperText mempty mempty $ pempty)
    _ <- unwrap (span >>> cl "mdc-line-ripple" $ pempty)
    pure
      { toUser: \u -> do
          floating.toUser {}
          w.toUser u
      , fromUser: w.fromUser
      }
  where
    id = unsafePerformEffect uniqueId
    helperId = unsafePerformEffect uniqueId

filledTextArea :: { columns :: Int, rows :: Int } -> PUI Web { value :: String } { value :: String }
filledTextArea { columns, rows } =
  label >>> cl "mdc-text-field" >>> cl "mdc-text-field--filled" >>> cl "mdc-text-field--textarea" >>> cl "mdc-text-field--no-label" $ wrap do
    _ <- unwrap (span >>> cl "mdc-text-field__ripple" $ pempty)
    w <- unwrap (field @"value" $ span >>> cl "mdc-text-field__resizer" $ textArea # cl "mdc-text-field__input" >>> "rows" := show rows >>> "columns" := show columns >>> "aria-label" := "Label")
    _ <- unwrap (span >>> cl "mdc-line-ripple" $ pempty)
    pure w

-- | Label content is chrome (`{} → {}`, announcing).
checkbox :: forall a. Default a => PUI Web {} {} -> PUI Web { value :: Maybe a } { value :: Maybe a }
checkbox labelContent =
  div >>> cl "mdc-form-field" >>> init (newComponent material.formField."MDCFormField") mempty mempty $ wrap do
    w <- unwrap $ div >>> cl "mdc-checkbox" >>> init (newComponent material.checkbox."MDCCheckbox") mempty mempty $ wrap do
      w' <- unwrap (field @"value" $ checkboxInput # cl "mdc-checkbox__native-control" # "id" := id)
      _ <- unwrap (div >>> cl "mdc-checkbox__background" $ RecordToRecord.do
        staticHTML """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div >>> cl "mdc-checkbox__mixedmark" $ pempty)
      _ <- unwrap (div >>> cl "mdc-checkbox__ripple" $ pempty)
      pure w'
    -- a real <label for=…> wrapper, so any `{} → {}` content works (even a
    -- bare text node)
    lbl <- unwrap (label >>> "for" := id $ labelContent)
    pure
      { toUser: \u -> do
          lbl.toUser {}
          w.toUser u
      , fromUser: w.fromUser
      }
    where
      id = unsafePerformEffect uniqueId

-- | The MD2 radio group, a `×→×` editor. Type-changing like `select @l`:
-- | the input field holds the selection state (`Maybe a`), the output
-- | field the bare selection (`a`). One radio per option; the shared
-- | native `name` gives browser-level exclusivity and the CSS keys off
-- | `:checked`, so each option's emission is its statically known value.
radioButton :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
radioButton options = field @"value" (radioLeaf options)

radioLeaf :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web (Maybe a) a
radioLeaf options =
  div >>> "style" := "display: flex; flex-direction: column; align-items: flex-start;" $ wrap do
    groupName <- liftEffect uniqueId
    members <- for options \o -> do
      uid <- liftEffect uniqueId
      _ <- unwrap (staticHTML (optionMarkup groupName uid o.label))
      root <- gets _.sibling
      inputNode <- liftEffect $ querySelectorIn root "input"
      pure { inputNode, value: o.value }
    mPropRef <- liftEffect $ Ref.new Nothing
    let render ma = for_ members \m -> setNodeChecked m.inputNode (Just m.value == ma)
    liftEffect $ for_ members \m -> listenNode m.inputNode "change" do
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> void $ prop m.value
    pure
      { toUser: \ma -> do
          render ma
          -- leaf echo (output is the bare selection, so only a `Just` echoes)
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> for_ ma \a' -> void $ prop a'
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }
  where
  optionMarkup groupName uid lbl =
    "<div class=\"mdc-form-field\">"
      <> "<div class=\"mdc-radio\">"
      <> "<input class=\"mdc-radio__native-control\" type=\"radio\" id=\"" <> uid <> "\" name=\"" <> groupName <> "\">"
      <> "<div class=\"mdc-radio__background\"><div class=\"mdc-radio__outer-circle\"></div><div class=\"mdc-radio__inner-circle\"></div></div>"
      <> "<div class=\"mdc-radio__ripple\"></div>"
      <> "</div>"
      <> "<label for=\"" <> uid <> "\">" <> lbl <> "</label>"
      <> "</div>"

-- | The MD2 Switch, a `×→×` `Boolean` editor (the name `switch` was
-- | already taken by the `+→+` case selector).
toggleSwitch :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
toggleSwitch config = field @"value" (switchLeaf config.label)

switchLeaf :: String -> PUI Web Boolean Boolean
switchLeaf lbl = div >>> "style" := "display: flex; align-items: center; gap: 8px;" $ wrap do
  _ <- unwrap (staticHTML switchMarkup)
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.switchControl."MDCSwitch" node
  _ <- unwrap (staticHTML ("<label>" <> lbl <> "</label>"))
  mPropRef <- liftEffect $ Ref.new Nothing
  -- MDCSwitch toggles itself on click; read the post-toggle state
  liftEffect $ listenNode node "click" do
    selected <- getSelected comp
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> void $ prop selected
  pure
    { toUser: \b -> do
        setSelected comp b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  switchMarkup = """
    <button class="mdc-switch mdc-switch--unselected" type="button" role="switch" aria-checked="false">
      <div class="mdc-switch__track"></div>
      <div class="mdc-switch__handle-track">
        <div class="mdc-switch__handle">
          <div class="mdc-switch__shadow"><div class="mdc-elevation-overlay"></div></div>
          <div class="mdc-switch__ripple"></div>
          <div class="mdc-switch__icons">
            <svg class="mdc-switch__icon mdc-switch__icon--on" viewBox="0 0 24 24"><path d="M19.69,5.23L8.96,15.96l-4.65-4.65L3,12.62l6.31,6.31l12-12L19.69,5.23z" /></svg>
            <svg class="mdc-switch__icon mdc-switch__icon--off" viewBox="0 0 24 24"><path d="M20 13H4v-2h16v2z" /></svg>
          </div>
        </div>
      </div>
    </button>"""

-- | The `×→×` `Number` editor. A `step` makes it the discrete slider.
-- | Mid-drag values emit continuously (like mid-typing text); a consumer
-- | that doesn't want the burst wraps its stage in `debounced`.
slider :: { label :: String, min :: Number, max :: Number, step :: Maybe Number } -> PUI Web { value :: Number } { value :: Number }
slider config = field @"value" (sliderLeaf config)

sliderLeaf :: { label :: String, min :: Number, max :: Number, step :: Maybe Number } -> PUI Web Number Number
sliderLeaf config = wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.slider."MDCSlider" node
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ listen comp "MDCSlider:input" do
    v <- getSliderValue comp
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> void $ prop v
  liftEffect $ listen comp "MDCSlider:change" do
    v <- getSliderValue comp
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> void $ prop v
  pure
    { toUser: \v -> do
        setSliderValue comp v
        -- construction may have happened before styles applied; re-measure
        layout comp
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop v
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  discrete = isJust config.step
  markup =
    "<div class=\"mdc-slider" <> (if discrete then " mdc-slider--discrete" else "") <> "\" style=\"min-width: 200px;\">"
      <> "<input class=\"mdc-slider__input\" type=\"range\" min=\"" <> show config.min <> "\" max=\"" <> show config.max <> "\" value=\"" <> show config.min <> "\""
      <> (case config.step of
            Just s -> " step=\"" <> show s <> "\""
            Nothing -> "")
      <> " aria-label=\"" <> config.label <> "\">"
      <> "<div class=\"mdc-slider__track\">"
      <> "<div class=\"mdc-slider__track--inactive\"></div>"
      <> "<div class=\"mdc-slider__track--active\"><div class=\"mdc-slider__track--active_fill\"></div></div>"
      <> "</div>"
      <> "<div class=\"mdc-slider__thumb\">"
      <> (if discrete then "<div class=\"mdc-slider__value-indicator-container\" aria-hidden=\"true\"><div class=\"mdc-slider__value-indicator\"><span class=\"mdc-slider__value-indicator-text\"></span></div></div>" else "")
      <> "<div class=\"mdc-slider__thumb-knob\"></div>"
      <> "</div>"
      <> "</div>"

-- | The MD2 exposed dropdown menu, a `×→×` editor. Type-changing like
-- | `radioButton @l`: the input field holds the selection state
-- | (`Maybe a`), the output field the bare selection (`a`). Options are
-- | design-system config.
select :: forall a. Eq a => { floatingLabel :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
select config options = field @"value" (selectLeaf config options)

selectLeaf :: forall a. Eq a => { floatingLabel :: String } -> Array { value :: a, label :: String } -> PUI Web (Maybe a) a
selectLeaf config options = wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.select."MDCSelect" node
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic selection fires MDCSelect:change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listen comp "MDCSelect:change" do
    busy <- Ref.read busyRef
    unless busy do
      idx <- getSelectedIndex comp
      for_ (options !! idx) \o -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop o.value
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setSelectedIndex comp idx
          Nothing -> setSelectedIndex comp (-1)
        Ref.write false busyRef
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> void $ prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup =
    "<div class=\"mdc-select mdc-select--filled\" style=\"min-width: 200px;\">"
      <> "<div class=\"mdc-select__anchor\" role=\"button\" aria-haspopup=\"listbox\" aria-expanded=\"false\">"
      <> "<span class=\"mdc-select__ripple\"></span>"
      <> "<span class=\"mdc-floating-label\">" <> config.floatingLabel <> "</span>"
      <> "<span class=\"mdc-select__selected-text-container\"><span class=\"mdc-select__selected-text\"></span></span>"
      <> "<span class=\"mdc-select__dropdown-icon\">"
      <> "<svg class=\"mdc-select__dropdown-icon-graphic\" viewBox=\"7 10 10 5\" focusable=\"false\">"
      <> "<polygon class=\"mdc-select__dropdown-icon-inactive\" stroke=\"none\" fill-rule=\"evenodd\" points=\"7 10 12 15 17 10\"></polygon>"
      <> "<polygon class=\"mdc-select__dropdown-icon-active\" stroke=\"none\" fill-rule=\"evenodd\" points=\"7 15 12 10 17 15\"></polygon>"
      <> "</svg>"
      <> "</span>"
      <> "<span class=\"mdc-line-ripple\"></span>"
      <> "</div>"
      <> "<div class=\"mdc-select__menu mdc-menu mdc-menu-surface mdc-menu-surface--fullwidth\">"
      <> "<ul class=\"mdc-deprecated-list\" role=\"listbox\">"
      <> foldMapWithIndex optionMarkup options
      <> "</ul>"
      <> "</div>"
      <> "</div>"
  optionMarkup idx o =
    "<li class=\"mdc-deprecated-list-item\" data-value=\"" <> show idx <> "\" role=\"option\">"
      <> "<span class=\"mdc-deprecated-list-item__ripple\"></span>"
      <> "<span class=\"mdc-deprecated-list-item__text\">" <> o.label <> "</span>"
      <> "</li>"

-- | The MD2 single-select segmented button, a `×→×` editor. Type-changing
-- | like `select @l`; selection styling is CSS-class-driven, so the
-- | wiring is hand-rolled per segment.
segmentedButton :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
segmentedButton options = field @"value" (segmentedLeaf options)

segmentedLeaf :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web (Maybe a) a
segmentedLeaf options =
  div >>> cl "mdc-segmented-button" >>> cl "mdc-segmented-button--single-select" >>> "role" := "radiogroup" $ wrap do
    segments <- for options \o -> do
      _ <- unwrap (staticHTML ("<button class=\"mdc-segmented-button__segment\" role=\"radio\" aria-checked=\"false\"><div class=\"mdc-segmented-button__ripple\"></div><div class=\"mdc-segmented-button__label\">" <> o.label <> "</div></button>"))
      node <- gets _.sibling
      pure { node, value: o.value }
    mPropRef <- liftEffect $ Ref.new Nothing
    let render msel = for_ segments \seg -> setClassIf seg.node "mdc-segmented-button__segment--selected" (Just seg.value == msel)
    liftEffect $ for_ segments \seg -> listenNode seg.node "click" do
      render (Just seg.value)
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> void $ prop seg.value
    pure
      { toUser: \ma -> do
          render ma
          -- leaf echo (output is the bare selection, so only a `Just` echoes)
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> for_ ma \a' -> void $ prop a'
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }

-- | The MD2 filter chip, a `×→×` `Boolean` editor. Selection styling is
-- | CSS-class-driven. Group chips in the `chipSet` ocular.
filterChip :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
filterChip config = field @"value" (chipLeaf config.label)

-- deprecated `mdc-chip` markup on purpose: the prebuilt v14 CSS bundle has
-- no `mdc-evolution-chip` rules at all
chipLeaf :: String -> PUI Web Boolean Boolean
chipLeaf lbl = wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  stateRef <- liftEffect $ Ref.new false
  mPropRef <- liftEffect $ Ref.new Nothing
  let render b = setClassIf node "mdc-chip--selected" b
  liftEffect $ listenNode node "click" do
    b <- not <$> Ref.read stateRef
    Ref.write b stateRef
    render b
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> void $ prop b
  pure
    { toUser: \b -> do
        Ref.write b stateRef
        render b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup =
    "<div class=\"mdc-chip\" role=\"row\">"
      <> "<div class=\"mdc-chip__ripple\"></div>"
      <> "<span class=\"mdc-chip__checkmark\">"
      <> "<svg class=\"mdc-chip__checkmark-svg\" viewBox=\"-2 -3 30 30\">"
      <> "<path class=\"mdc-chip__checkmark-path\" fill=\"none\" stroke=\"black\" d=\"M1.73,12.91 8.1,19.28 22.79,4.59\" />"
      <> "</svg>"
      <> "</span>"
      <> "<span role=\"gridcell\">"
      <> "<span role=\"checkbox\" tabindex=\"0\" aria-checked=\"false\" class=\"mdc-chip__primary-action\">"
      <> "<span class=\"mdc-chip__text\">" <> lbl <> "</span>"
      <> "</span>"
      <> "</span>"
      <> "</div>"

-- | The MD2 icon button (toggle variant), a `×→×` `Boolean` editor —
-- | `onIcon` shows while `true`, `offIcon` while `false`.
iconToggle :: { onIcon :: String, offIcon :: String, label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
iconToggle config = field @"value" (iconToggleLeaf config)

iconToggleLeaf :: { onIcon :: String, offIcon :: String, label :: String } -> PUI Web Boolean Boolean
iconToggleLeaf config = wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.iconButton."MDCIconButtonToggle" node
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ listen comp "MDCIconButtonToggle:change" do
    on' <- getIconToggleOn comp
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> void $ prop on'
  pure
    { toUser: \b -> do
        setIconToggleOn comp b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> void $ prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup =
    "<button class=\"mdc-icon-button\" aria-label=\"" <> config.label <> "\" aria-pressed=\"false\">"
      <> "<div class=\"mdc-icon-button__ripple\"></div>"
      <> "<i class=\"material-icons mdc-icon-button__icon mdc-icon-button__icon--on\">" <> config.onIcon <> "</i>"
      <> "<i class=\"material-icons mdc-icon-button__icon\">" <> config.offIcon <> "</i>"
      <> "</button>"

-- | The `×→×` display citizen for async progress: `{ busy } → {}`, the
-- | shape `PUI.action`'s progress slot expects.
indeterminateLinearProgress :: PUI Web { busy :: Boolean } {}
indeterminateLinearProgress =
  div >>> "role" := "indeterminateLinearProgress" >>> cl "mdc-linear-progress" >>> "aria-label" := "Progress Bar" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" >>> "aria-valuenow" := "0" >>> effAdapter adapter $ RecordToRecord.do
    div >>> cl "mdc-linear-progress__buffer" $ RecordToRecord.do
      div >>> cl "mdc-linear-progress__buffer-bar" $ pempty
      div >>> cl "mdc-linear-progress__buffer-dots" $ pempty
    div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__primary-bar" $
      span >>> cl "mdc-linear-progress__bar-inner" $ pempty
    div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__secondary-bar" $
      span >>> cl "mdc-linear-progress__bar-inner" $ pempty
    where
      adapter = do
        comp <- gets _.sibling >>= (liftEffect <<< newComponent material.linearProgress."MDCLinearProgress")
        liftEffect $ close comp
        liftEffect $ setDeterminate comp false
        pure
          { pre: \r -> (if r.busy then open comp else close comp) $> {}
          , post: pure }

-- | `indeterminateLinearProgress`'s circular sibling — the same
-- | `{ busy } → {}` display citizen.
indeterminateCircularProgress :: PUI Web { busy :: Boolean } {}
indeterminateCircularProgress =
  div >>> cl "mdc-circular-progress" >>> "style" := "width: 48px; height: 48px;" >>> "role" := "progressbar" >>> "aria-label" := "Progress" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" >>> effAdapter adapter $ staticHTML innards
    where
      adapter = do
        comp <- gets _.sibling >>= (liftEffect <<< newComponent material.circularProgress."MDCCircularProgress")
        liftEffect $ close comp
        liftEffect $ setDeterminate comp false
        pure
          { pre: \r -> (if r.busy then open comp else close comp) $> {}
          , post: pure }
      innards = """
        <div class="mdc-circular-progress__determinate-container">
          <svg class="mdc-circular-progress__determinate-circle-graphic" viewBox="0 0 48 48" xmlns="http://www.w3.org/2000/svg">
            <circle class="mdc-circular-progress__determinate-track" cx="24" cy="24" r="18" stroke-width="4"/>
            <circle class="mdc-circular-progress__determinate-circle" cx="24" cy="24" r="18" stroke-dasharray="113.097" stroke-dashoffset="113.097" stroke-width="4"/>
          </svg>
        </div>
        <div class="mdc-circular-progress__indeterminate-container">
          <div class="mdc-circular-progress__spinner-layer">
            <div class="mdc-circular-progress__circle-clipper mdc-circular-progress__circle-left">
              <svg class="mdc-circular-progress__indeterminate-circle-graphic" viewBox="0 0 48 48" xmlns="http://www.w3.org/2000/svg">
                <circle cx="24" cy="24" r="18" stroke-dasharray="113.097" stroke-dashoffset="56.549" stroke-width="4"/>
              </svg>
            </div>
            <div class="mdc-circular-progress__gap-patch">
              <svg class="mdc-circular-progress__indeterminate-circle-graphic" viewBox="0 0 48 48" xmlns="http://www.w3.org/2000/svg">
                <circle cx="24" cy="24" r="18" stroke-dasharray="113.097" stroke-dashoffset="56.549" stroke-width="3.2"/>
              </svg>
            </div>
            <div class="mdc-circular-progress__circle-clipper mdc-circular-progress__circle-right">
              <svg class="mdc-circular-progress__indeterminate-circle-graphic" viewBox="0 0 48 48" xmlns="http://www.w3.org/2000/svg">
                <circle cx="24" cy="24" r="18" stroke-dasharray="113.097" stroke-dashoffset="56.549" stroke-width="4"/>
              </svg>
            </div>
          </div>
        </div>"""

-- UIOculars

headline1 :: Ocular (PUI Web)
headline1 w = h1 w # cl "mdc-typography--headline1"

headline2 :: Ocular (PUI Web)
headline2 w = h2 w # cl "mdc-typography--headline2"

headline3 :: Ocular (PUI Web)
headline3 w = h3 w # cl "mdc-typography--headline3"

headline4 :: Ocular (PUI Web)
headline4 w = h4 w # cl "mdc-typography--headline4"

headline5 :: Ocular (PUI Web)
headline5 w = h5 w # cl "mdc-typography--headline5"

headline6 :: Ocular (PUI Web)
headline6 w = h6 w # cl "mdc-typography--headline6"

subtitle1 :: Ocular (PUI Web)
subtitle1 w = p w # cl "mdc-typography--subtitle1"

subtitle2 :: Ocular (PUI Web)
subtitle2 w = p w # cl "mdc-typography--subtitle2"

caption :: Ocular (PUI Web)
caption w = span w # cl "mdc-typography--caption"

overline :: Ocular (PUI Web)
overline w = span w # cl "mdc-typography--overline"

body1 :: Ocular (PUI Web)
body1 w = p w # cl"mdc-typography--body1"

body2 :: Ocular (PUI Web)
body2 w = p w # cl"mdc-typography--body2"

elevation1 :: Ocular (PUI Web)
elevation1 w = div w # cl "mdc-elevation--z1"

elevation10 :: Ocular (PUI Web)
elevation10 w = div w # cl "mdc-elevation--z10" # "style" := "padding: 25px"

elevation20 :: Ocular (PUI Web)
elevation20 w = div w # cl "mdc-elevation--z20" # "style" := "padding: 25px"

-- | A card with an optional caption — the caption is design-system config
-- | (like `filledTextField`'s `floatingLabel`). The card is content-agnostic
-- | (any polarity), so its caption chrome is hand-fused, not merged.
card :: { caption :: Maybe String } -> Ocular (PUI Web)
card { caption: mCaption } content =
  div >>> cl "mdc-card" >>> "style" := "padding: 10px; margin: 15px 0 15px 0; text-align: justify;" $ wrap do
    for_ mCaption \c -> void $ unwrap (caption $ staticText c)
    unwrap content

dialog :: { title :: String } -> Ocular (PUI Web)
dialog { title } content =
  aside >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") mempty mempty $ wrap do
    result <- unwrap $
      div >>> cl "mdc-dialog__container" $
        div >>> cl "mdc-dialog__surface" >>> "role" := "alertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := "my-dialog-title" >>> "aria-describedby" := "my-dialog-content" $ wrap do
          _ <- unwrap (h2 >>> cl "mdc-dialog__title" >>> "id" := "my-dialog-title" $ staticText title)
          unwrap (div >>> cl "mdc-dialog__content" >>> "id" := "my-dialog-content" $ content)
    _ <- unwrap (div >>> cl "mdc-dialog__scrim" $ pempty)
    pure result

simpleDialog :: { title :: String, confirm :: String } -> Ocular (PUI Web)
simpleDialog { title, confirm } content =
  div >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") open (\a propStatus -> close a) $ wrap do
    result <- unwrap $
      div >>> cl "mdc-dialog__container" $
        div >>> cl "mdc-dialog__surface" >>> "role" := "altertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := "my-dialog-title" >>> "aria-describedby" := "my-dialog-content" $ Semigroupoid.do
          wrap do
            _ <- unwrap (h2 >>> cl "mdc-dialog__title" >>> "id" := id $ staticText title)
            unwrap (div >>> cl "mdc-dialog__content" >>> "id" := id' $ content)
          div >>> cl "mdc-dialog__actions" $
            HTML.button >>> "type" := "button" >>> cl "mdc-button" >>> cl "mdc-dialog__button" $ RecordToRecord.do
              div >>> cl "mdc-button__ripple" $ pempty
              span >>> cl "mdc-button__label" $ staticText confirm
    _ <- unwrap (div >>> cl "mdc-dialog__scrim" $ pempty)
    pure result
    where
      id = unsafePerformEffect uniqueId
      id' = unsafePerformEffect uniqueId

-- | The `+→×` status receiver: shows message case `l` in a snackbar,
-- | contributing no fields (`text` echoes its `{}`, so it announces).
snackbar :: PUI Web [ event :: String ] {}
snackbar = snackbarContainer $ lcmap (\v -> { value: Variant.on (Proxy @"event") identity Variant.case_ v }) text

-- opens on every message and auto-dismisses on the foundation's timeout;
-- closing on emission instead would race the open (the `text` leaf echoes
-- synchronously inside every `toUser`)
snackbarContainer :: Ocular (PUI Web)
snackbarContainer content =
  aside >>> cl "mdc-snackbar" >>> init (newComponent material.snackbar."MDCSnackbar") open mempty $
    div >>> cl "mdc-snackbar__surface" >>> "role" := "status" >>> "aria-relevant" := "additions" $
      div >>> cl "mdc-snackbar__label" >>> "aria-atomic" := "false" $
        content

-- | The `+→×` status receiver in banner clothing: shows message case `l`
-- | in an MDC banner, contributing no fields. Unlike the auto-dismissing
-- | snackbar it stays until its own Dismiss action (foundation-handled).
banner :: PUI Web [ event :: String ] {}
banner = bannerContainer $ lcmap (\v -> { value: Variant.on (Proxy @"event") identity Variant.case_ v }) text

bannerContainer :: Ocular (PUI Web)
bannerContainer content =
  div >>> cl "mdc-banner" >>> "role" := "banner" >>> init (newComponent material.banner."MDCBanner") open mempty $
    div >>> cl "mdc-banner__content" >>> "role" := "alertdialog" >>> "aria-live" := "assertive" $ wrap do
      w <- unwrap (div >>> cl "mdc-banner__graphic-text-wrapper" $ div >>> cl "mdc-banner__text" $ content)
      _ <- unwrap (div >>> cl "mdc-banner__actions" $ staticHTML "<button type=\"button\" class=\"mdc-button mdc-banner__primary-action\"><div class=\"mdc-button__ripple\"></div><div class=\"mdc-button__label\">Dismiss</div></button>")
      pure w

-- | Anchor button plus menu surface around a merge of `menuItem @l`s; the
-- | menu closes itself on item selection.
menu :: { label :: String } -> Ocular (PUI Web)
menu config content = div >>> cl "mdc-menu-surface--anchor" >>> "style" := "display: inline-block;" $ wrap do
  _ <- unwrap (staticHTML ("<button class=\"mdc-button mdc-button--outlined\"><span class=\"mdc-button__ripple\"></span><span class=\"mdc-button__label\">" <> config.label <> "</span><i class=\"material-icons mdc-button__icon\" aria-hidden=\"true\">arrow_drop_down</i></button>"))
  anchorNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.ripple."MDCRipple" anchorNode
  w <- unwrap (div >>> cl "mdc-menu" >>> cl "mdc-menu-surface" $ ul >>> cl "mdc-deprecated-list" >>> "role" := "menu" >>> "aria-hidden" := "true" >>> "aria-orientation" := "vertical" $ content)
  menuNode <- gets _.sibling
  comp <- liftEffect $ newComponent material.menu."MDCMenu" menuNode
  liftEffect $ listenNode anchorNode "click" (setMenuOpen comp true)
  pure w

-- | Chrome for a group of `filterChip @l`s.
chipSet :: Ocular (PUI Web)
chipSet content =
  div >>> cl "mdc-chip-set" >>> cl "mdc-chip-set--filter" >>> "role" := "grid" $ content

list :: Ocular (PUI Web)
list content = ul >>> cl "mdc-deprecated-list" $ content

listItem :: Ocular (PUI Web)
listItem content = li >>> cl "mdc-deprecated-list-item" $ wrap do
  _ <- unwrap (span >>> cl "mdc-deprecated-list-item__ripple" $ pempty)
  unwrap (span >>> cl "mdc-deprecated-list-item__text" $ content)

-- | Table chrome with a static header from config; rows are `dataRow`s of
-- | `dataCell`s.
dataTable :: { label :: String, columns :: Array String } -> Ocular (PUI Web)
dataTable config content =
  div >>> cl "mdc-data-table" $
    div >>> cl "mdc-data-table__table-container" $
      table >>> cl "mdc-data-table__table" >>> "aria-label" := config.label $ wrap do
        _ <- unwrap (thead $ tr >>> cl "mdc-data-table__header-row" $ headerCells)
        unwrap (tbody >>> cl "mdc-data-table__content" $ content)
  where
  headerCells :: PUI Web {} {}
  headerCells = wrap do
    for_ config.columns \c -> void $ unwrap (th >>> cl "mdc-data-table__header-cell" >>> "role" := "columnheader" >>> "scope" := "col" $ staticText c)
    pure
      { toUser: mempty
      , fromUser: \prop -> void $ prop {}
      }

dataRow :: Ocular (PUI Web)
dataRow content = tr >>> cl "mdc-data-table__row" $ content

dataCell :: Ocular (PUI Web)
dataCell content = td >>> cl "mdc-data-table__cell" $ content

-- | Masonry image list; the prebuilt MDC CSS leaves column layout to a
-- | SCSS mixin, so it rides in an inline style here.
imageList :: { columns :: Int } -> Ocular (PUI Web)
imageList config content =
  ul >>> cl "mdc-image-list" >>> cl "mdc-image-list--masonry" >>> "style" := ("column-count: " <> show config.columns <> "; column-gap: 16px; margin: 0;") $ content

layoutGrid :: Ocular (PUI Web)
layoutGrid content = div >>> cl "mdc-layout-grid" $ div >>> cl "mdc-layout-grid__inner" $ content

layoutCell :: { span :: Int } -> Ocular (PUI Web)
layoutCell config content = div >>> cl "mdc-layout-grid__cell" >>> cl ("mdc-layout-grid__cell--span-" <> show config.span) $ content

topAppBar :: { title :: String } -> Ocular (PUI Web)
topAppBar config content = wrap do
  _ <- unwrap (staticHTML ("<header class=\"mdc-top-app-bar\"><div class=\"mdc-top-app-bar__row\"><section class=\"mdc-top-app-bar__section mdc-top-app-bar__section--align-start\"><span class=\"mdc-top-app-bar__title\">" <> config.title <> "</span></section></div></header>"))
  headerNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.topAppBar."MDCTopAppBar" headerNode
  unwrap (div >>> cl "mdc-top-app-bar--fixed-adjust" $ content)

-- | Permanent navigation drawer beside the content; the drawer's own nav
-- | is chrome (`{} → {}`, e.g. a `list` of `listItem`s).
drawer :: { title :: String, subtitle :: String } -> PUI Web {} {} -> Ocular (PUI Web)
drawer config nav content = div >>> "style" := "display: flex;" $ wrap do
  _ <- unwrap (aside >>> cl "mdc-drawer" $ wrap do
    _ <- unwrap (staticHTML ("<div class=\"mdc-drawer__header\"><h3 class=\"mdc-drawer__title\">" <> config.title <> "</h3><h6 class=\"mdc-drawer__subtitle\">" <> config.subtitle <> "</h6></div>"))
    unwrap (div >>> cl "mdc-drawer__content" $ nav))
  unwrap (div >>> cl "mdc-drawer-app-content" >>> "style" := "flex: 1; padding: 16px;" $ content)

-- | Attach a hover/focus tooltip to the wrapped element (single-element
-- | content: the anchor is the content's root node).
tooltip :: { text :: String } -> Ocular (PUI Web)
tooltip config content = wrap do
  w <- unwrap ("aria-describedby" := tipId $ content)
  _ <- unwrap (staticHTML ("<div id=\"" <> tipId <> "\" class=\"mdc-tooltip\" role=\"tooltip\" aria-hidden=\"true\"><div class=\"mdc-tooltip__surface mdc-tooltip__surface-animation\">" <> config.text <> "</div></div>"))
  tipNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.tooltip."MDCTooltip" tipNode
  pure w
  where
  tipId = unsafePerformEffect uniqueId

-- announcing statics (`{} → {}` chrome with a face)

divider :: PUI Web {} {}
divider = staticHTML "<hr class=\"mdc-deprecated-list-divider\" style=\"width: 100%;\">"

imageListItem :: { src :: String, label :: String } -> PUI Web {} {}
imageListItem config = staticHTML $
  "<li class=\"mdc-image-list__item\" style=\"margin-bottom: 16px;\">"
    <> "<img class=\"mdc-image-list__image\" src=\"" <> config.src <> "\" alt=\"" <> config.label <> "\">"
    <> "<div class=\"mdc-image-list__supporting\"><span class=\"mdc-image-list__label\">" <> config.label <> "</span></div>"
    <> "</li>"

-- Private

foreign import data Component :: Type
foreign import data ComponentClass :: Type
foreign import open :: Component -> Effect Unit
foreign import close :: Component -> Effect Unit
foreign import newComponent :: ComponentClass -> Node -> Effect Component
foreign import setDeterminate :: Component -> Boolean -> Effect Unit
foreign import listen :: Component -> String -> Effect Unit -> Effect Unit
foreign import listenNode :: Node -> String -> Effect Unit -> Effect Unit
foreign import setClassIf :: Node -> String -> Boolean -> Effect Unit
foreign import getSliderValue :: Component -> Effect Number
foreign import setSliderValue :: Component -> Number -> Effect Unit
foreign import layout :: Component -> Effect Unit
foreign import getSelected :: Component -> Effect Boolean
foreign import setSelected :: Component -> Boolean -> Effect Unit
foreign import getSelectedIndex :: Component -> Effect Int
foreign import setSelectedIndex :: Component -> Int -> Effect Unit
foreign import getIconToggleOn :: Component -> Effect Boolean
foreign import setIconToggleOn :: Component -> Boolean -> Effect Unit
foreign import setMenuOpen :: Component -> Boolean -> Effect Unit
foreign import setTabActive :: Component -> Boolean -> Effect Unit
foreign import closeBanner :: Component -> Effect Unit
foreign import querySelectorIn :: Node -> String -> Effect Node
foreign import setNodeChecked :: Node -> Boolean -> Effect Unit
foreign import material
  :: { textField :: { "MDCTextField" :: ComponentClass }
    --  , textFieldHelperText :: { "MDCTextFieldHelperText" :: ComponentClass }
     , ripple :: { "MDCRipple" :: ComponentClass }
     , drawer :: { "MDCDrawer" :: ComponentClass }
     , tabBar :: { "MDCTabBar" :: ComponentClass }
     , tab :: { "MDCTab" :: ComponentClass }
     , dialog :: { "MDCDialog" :: ComponentClass }
     , snackbar :: { "MDCSnackbar" :: ComponentClass }
     , banner :: { "MDCBanner" :: ComponentClass }
     , radio :: { "MDCRadio" :: ComponentClass }
     , chips :: { "MDCChip" :: ComponentClass }
     , select :: { "MDCSelect" :: ComponentClass }
     , list :: { "MDCList" :: ComponentClass }
     , menu :: { "MDCMenu" :: ComponentClass }
     , checkbox :: { "MDCCheckbox" :: ComponentClass }
     , formField :: { "MDCFormField" :: ComponentClass }
     , linearProgress :: { "MDCLinearProgress" :: ComponentClass }
     , circularProgress :: { "MDCCircularProgress" :: ComponentClass }
     , slider :: { "MDCSlider" :: ComponentClass }
     , switchControl :: { "MDCSwitch" :: ComponentClass }
     , iconButton :: { "MDCIconButtonToggle" :: ComponentClass }
     , tooltip :: { "MDCTooltip" :: ComponentClass }
     , topAppBar :: { "MDCTopAppBar" :: ComponentClass }
     }

foreign import mdcTextFieldHelperText :: Node -> Effect Component
foreign import setValid :: Component -> Boolean -> Effect Unit
foreign import setContent :: Component -> String -> Effect Unit
foreign import useNativeValidation :: Component -> Boolean -> Effect Unit
