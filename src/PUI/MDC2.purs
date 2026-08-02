-- Material Design 2 (https://m2.material.io) components implemented as
-- PUI Web/UIOcular (PUI Web) datatypes — the MD2 sibling of `PUI.MDC3`,
-- built on the MDC Web foundation classes from `material-components-web`:
-- a component leaf is the catalog's documented DOM markup plus a foundation
-- instance (`newComponent material.x."MDCX"`) wired through its documented
-- properties and events — the foundation owns ripples, label float,
-- activation and aria, exactly as the MDC Web docs prescribe. The
-- vocabulary is two-sorted, with the same citizenship and (where the
-- concept exists in both design systems) the same names and signatures as
-- `PUI.MDC3`, so a demo switches design systems by switching the import:
--
--   * **components** — widgets with a model interface, every one a citizen
--     of exactly one row direction:
--       `×→×` editors — `filledTextField @l`, `outlinedTextField @l` (the
--         MD2 variant pair), `filledTextArea @l`, `checkbox @l`,
--         `radioButton @l`, `toggleSwitch @l` (the MD2 Switch),
--         `slider @l`, `select @l` (the MD2 exposed dropdown),
--         `segmentedButton @l`, `tabBar @l` (the same-type selector — the
--         `looped`-ensemble citizen), `filterChip @l`, `iconToggle @l`;
--       `×→×` displays — `indeterminateLinearProgress`,
--         `indeterminateCircularProgress` (both `{ busy } → {}`, the shape
--         `PUI.action`'s progress slot expects) and the determinate
--         `linearProgress` (`{ value } → {}`);
--       `×→+` events — `button @l` (the contained/raised button — the
--         high-emphasis default; `outlinedButton`, `textButton` are the
--         other two MD2 emphasis levels), `fab @l`, `iconButton @l`,
--         `menuItem @l`;
--       `+→×` statuses — `snackbar @l`, `banner @l` (MD2 still has the
--         banner; MD3 dropped it, so `PUI.MDC3` has no citizen for it).
--     No scalar or polymorphic component interfaces. Variant *editing* has
--     no `+→+` component citizens: it goes through record-shaped editor
--     state (`dimap`-bracketed `looped` pipelines — a selection component
--     followed by payload-typed panes, each `# provided # lcmap <paneOf>`
--     shown by the presence of its `Maybe` payload — see the demos); `+→+`
--     remains the dispatch direction (`VariantToVariant.do` of action stages).
--   * **oculars** — shape-preserving decorators (`card`, `dialog`, `menu`,
--     `chipSet`, `list`/`listItem`, `dataTable`/`dataRow`/`dataCell`,
--     `imageList`, `layoutGrid`/`layoutCell`, `topAppBar`, `drawer`,
--     `tooltip`, the MD2 typography — `headline1` ... `overline` — and
--     elevations): no model of their own, any polarity.
--   * plus **announcing statics** (`{} → {}` chrome with a face):
--     `divider`, `imageListItem`.
--
-- MD2 catalog entries with no MDC Web implementation (backdrop, bottom app
-- bar, bottom navigation, date pickers, navigation rail, sheets) are
-- absent here too.
--
-- Page requirements: the prebuilt MDC Web stylesheet
-- (https://unpkg.com/material-components-web@latest/dist/material-components-web.min.css),
-- the Material Icons font
-- (https://fonts.googleapis.com/icon?family=Material+Icons) and Roboto —
-- unlike `PUI.MDC3`, whose component styles ship in the bundle, MD2's
-- styles come from the page-linked prebuilt CSS (which is also why the
-- chips and lists here stay on the `mdc-chip`/`mdc-deprecated-list` markup
-- that CSS actually carries).
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
--
-- **The `dimap` round-trip contract for editors.** An editor bracketed by
-- `dimap f g` behaves as an iso lens: `f` maps the model into the editor's
-- canonical value, `g` maps edits back. If `g >>> f` is not the identity
-- on the values the user actually enters, the field visibly *normalizes*
-- input on each echo — the focus guard suppresses this only for the field
-- currently being typed in. Conversions that can fail or lose information
-- (parse-then-format, unit conversion over strings) belong in the model
-- (`rmap` a total `Model -> Model` after `completed`), not in a leaf
-- bracket — see the temperature-converter demo.
module PUI.MDC2
  ( OptLabelIcon(..)
  , OptLabel(..)
  , OptIcon(..)
  , OptSelected(..)
  , banner
  , body1
  , body2
  , button
  , caption
  , card
  , cardActions
  , checkbox
  , chipSet
  , dataCell
  , dataRow
  , dataTable
  , debouncedTextField
  , dialog
  , divider
  , drawer
  , elevation1
  , elevation10
  , elevation20
  , fab
  , filledTextArea
  , filledTextField
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
  , linearProgress
  , list
  , listItem
  , listOf
  , menu
  , menuItem
  , outlinedButton
  , outlinedTextField
  , overline
  , radioButton
  , segmentedButton
  , select
  , simpleDialog
  , slider
  , sliderLive
  , snackbar
  , subtitle1
  , subtitle2
  , tabBar
  , textButton
  , toggleSwitch
  , tooltip
  , topAppBar
  )
  where

import Prelude hiding (div)

import Control.Monad.State (gets)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Array (findIndex, mapWithIndex, (!!))
import Data.Default (class Default, default)
import Data.Foldable (foldMap, for_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Lens.Extra.Types (Ocular)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Row.RecordToRecord (field, pempty, projected)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Traversable (for)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (PUI, constantly, foreach)
import PUI.HTML (aside, cl, clWhen, clicked, div, el, h1, h2, h3, h4, h5, h6, i, init, label, li, p, span, staticHTML, staticText, table, tbody, td, text, th, thead, tr, ul, (:=))
import PUI.Web (Node, Web, addEventListener, attribute, clazz, element, getChecked, getValue, isFocused, onInputDebounced, setAttribute, setChecked, uniqueId)
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))

-- UIs

-- Conversion tags scope which field names lift a bare value to `Just`, as
-- in `PUI.MDC3`: an existing `Maybe` passes through, every other field
-- passes through at its given type. A field name is optional on some
-- components and required on others — `label` is optional on `button`,
-- required on `slider`; `icon` is optional on `button`, required on `fab`
-- — so the *tag*, not a global per-symbol instance, decides which fields
-- are optional for a given widget. One tag per distinct optional-field
-- set: `OptLabelIcon` (buttons), `OptLabel` (fab, caption via card),
-- `OptSelected` (listOf), `OptIcon` (tabBar options).
data OptLabelIcon = OptLabelIcon

instance ConvertOption OptLabelIcon "label" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabelIcon "icon" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabelIcon sym a a where
  convertOption _ _ = identity

data OptLabel = OptLabel

instance ConvertOption OptLabel "label" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabel "caption" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabel sym a a where
  convertOption _ _ = identity

data OptSelected = OptSelected

instance ConvertOption OptSelected sym a a where
  convertOption _ _ = identity

data OptIcon = OptIcon

instance ConvertOption OptIcon "icon" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptIcon sym a a where
  convertOption _ _ = identity

-- | The `×→+` event button (the MD2 contained/raised button — the
-- | high-emphasis default): reads the whole record it is shown and fires
-- | it as event case `l` on click. Both fields are optional and default to
-- | `Nothing`: `button {}` is bare, `button { label: "Count" }` labels it,
-- | `icon: "add"` adds a Material Icons icon.
button
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
button = buttonOf (Just "mdc-button--raised")

-- | `button` at the MD2 outlined (medium) emphasis.
outlinedButton
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
outlinedButton = buttonOf (Just "mdc-button--outlined")

-- | `button` at the MD2 text (lowest) emphasis — the bare `mdc-button`.
textButton
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
textButton = buttonOf Nothing

buttonOf
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => Maybe String
  -> { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
buttonOf mModifier provided = recordToCase @"clicked" $ eventLeaf $
  el "button" >>> cl "mdc-button" >>> modifier >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    span >>> cl "mdc-button__ripple" $ pempty
    span >>> cl "mdc-button__focus-ring" $ pempty
    case config.icon of
      Just icon' -> i >>> cl "material-icons" >>> cl "mdc-button__icon" >>> "aria-hidden" := "true" $ staticText icon'
      Nothing -> pempty
    case config.label of
      Just label' -> span >>> cl "mdc-button__label" $ staticText label'
      Nothing -> pempty
  where
  config = convertOptionsWithDefaults OptLabelIcon { label: Nothing, icon: Nothing } provided :: { label :: Maybe String, icon :: Maybe String }
  modifier = case mModifier of
    Just m -> cl m
    Nothing -> identity

-- the click-emitter protocol over any `{} → {}` element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld) —
-- `clicked` over the input-freed chrome, the last-built element listening
eventLeaf :: forall a. PUI Web {} {} -> PUI Web a a
eventLeaf chrome = clicked (chrome # constantly {})

-- | The `×→+` event FAB: like `button @l`, reads the whole record it is
-- | shown and fires it as event case `l` on click. `icon` is required; a
-- | `label` (bare string) makes it the extended FAB.
fab
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabel { label :: Maybe String } { | provided } { icon :: String, label :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
fab provided = recordToCase @"clicked" $ eventLeaf $
  el "button" >>> cl "mdc-fab" >>> extended >>> "aria-label" := fromMaybe config.icon config.label >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    div >>> cl "mdc-fab__ripple" $ pempty
    span >>> cl "mdc-fab__focus-ring" $ pempty
    span >>> cl "mdc-fab__icon" >>> cl "material-icons" $ staticText config.icon
    case config.label of
      Just label' -> span >>> cl "mdc-fab__label" $ staticText label'
      Nothing -> pempty
  where
  config = convertOptionsWithDefaults OptLabel { label: Nothing } provided :: { icon :: String, label :: Maybe String }
  extended = case config.label of
    Just _ -> cl "mdc-fab--extended"
    Nothing -> identity

-- | The `×→+` event icon button (the MD2 icon button; for the toggling
-- | variant see the `×→×` editor `iconToggle @l`).
iconButton :: forall r. { icon :: String, label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
iconButton config = recordToCase @"clicked" $ eventLeaf $
  el "button" >>> cl "mdc-icon-button" >>> cl "material-icons" >>> "aria-label" := config.label >>> "data-mdc-ripple-is-unbounded" := "" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    div >>> cl "mdc-icon-button__ripple" $ pempty
    span >>> cl "mdc-icon-button__focus-ring" $ pempty
    staticText config.icon

-- | The `×→+` event list item for the `menu` ocular: fires the record it
-- | is shown as event case `l` on click (the menu closes itself).
menuItem :: forall r. { label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
menuItem config = recordToCase @"clicked" $ eventLeaf $
  li >>> cl "mdc-deprecated-list-item" >>> "role" := "menuitem" >>> "tabindex" := "-1" $ RecordToRecord.do
    span >>> cl "mdc-deprecated-list-item__ripple" $ pempty
    span >>> cl "mdc-deprecated-list-item__text" $ staticText config.label

-- TODO support input types: email, text, password, number, search, tel, url
filledTextField :: { floatingLabel :: String } -> PUI Web { value :: String } { value :: String }
filledTextField config = field @"value" (textFieldLeaf "filled" Nothing config.floatingLabel)

-- | `filledTextField` in the MD2 outlined variant.
outlinedTextField :: { floatingLabel :: String } -> PUI Web { value :: String } { value :: String }
outlinedTextField config = field @"value" (textFieldLeaf "outlined" Nothing config.floatingLabel)

-- | `filledTextField` over a debounced input listener: keystrokes coalesce
-- | at the DOM boundary (`Web.onInputDebounced`), so the field is loop-safe
-- | to debounce — the wire itself stays synchronous.
debouncedTextField :: { floatingLabel :: String, ms :: Number } -> PUI Web { value :: String } { value :: String }
debouncedTextField { floatingLabel, ms } = field @"value" (textFieldLeaf "filled" (Just ms) floatingLabel)

-- the raw MD2 text field — scalar, so private; the documented markup per
-- variant plus an `MDCTextField` foundation, values written through the
-- foundation's `value` property so the label float and line ripple stay
-- foundation-managed. Focus-guarded like `Web.input`: model updates never
-- clobber the field being typed in, but still echo so merge gates keep
-- flowing.
textFieldLeaf :: String -> Maybe Number -> String -> PUI Web String String
textFieldLeaf variant mDebounce floatingLabel = wrap do
  labelId <- liftEffect uniqueId
  inputNode <- element "label" do
    if variant == "outlined"
      then void $ unwrap $ span >>> cl "mdc-notched-outline" $ RecordToRecord.do
        span >>> cl "mdc-notched-outline__leading" $ pempty
        span >>> cl "mdc-notched-outline__notch" $
          span >>> cl "mdc-floating-label" >>> "id" := labelId $ staticText floatingLabel
        span >>> cl "mdc-notched-outline__trailing" $ pempty
      else do
        _ <- unwrap (span >>> cl "mdc-text-field__ripple" $ pempty)
        void $ unwrap (span >>> cl "mdc-floating-label" >>> "id" := labelId $ staticText floatingLabel)
    element "input" (pure unit)
    clazz "mdc-text-field__input"
    attribute "type" "text"
    attribute "aria-labelledby" labelId
    node <- gets _.sibling
    when (variant == "filled") $
      void $ unwrap (span >>> cl "mdc-line-ripple" $ pempty)
    pure node
  clazz "mdc-text-field"
  clazz ("mdc-text-field--" <> variant)
  fieldNode <- gets _.sibling
  comp <- liftEffect $ newComponent material.textField."MDCTextField" fieldNode
  liftEffect $ textFieldWiring comp inputNode mDebounce

-- the shared text-field/text-area wiring: write through the foundation's
-- `value` (focus-guarded, echoing), read the native input's events
textFieldWiring :: Component -> Node -> Maybe Number -> Effect { toUser :: String -> Effect Unit, fromUser :: (String -> Effect Unit) -> Effect Unit }
textFieldWiring comp inputNode mDebounce = do
  mPropRef <- Ref.new Nothing
  pure
    { toUser: \newa -> do
        focused <- isFocused inputNode
        unless focused $ setStringProp "value" comp newa
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop newa
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        case mDebounce of
          Nothing -> void $ addEventListener "input" inputNode $ const do
            value <- getValue inputNode
            prop value
          Just millis -> onInputDebounced inputNode millis prop
    }

filledTextArea :: { columns :: Int, rows :: Int } -> PUI Web { value :: String } { value :: String }
filledTextArea { columns, rows } = field @"value" $ wrap do
  inputNode <- element "label" do
    _ <- unwrap (span >>> cl "mdc-text-field__ripple" $ pempty)
    node <- element "span" do
      element "textarea" (pure unit)
      clazz "mdc-text-field__input"
      attribute "rows" (show rows)
      attribute "cols" (show columns)
      attribute "aria-label" "Label"
      gets _.sibling
    clazz "mdc-text-field__resizer"
    _ <- unwrap (span >>> cl "mdc-line-ripple" $ pempty)
    pure node
  clazz "mdc-text-field"
  clazz "mdc-text-field--filled"
  clazz "mdc-text-field--textarea"
  clazz "mdc-text-field--no-label"
  fieldNode <- gets _.sibling
  comp <- liftEffect $ newComponent material.textField."MDCTextField" fieldNode
  liftEffect $ textFieldWiring comp inputNode Nothing

-- | Label content is chrome (`{} → {}`, announcing); a real `<label
-- | for=…>` wrapper associates it, so clicking the text toggles the box
-- | (and any `{} → {}` content works, even a bare text node).
checkbox :: forall a. Default a => PUI Web {} {} -> PUI Web { value :: Maybe a } { value :: Maybe a }
checkbox labelContent = field @"value" $ wrap do
  checkboxId <- liftEffect uniqueId
  aRef <- liftEffect $ Ref.new default
  mPropRef <- liftEffect $ Ref.new Nothing
  parts <- element "div" do
    inputNode <- element "div" do
      element "input" (pure unit)
      clazz "mdc-checkbox__native-control"
      attribute "type" "checkbox"
      attribute "id" checkboxId
      node <- gets _.sibling
      _ <- unwrap $ div >>> cl "mdc-checkbox__background" $ RecordToRecord.do
        staticHTML """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        div >>> cl "mdc-checkbox__mixedmark" $ pempty
      _ <- unwrap (div >>> cl "mdc-checkbox__ripple" $ pempty)
      _ <- unwrap (div >>> cl "mdc-checkbox__focus-ring" $ pempty)
      pure node
    clazz "mdc-checkbox"
    cbNode <- gets _.sibling
    cbComp <- liftEffect $ newComponent material.checkbox."MDCCheckbox" cbNode
    lbl <- unwrap (label >>> "for" := checkboxId $ labelContent)
    pure { inputNode, cbComp, lbl }
  clazz "mdc-form-field"
  ffNode <- gets _.sibling
  ffComp <- liftEffect $ newComponent material.formField."MDCFormField" ffNode
  liftEffect $ setFormFieldInput ffComp parts.cbComp
  pure
    { toUser: \ma -> do
        parts.lbl.toUser {}
        case ma of
          Nothing -> setChecked parts.inputNode false
          Just newa -> do
            setChecked parts.inputNode true
            Ref.write newa aRef
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop ma
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        listenNode parts.inputNode "change" do
          checked <- getChecked parts.inputNode
          a <- Ref.read aRef
          prop (if checked then Just a else Nothing)
    }

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
      liftEffect do
        radioNode <- querySelectorIn root ".mdc-radio"
        radioComp <- newComponent material.radio."MDCRadio" radioNode
        ffComp <- newComponent material.formField."MDCFormField" root
        setFormFieldInput ffComp radioComp
      pure { inputNode, value: o.value }
    mPropRef <- liftEffect $ Ref.new Nothing
    let render ma = for_ members \m -> setChecked m.inputNode (Just m.value == ma)
    liftEffect $ for_ members \m -> listenNode m.inputNode "change" do
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop m.value
    pure
      { toUser: \ma -> do
          render ma
          -- leaf echo (output is the bare selection, so only a `Just` echoes)
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> for_ ma \a' -> prop a'
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }
  where
  optionMarkup groupName uid lbl =
    "<div class=\"mdc-form-field\">"
      <> "<div class=\"mdc-radio\">"
      <> "<input class=\"mdc-radio__native-control\" type=\"radio\" id=\"" <> uid <> "\" name=\"" <> groupName <> "\">"
      <> "<div class=\"mdc-radio__background\"><div class=\"mdc-radio__outer-circle\"></div><div class=\"mdc-radio__inner-circle\"></div></div>"
      <> "<div class=\"mdc-radio__ripple\"></div>"
      <> "<div class=\"mdc-radio__focus-ring\"></div>"
      <> "</div>"
      <> "<label for=\"" <> uid <> "\">" <> lbl <> "</label>"
      <> "</div>"

-- | The MD2 Switch, a `×→×` `Boolean` editor (the name `switch` was
-- | already taken by the `+→+` case selector).
toggleSwitch :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
toggleSwitch config = field @"value" (switchLeaf config.label)

switchLeaf :: String -> PUI Web Boolean Boolean
switchLeaf lbl = div >>> "style" := "display: flex; align-items: center; gap: 8px;" $ wrap do
  switchId <- liftEffect uniqueId
  _ <- unwrap (staticHTML (switchMarkup switchId))
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.switchControl."MDCSwitch" node
  _ <- unwrap (staticHTML ("<label for=\"" <> switchId <> "\">" <> lbl <> "</label>"))
  mPropRef <- liftEffect $ Ref.new Nothing
  -- MDCSwitch toggles itself on click; read the post-toggle state
  liftEffect $ listenNode node "click" do
    selected <- getBoolProp "selected" comp
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop selected
  pure
    { toUser: \b -> do
        setBoolProp "selected" comp b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  switchMarkup switchId =
    "<button id=\"" <> switchId <> "\" class=\"mdc-switch mdc-switch--unselected\" type=\"button\" role=\"switch\" aria-checked=\"false\">"
      <> """
      <div class="mdc-switch__track"></div>
      <div class="mdc-switch__handle-track">
        <div class="mdc-switch__handle">
          <div class="mdc-switch__shadow"><div class="mdc-elevation-overlay"></div></div>
          <div class="mdc-switch__ripple"></div>
          <div class="mdc-switch__focus-ring-wrapper"><div class="mdc-switch__focus-ring"></div></div>
          <div class="mdc-switch__icons">
            <svg class="mdc-switch__icon mdc-switch__icon--on" viewBox="0 0 24 24"><path d="M19.69,5.23L8.96,15.96l-4.65-4.65L3,12.62l6.31,6.31l12-12L19.69,5.23z" /></svg>
            <svg class="mdc-switch__icon mdc-switch__icon--off" viewBox="0 0 24 24"><path d="M20 13H4v-2h16v2z" /></svg>
          </div>
        </div>
      </div>
    </button>"""

-- | The `×→×` editor of a **bounded quantity** — the whole business datum
-- | `{ current, min, max, step }` rides the canonical row: the constraints
-- | are model data, never UI literals (guardrail A8's channel-fed
-- | resolution), so they arrive from the seed — pointedness makes a
-- | missing bound a compile error at `body` — and may change at runtime
-- | (the leaf re-scopes in place; a bounds change re-initializes the MDC
-- | foundation, a value change just moves the thumb). `step` is `Just` for
-- | the discrete slider, `Nothing` for the continuous one. Emits on
-- | **commit** only (thumb release), the whole quantity with `current`
-- | replaced — an editor cannot invent its own bounds — so an `updated`
-- | fold sees each drag as a single transaction. For continuous mid-drag
-- | emissions (live readouts), use `sliderLive`.
slider :: { label :: String } -> PUI Web { value :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } { value :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
slider config = field @"value" (sliderLeaf false config.label)

-- | `slider` emitting continuously mid-drag (like mid-typing text); a
-- | consumer that doesn't want the burst wraps its stage in `debounced`.
sliderLive :: { label :: String } -> PUI Web { value :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } } { value :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number } }
sliderLive config = field @"value" (sliderLeaf true config.label)

-- `MDCSlider`'s value API is method-based (`getValue`/`setValue`), the one
-- foundation here off the property-wiring convention; its bounds are
-- read from the DOM at construction only, so a bounds change rewrites the
-- input attributes and constructs a fresh foundation over the same markup
sliderLeaf :: Boolean -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: Maybe Number } { current :: Number, min :: Number, max :: Number, step :: Maybe Number }
sliderLeaf live label = wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  stateRef <- liftEffect $ Ref.new Nothing
  let
    emit comp = do
      v <- getSliderValue comp
      st <- Ref.read stateRef
      for_ st \s -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop (s.quantity { current = v })
    scoped q = do
      st <- Ref.read stateRef
      case st of
        Just s | s.quantity.min == q.min, s.quantity.max == q.max, s.quantity.step == q.step -> do
          Ref.write (Just s { quantity = q }) stateRef
          pure s.comp
        _ -> do
          for_ st \s -> destroyComponent s.comp
          configureMdcSlider node q.min q.max (fromMaybe 0.0 q.step) (isJust q.step) q.current
          comp <- newComponent material.slider."MDCSlider" node
          when live $ listen comp "MDCSlider:input" (emit comp)
          listen comp "MDCSlider:change" (emit comp)
          Ref.write (Just { comp, quantity: q }) stateRef
          pure comp
  pure
    { toUser: \q -> do
        comp <- scoped q
        setSliderValue comp q.current
        -- construction may have happened before styles applied; re-measure
        layoutComponent comp
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop q
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup =
    "<div class=\"mdc-slider\" style=\"min-width: 200px;\">"
      <> "<input class=\"mdc-slider__input\" type=\"range\" min=\"0\" max=\"100\" value=\"0\""
      <> " aria-label=\"" <> label <> "\">"
      <> "<div class=\"mdc-slider__track\">"
      <> "<div class=\"mdc-slider__track--inactive\"></div>"
      <> "<div class=\"mdc-slider__track--active\"><div class=\"mdc-slider__track--active_fill\"></div></div>"
      <> "</div>"
      <> "<div class=\"mdc-slider__thumb\">"
      <> "<div class=\"mdc-slider__thumb-knob\"></div>"
      <> "</div>"
      <> "</div>"

-- | The MD2 exposed dropdown menu (filled select), a `×→×` editor.
-- | Type-changing like `radioButton @l`: the input field holds the
-- | selection state (`Maybe a`), the output field the bare selection
-- | (`a`). Options are design-system config.
select :: forall a. Eq a => { floatingLabel :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
select config options = field @"value" (selectLeaf config options)

selectLeaf :: forall a. Eq a => { floatingLabel :: String } -> Array { value :: a, label :: String } -> PUI Web (Maybe a) a
selectLeaf config options = wrap do
  labelId <- liftEffect uniqueId
  textId <- liftEffect uniqueId
  _ <- unwrap (staticHTML (markup labelId textId))
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.select."MDCSelect" node
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic selection fires MDCSelect:change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listen comp "MDCSelect:change" do
    busy <- Ref.read busyRef
    unless busy do
      idx <- getIntProp "selectedIndex" comp
      for_ (options !! idx) \o -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop o.value
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setIntProp "selectedIndex" comp idx
          Nothing -> setIntProp "selectedIndex" comp (-1)
        Ref.write false busyRef
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup labelId textId =
    "<div class=\"mdc-select mdc-select--filled\" style=\"min-width: 200px;\">"
      <> "<div class=\"mdc-select__anchor\" role=\"button\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-labelledby=\"" <> labelId <> " " <> textId <> "\">"
      <> "<span class=\"mdc-select__ripple\"></span>"
      <> "<span class=\"mdc-floating-label\" id=\"" <> labelId <> "\">" <> config.floatingLabel <> "</span>"
      <> "<span class=\"mdc-select__selected-text-container\"><span class=\"mdc-select__selected-text\" id=\"" <> textId <> "\"></span></span>"
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
    "<li class=\"mdc-deprecated-list-item\" data-value=\"" <> show idx <> "\" role=\"option\" aria-selected=\"false\">"
      <> "<span class=\"mdc-deprecated-list-item__ripple\"></span>"
      <> "<span class=\"mdc-deprecated-list-item__text\">" <> o.label <> "</span>"
      <> "</li>"

-- | The MD2 single-select segmented button, a `×→×` editor. Type-changing
-- | like `select @l`; selection styling is CSS-class-driven, so the
-- | wiring is hand-rolled per segment, as in `PUI.MDC3`.
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
    let render msel = for_ segments \seg -> do
          setClassIf seg.node "mdc-segmented-button__segment--selected" (Just seg.value == msel)
          setAttribute seg.node "aria-checked" (if Just seg.value == msel then "true" else "false")
    liftEffect $ for_ segments \seg -> listenNode seg.node "click" do
      render (Just seg.value)
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop seg.value
    pure
      { toUser: \ma -> do
          render ma
          -- leaf echo (output is the bare selection, so only a `Just` echoes)
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> for_ ma \a' -> prop a'
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
  actionNode <- liftEffect $ querySelectorIn node ".mdc-chip__primary-action"
  stateRef <- liftEffect $ Ref.new false
  mPropRef <- liftEffect $ Ref.new Nothing
  let render b = do
        setClassIf node "mdc-chip--selected" b
        setAttribute actionNode "aria-checked" (if b then "true" else "false")
  liftEffect $ listenNode node "click" do
    b <- not <$> Ref.read stateRef
    Ref.write b stateRef
    render b
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop b
  pure
    { toUser: \b -> do
        Ref.write b stateRef
        render b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop b
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
    on' <- getBoolProp "on" comp
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop on'
  pure
    { toUser: \b -> do
        setBoolProp "on" comp b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup =
    "<button class=\"mdc-icon-button\" aria-label=\"" <> config.label <> "\" aria-pressed=\"false\">"
      <> "<div class=\"mdc-icon-button__ripple\"></div>"
      <> "<span class=\"mdc-icon-button__focus-ring\"></span>"
      <> "<i class=\"material-icons mdc-icon-button__icon mdc-icon-button__icon--on\">" <> config.onIcon <> "</i>"
      <> "<i class=\"material-icons mdc-icon-button__icon\">" <> config.offIcon <> "</i>"
      <> "</button>"

-- | The MD2 tab bar, a `×→×` editor like `segmentedButton @l` but
-- | **same-type** (`Cons l a () s`): the selection is always known from the
-- | input, so it echoes unconditionally and sits happily inside `looped`
-- | ensembles (selection field + `provided` payload panes). One tab per
-- | option; `MDCTabBar` drives activation — indicator transitions,
-- | `aria-selected`, and arrow-key navigation come from the foundation.
tabBar
  :: forall provided a
   . Eq a
  => ConvertOptionsWithDefaults OptIcon { icon :: Maybe String } { | provided } { value :: a, label :: String, icon :: Maybe String }
  => Array { | provided }
  -> PUI Web { value :: a } { value :: a }
tabBar options = field @"value" (tabBarLeaf (convertOptionsWithDefaults OptIcon { icon: Nothing } <$> options))

tabBarLeaf :: forall a. Eq a => Array { value :: a, label :: String, icon :: Maybe String } -> PUI Web a a
tabBarLeaf options = wrap do
  _ <- unwrap $ div >>> cl "mdc-tab-bar" >>> "role" := "tablist" $
    div >>> cl "mdc-tab-scroller" $
      div >>> cl "mdc-tab-scroller__scroll-area" $
        div >>> cl "mdc-tab-scroller__scroll-content" $
          staticHTML (foldMap tabMarkup options)
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.tabBar."MDCTabBar" node
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic activateTab fires MDCTabBar:activated too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ onTabBarActivated comp \idx -> do
    busy <- Ref.read busyRef
    unless busy do
      for_ (options !! idx) \o -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop o.value
  pure
    { toUser: \a -> do
        for_ (findIndex (\o -> o.value == a) options) \idx -> do
          Ref.write true busyRef
          activateTab comp idx
          Ref.write false busyRef
        -- leaf echo: the selection is always known, so always announce
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop a
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  tabMarkup o =
    "<button class=\"mdc-tab\" role=\"tab\" aria-selected=\"false\" tabindex=\"-1\">"
      <> "<span class=\"mdc-tab__content\">"
      <> (case o.icon of
            Just icon' -> "<span class=\"mdc-tab__icon material-icons\" aria-hidden=\"true\">" <> icon' <> "</span>"
            Nothing -> "")
      <> "<span class=\"mdc-tab__text-label\">" <> o.label <> "</span>"
      <> "</span>"
      <> "<span class=\"mdc-tab-indicator\"><span class=\"mdc-tab-indicator__content mdc-tab-indicator__content--underline\"></span></span>"
      <> "<span class=\"mdc-tab__ripple\"></span>"
      <> "</button>"

-- | The `×→×` display citizen for async progress: `{ busy } → {}`, the
-- | shape `PUI.action`'s progress slot expects. Closed while idle (the
-- | foundation's open/close protocol does the visibility).
indeterminateLinearProgress :: PUI Web { busy :: Boolean } {}
indeterminateLinearProgress = wrap do
  _ <- unwrap $ div >>> "role" := "progressbar" >>> cl "mdc-linear-progress" >>> cl "mdc-linear-progress--indeterminate" >>> "aria-label" := "Progress Bar" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" >>> "aria-valuenow" := "0" $ linearProgressInnards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.linearProgress."MDCLinearProgress" node
  liftEffect $ close comp
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        if r.busy then open comp else close comp
        -- display echo (like `text`): announce the `{}` per feed, so gated
        -- merges and `tapped`/`completed` stages keep flowing
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | The **determinate** linear progress display, a `{ value :: Number } → {}`
-- | display citizen: `value` is the filled fraction (0.0–1.0). The gauge
-- | shape: `linearProgress # projected fraction`.
linearProgress :: PUI Web { value :: Number } {}
linearProgress = wrap do
  _ <- unwrap $ div >>> "role" := "progressbar" >>> cl "mdc-linear-progress" >>> "aria-label" := "Progress" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" $ linearProgressInnards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.linearProgress."MDCLinearProgress" node
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setNumberProp "progress" comp r.value
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- the documented buffer/bar innards shared by both linear variants
linearProgressInnards :: PUI Web {} {}
linearProgressInnards = RecordToRecord.do
  div >>> cl "mdc-linear-progress__buffer" $ RecordToRecord.do
    div >>> cl "mdc-linear-progress__buffer-bar" $ pempty
    div >>> cl "mdc-linear-progress__buffer-dots" $ pempty
  div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__primary-bar" $
    span >>> cl "mdc-linear-progress__bar-inner" $ pempty
  div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__secondary-bar" $
    span >>> cl "mdc-linear-progress__bar-inner" $ pempty

-- | `indeterminateLinearProgress`'s circular sibling — the same
-- | `{ busy } → {}` display citizen.
indeterminateCircularProgress :: PUI Web { busy :: Boolean } {}
indeterminateCircularProgress = wrap do
  _ <- unwrap $ div >>> cl "mdc-circular-progress" >>> cl "mdc-circular-progress--indeterminate" >>> "style" := "width: 48px; height: 48px;" >>> "role" := "progressbar" >>> "aria-label" := "Progress" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" $ staticHTML innards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.circularProgress."MDCCircularProgress" node
  liftEffect $ close comp
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        if r.busy then open comp else close comp
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }
  where
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

-- the MD2 typography scale, via the `mdc-typography--*` classes from the
-- prebuilt stylesheet

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

body1 :: Ocular (PUI Web)
body1 w = p w # cl "mdc-typography--body1"

body2 :: Ocular (PUI Web)
body2 w = p w # cl "mdc-typography--body2"

caption :: Ocular (PUI Web)
caption w = span w # cl "mdc-typography--caption"

overline :: Ocular (PUI Web)
overline w = span w # cl "mdc-typography--overline"

-- MD2 elevation levels as surface decorators (`mdc-elevation--z*` from the
-- prebuilt stylesheet; levels 10 and 20 pad like `PUI.MDC3`'s
-- `elevation3`/`elevation5`)

elevation1 :: Ocular (PUI Web)
elevation1 w = div w # cl "mdc-elevation--z1"

elevation10 :: Ocular (PUI Web)
elevation10 w = div w # cl "mdc-elevation--z10" # "style" := "padding: 25px"

elevation20 :: Ocular (PUI Web)
elevation20 w = div w # cl "mdc-elevation--z20" # "style" := "padding: 25px"

-- | A card with an optional caption — the caption is design-system config
-- | (like `filledTextField`'s `floatingLabel`). The card is content-agnostic
-- | (any polarity), so its caption chrome is hand-fused, not merged. The
-- | caption defaults to none: `card {}` is captionless, `card { caption:
-- | "Title" }` labels it.
card
  :: forall provided
   . ConvertOptionsWithDefaults OptLabel { caption :: Maybe String } { | provided } { caption :: Maybe String }
  => { | provided }
  -> Ocular (PUI Web)
card provided content =
  div >>> cl "mdc-card" >>> "style" := "padding: 10px; margin: 15px 0 15px 0; text-align: justify;" $ wrap do
    for_ mCaption \c -> void $ unwrap (caption $ staticText c)
    unwrap content
  where
  { caption: mCaption } = convertOptionsWithDefaults OptLabel { caption: Nothing } provided :: { caption :: Maybe String }

-- | The MD2 card button-row area: a flex row for a group of buttons, so they
-- | sit inline at their natural width instead of stretching down the card's
-- | flex column. Wrap a button group: `cardActions $ RecordToVariant.do …`.
cardActions :: Ocular (PUI Web)
cardActions = div >>> cl "mdc-card__actions"

-- | Modal ocular with the open-on-feed/close-on-emission protocol: the
-- | dialog opens (via the MDC foundation — animation, scrim, Esc) whenever
-- | a value is fed, and closes when its content emits, so feed it
-- | selectively (behind an event case), put the deciding emitters inside,
-- | and the emission both closes the dialog and flows on. The content's
-- | final stage must emit only on decision (buttons, `clicked`) — an
-- | echoing display there would close the dialog the moment it opens.
dialog :: { title :: String } -> Ocular (PUI Web)
dialog { title } content = wrap do
  titleId <- liftEffect uniqueId
  contentId <- liftEffect uniqueId
  unwrap $ div >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") open close $ wrap do
    result <- unwrap $
      div >>> cl "mdc-dialog__container" $
        div >>> cl "mdc-dialog__surface" >>> "role" := "alertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := titleId >>> "aria-describedby" := contentId $ wrap do
          _ <- unwrap (h2 >>> cl "mdc-dialog__title" >>> "id" := titleId $ staticText title)
          unwrap (div >>> cl "mdc-dialog__content" >>> "id" := contentId $ content)
    _ <- unwrap (div >>> cl "mdc-dialog__scrim" $ pempty)
    pure result

-- | `dialog` with a built-in confirm action: same open-on-feed protocol,
-- | and the confirm button is a `clicked` pass-through — clicking it
-- | emits the content's last output (so give displays a `# tapped`),
-- | which closes the dialog and flows on.
simpleDialog :: { title :: String, confirm :: String } -> Ocular (PUI Web)
simpleDialog { title, confirm } content = wrap do
  titleId <- liftEffect uniqueId
  contentId <- liftEffect uniqueId
  unwrap $ div >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") open close $ wrap do
    result <- unwrap $
      div >>> cl "mdc-dialog__container" $
        div >>> cl "mdc-dialog__surface" >>> "role" := "alertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := titleId >>> "aria-describedby" := contentId $ Semigroupoid.do
          wrap do
            _ <- unwrap (h2 >>> cl "mdc-dialog__title" >>> "id" := titleId $ staticText title)
            unwrap (div >>> cl "mdc-dialog__content" >>> "id" := contentId $ content)
          div >>> cl "mdc-dialog__actions" $ eventLeaf $
            el "button" >>> "type" := "button" >>> cl "mdc-button" >>> cl "mdc-dialog__button" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
              div >>> cl "mdc-button__ripple" $ pempty
              span >>> cl "mdc-button__label" $ staticText confirm
    _ <- unwrap (div >>> cl "mdc-dialog__scrim" $ pempty)
    pure result

-- | The `+→×` status receiver: shows message case `l` in a snackbar,
-- | contributing no fields (`text` echoes its `{}`, so it announces).
snackbar :: PUI Web [ event :: String ] {}
snackbar = snackbarContainer $ text # projected eventText

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
-- | MD2-only: MD3 dropped the banner, so `PUI.MDC3` has no citizen for it.
banner :: PUI Web [ event :: String ] {}
banner = bannerContainer $ text # projected eventText
-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = Variant.on (Proxy @"event") identity Variant.case_


bannerContainer :: Ocular (PUI Web)
bannerContainer content = wrap do
  w <- unwrap $ div >>> cl "mdc-banner" >>> "role" := "banner" $
    div >>> cl "mdc-banner__content" >>> "role" := "alertdialog" >>> "aria-live" := "assertive" $ wrap do
      w' <- unwrap (div >>> cl "mdc-banner__graphic-text-wrapper" $ div >>> cl "mdc-banner__text" $ content)
      _ <- unwrap (div >>> cl "mdc-banner__actions" $ staticHTML "<button type=\"button\" class=\"mdc-button mdc-banner__primary-action\"><div class=\"mdc-button__ripple\"></div><div class=\"mdc-button__label\">Dismiss</div></button>")
      pure w'
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.banner."MDCBanner" node
  dismissNode <- liftEffect $ querySelectorIn node ".mdc-banner__primary-action"
  liftEffect $ listenNode dismissNode "click" (closeBanner comp)
  pure
    { toUser: \msg -> do
        open comp
        w.toUser msg
    , fromUser: w.fromUser
    }

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
  liftEffect $ listenNode anchorNode "click" (setBoolProp "open" comp true)
  pure w

-- | Chrome for a group of `filterChip @l`s.
chipSet :: Ocular (PUI Web)
chipSet content =
  div >>> cl "mdc-chip-set" >>> cl "mdc-chip-set--filter" >>> "role" := "grid" $ content

list :: Ocular (PUI Web)
list content = wrap do
  w <- unwrap (ul >>> cl "mdc-deprecated-list" $ content)
  node <- gets _.sibling
  _ <- liftEffect $ newComponent material.list."MDCList" node
  liftEffect $ fixListTabIndexes node
  pure w

-- | MD2 pins the single-line list item at 48px and ellipsis-clips its text
-- | slot — right for text rows, wrong for embedded controls (a segmented
-- | button would be clipped and spill into the next row). The ocular lets
-- | content define the height (48px stays the floor) and lays the text slot
-- | out as a centered row, so mixed content (typography beside a control)
-- | sits side by side unclipped; single-line text rows render as before.
listItem :: Ocular (PUI Web)
listItem content = li >>> cl "mdc-deprecated-list-item" >>> "style" := "height: auto; min-height: 48px;" $ wrap do
  _ <- unwrap (span >>> cl "mdc-deprecated-list-item__ripple" $ pempty)
  unwrap (span >>> cl "mdc-deprecated-list-item__text" >>> "style" := "display: flex; align-items: center; gap: 16px; width: 100%; white-space: normal; overflow: visible;" $ content)

-- | The MD2 list as a **dynamic collection component**: one item widget per
-- | array element; items satisfying `selected` get the MD2 selected
-- | styling (optional — `listOf {}` selects nothing); every item is a
-- | click emitter replaying its own value, so the component's output is
-- | the clicked item.
listOf
  :: forall provided i a o
   . ConvertOptionsWithDefaults OptSelected { selected :: a -> Boolean } { | provided } { selected :: a -> Boolean }
  => { | provided }
  -> (i -> Array a)
  -> PUI Web a o
  -> PUI Web i a
listOf provided f item = wrap do
  w <- unwrap $ ul >>> cl "mdc-deprecated-list" >>> "style" := "overflow-y: auto;" $
    ( inRow ( clicked $ clWhen config.selected "mdc-deprecated-list-item--selected"
          $ li >>> cl "mdc-deprecated-list-item" >>> "style" := "cursor: pointer;" $ item
      ) # foreach @"ix" (mapWithIndex (\ix it -> { ix, item: it }) <<< f)
    )
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.list."MDCList" node
  pure
    { toUser: \items -> do
        w.toUser items
        fixListTabIndexes node
        layoutComponent comp
    , fromUser: w.fromUser
    }
  where
  config = convertOptionsWithDefaults OptSelected { selected: const false } provided

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
      , fromUser: \prop -> prop {}
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

-- | The permanent navigation drawer with a **live nav slot**: nav and
-- | content are sibling stages over the same types — both see every value
-- | fed, and either side's emissions exit the drawer, so a selectable nav
-- | (a `listOf` of sections folded via `updated`) drives the content
-- | beside it.
drawer :: forall i o. { title :: String, subtitle :: String } -> PUI Web i o -> PUI Web i o -> PUI Web i o
drawer config nav content = div >>> "style" := "display: flex;" $ wrap do
  nav' <- unwrap (aside >>> cl "mdc-drawer" $ wrap do
    _ <- unwrap (staticHTML ("<div class=\"mdc-drawer__header\"><h3 class=\"mdc-drawer__title\">" <> config.title <> "</h3><h6 class=\"mdc-drawer__subtitle\">" <> config.subtitle <> "</h6></div>"))
    unwrap (div >>> cl "mdc-drawer__content" $ nav))
  content' <- unwrap (div >>> cl "mdc-drawer-app-content" >>> "style" := "flex: 1; padding: 16px;" $ content)
  pure
    { toUser: \i' -> do
        nav'.toUser i'
        content'.toUser i'
    , fromUser: \prop -> do
        nav'.fromUser prop
        content'.fromUser prop
    }

-- | Attach a hover/focus tooltip to the wrapped element (single-element
-- | content: the anchor is the content's root node). An annotation, not
-- | a container — it reads best trailing, widget first:
-- | `checkbox (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }`.
tooltip :: { text :: String } -> Ocular (PUI Web)
tooltip config content = wrap do
  tipId <- liftEffect uniqueId
  w <- unwrap ("aria-describedby" := tipId $ content)
  _ <- unwrap (staticHTML ("<div id=\"" <> tipId <> "\" class=\"mdc-tooltip\" role=\"tooltip\" aria-hidden=\"true\"><div class=\"mdc-tooltip__surface mdc-tooltip__surface-animation\">" <> config.text <> "</div></div>"))
  tipNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.tooltip."MDCTooltip" tipNode
  pure w

-- announcing statics (`{} → {}` chrome with a face)

divider :: PUI Web {} {}
divider = staticHTML "<hr class=\"mdc-deprecated-list-divider\" style=\"width: 100%;\">"

imageListItem :: { src :: String, label :: String } -> PUI Web {} {}
imageListItem config = staticHTML $
  "<li class=\"mdc-image-list__item\" style=\"margin-bottom: 16px;\">"
    <> "<img class=\"mdc-image-list__image\" src=\"" <> config.src <> "\" alt=\"" <> config.label <> "\">"
    <> "<div class=\"mdc-image-list__supporting\"><span class=\"mdc-image-list__label\">" <> config.label <> "</span></div>"
    <> "</li>"

-- the element adapter for the index-keyed internal collection: reads the
-- item out of the reconciler's { ix, item } row at the wiring level (the
-- closed-singleton adopters deliberately do not read from wider rows)
inRow :: forall a o. PUI Web a o -> PUI Web { ix :: Int, item :: a } o
inRow w = wrap $ unwrap w <#> \w' -> { toUser: \r -> w'.toUser r.item, fromUser: w'.fromUser }

-- Private

foreign import data Component :: Type
foreign import data ComponentClass :: Type

-- foundation construction and its method-based protocols
foreign import newComponent :: ComponentClass -> Node -> Effect Component
foreign import open :: Component -> Effect Unit
foreign import close :: Component -> Effect Unit
foreign import closeBanner :: Component -> Effect Unit
foreign import layoutComponent :: Component -> Effect Unit
foreign import activateTab :: Component -> Int -> Effect Unit
foreign import onTabBarActivated :: Component -> (Int -> Effect Unit) -> Effect Unit
foreign import getSliderValue :: Component -> Effect Number
foreign import setSliderValue :: Component -> Number -> Effect Unit
foreign import setFormFieldInput :: Component -> Component -> Effect Unit

-- property access — MDC foundations expose their model as component
-- properties (value, selected, selectedIndex, on, open, progress, ...),
-- the same mechanism as `PUI.MDC3`'s element properties
foreign import setStringProp :: String -> Component -> String -> Effect Unit
foreign import setNumberProp :: String -> Component -> Number -> Effect Unit
foreign import setIntProp :: String -> Component -> Int -> Effect Unit
foreign import getIntProp :: String -> Component -> Effect Int
foreign import setBoolProp :: String -> Component -> Boolean -> Effect Unit
foreign import getBoolProp :: String -> Component -> Effect Boolean

-- component/node events and DOM odds and ends
foreign import listen :: Component -> String -> Effect Unit -> Effect Unit
foreign import destroyComponent :: Component -> Effect Unit
foreign import configureMdcSlider :: Node -> Number -> Number -> Number -> Boolean -> Number -> Effect Unit
foreign import listenNode :: Node -> String -> Effect Unit -> Effect Unit
foreign import setClassIf :: Node -> String -> Boolean -> Effect Unit
foreign import querySelectorIn :: Node -> String -> Effect Node
foreign import fixListTabIndexes :: Node -> Effect Unit

foreign import material
  :: { textField :: { "MDCTextField" :: ComponentClass }
     , ripple :: { "MDCRipple" :: ComponentClass }
     , tabBar :: { "MDCTabBar" :: ComponentClass }
     , dialog :: { "MDCDialog" :: ComponentClass }
     , snackbar :: { "MDCSnackbar" :: ComponentClass }
     , banner :: { "MDCBanner" :: ComponentClass }
     , radio :: { "MDCRadio" :: ComponentClass }
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
