-- | The **Material Design 2** vocabulary (https://m2.material.io) — the
-- | catalogue as far as MDC Web implements it, and the twin of
-- | `PUI.Web.MDC3`: where a concept survived into Material 3 it keeps the
-- | same name and the same signature there, so a screen changes design
-- | system by changing this one import.
-- |
-- | **The page must load** the prebuilt MDC Web stylesheet, the Material
-- | Icons font and Roboto. Unlike `PUI.Web.MDC3`, whose component styles
-- | ship in the bundle, Material 2's come from the page.
-- |
-- | The catalogue, by what the user does with it:
-- |
-- |   * **enter a value** — `filledTextField`/`outlinedTextField` (and
-- |     `debouncedTextField`), `filledTextArea`, `slider`/`sliderLive`,
-- |     `checkbox`, `toggleSwitch`, `filterChip`, `iconToggle`
-- |   * **choose among options** — `radioButton` and `segmentedButton` (a
-- |     handful, all visible), `select` (a longer list), `tabBar` (the
-- |     sections of a screen), `listOf` (a list built from data, picked by
-- |     clicking a row)
-- |   * **act** — `button`, with `outlinedButton` and `textButton` at the
-- |     lower emphasis levels; `fab`; `iconButton`; `menu`/`menuItem`
-- |   * **be told something** — `snackbar` (passing), `banner` (until
-- |     dismissed), `dialog`/`simpleDialog` (must be answered),
-- |     `linearProgress`, `indeterminateLinearProgress`,
-- |     `indeterminateCircularProgress`, `tooltip`
-- |   * **structure and surface** — `card`/`cardActions`, `list`/`listItem`,
-- |     `dataTable`/`dataRow`/`dataCell`, `imageList`/`imageListItem`,
-- |     `layoutGrid`/`layoutCell`, `topAppBar`, `drawer`, `chipSet`,
-- |     `divider`, the type scale (`headline1` … `overline`) and the
-- |     elevations (`elevation1`/`elevation10`/`elevation20`)
-- |
-- | Anything in the Material 2 catalogue that MDC Web never implemented
-- | (backdrop, bottom app bar, bottom navigation, date pickers, navigation
-- | rail, sheets) is absent here too: this vocabulary offers only what the
-- | design system actually ships.
module PUI.Web.MDC2
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
  , imagePane
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
  , confirmed
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
import Data.Foldable (foldMap, for_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Traversable (for)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (Ocular, PUI, blank, foreach, projected, static)
import PUI.Web.HTML (aside, attrWith, cl, clWhen, clicked, div, el, h1, h2, h3, h4, h5, h6, i, img, init, label, li, p, shown, span, staticText, table, tbody, td, text, th, thead, tr, ul, (:=))
import PUI.Web (Node, Web, OptCaption(..), staticHTML, addEventListener, attribute, clazz, element, getChecked, getValue, isFocused, onInputDebounced, setAttribute, setChecked, uniqueId)
import QualifiedDo.Semigroupoid as Semigroupoid
import Prim.Row (class Cons, class Union)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Record (get) as Record
import Type.Proxy (Proxy(..))

-- Implementation notes — the reference above is the contract.
--
-- Material Design 2 (https://m2.material.io) components implemented as
-- PUI Web/UIOcular (PUI Web) datatypes — the MD2 sibling of `PUI.Web.MDC3`,
-- built on the MDC Web foundation classes from `material-components-web`:
-- a component leaf is the catalog's documented DOM markup plus a foundation
-- instance (`newComponent material.x."MDCX"`) wired through its documented
-- properties and events — the foundation owns ripples, label float,
-- activation and aria, exactly as the MDC Web docs prescribe. The
-- vocabulary is two-sorted, with the same citizenship and (where the
-- concept exists in both design systems) the same names and signatures as
-- `PUI.Web.MDC3`, so a demo switches design systems by switching the import:
--
--   * **components** — UI components with a model interface, every one a citizen
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
--         banner; MD3 dropped it, so `PUI.Web.MDC3` has no citizen for it).
--     No scalar or polymorphic component interfaces. Variant *editing* has
--     no `+→+` component citizens: it goes through record-shaped editor
--     state (`dimap`-bracketed `looped` pipelines — a selection component
--     followed by editor panes, each `# inCase @l <selectionOf>` existing
--     while the selection sits at its case — see the demos); `+→+`
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
-- Internally the live leaf of a compound is `field @l`-lifted — `field`
-- is the `Strong` field lens, so every editor is a whole-row citizen
-- `p { l | rest } { l | rest }`: fed the wide row it edits its field, and
-- each emission re-attaches the background the lens retains (runtime
-- completeness by construction; freshness rests on the enclosing loop's
-- re-broadcast) — and its chrome is hand-fused in the `Web` monad
-- (decoration as implementation technique — and a necessity: abstract
-- labels cannot flow through the merges' `Nub`, so a skolem-labeled
-- operand can't be merged); all-chrome groups (button content, progress
-- bars) have concrete rows and stay literal `RecordToRecord.do` merges of
-- announcing chrome (`staticText`/`staticHTML`/`static` at `{} → {}`).
-- Code order = DOM order throughout.
--
-- **The `dimap` round-trip contract for editors.** An editor bracketed by
-- `dimap f g` behaves as an iso lens: `f` maps the model into the editor's
-- canonical value, `g` maps edits back. If `g >>> f` is not the identity
-- on the values the user actually enters, the field visibly *normalizes*
-- input on each echo — the focus guard suppresses this only for the field
-- currently being typed in. Conversions that can fail or lose information
-- (parse-then-format, unit conversion over strings) belong in the model
-- (a `settled` normalization on the whole-row stage), not in a leaf
-- bracket — see the temperature-converter demo.

-- UIs

-- Conversion tags scope which field names lift a bare value to `Just`, as
-- in `PUI.Web.MDC3`: an existing `Maybe` passes through, every other field
-- passes through at its given type. A field name is optional on some
-- components and required on others — `label` is optional on `button`,
-- required on `slider`; `icon` is optional on `button`, required on `fab`
-- — so the *tag*, not a global per-symbol instance, decides which fields
-- are optional for a given UI component. One tag per distinct optional-field
-- set: `OptLabelIcon` (buttons), `OptLabel` (fab),
-- `OptSelected` (listOf), `OptIcon` (tabBar options).
-- | Marks `label` and `icon` as optional on the buttons — write either,
-- | both or neither, as a plain string.
data OptLabelIcon = OptLabelIcon

instance ConvertOption OptLabelIcon "label" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabelIcon "icon" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabelIcon sym a a where
  convertOption _ _ = identity

-- | Marks the `label` of a FAB as optional.
data OptLabel = OptLabel

instance ConvertOption OptLabel "label" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptLabel sym a a where
  convertOption _ _ = identity

-- | Marks a list's `selected` test as optional — `listOf {}` selects
-- | nothing.
data OptSelected = OptSelected

instance ConvertOption OptSelected sym a a where
  convertOption _ _ = identity

-- | Marks the `icon` of a tab as optional — tabs may be labelled, iconed or
-- | both.
data OptIcon = OptIcon

instance ConvertOption OptIcon "icon" String (Maybe String) where
  convertOption _ _ = Just
else instance ConvertOption OptIcon sym a a where
  convertOption _ _ = identity

-- | The **raised button** — Material's high-emphasis action and the default
-- | choice; `outlinedButton` and `textButton` are the same button at medium
-- | and low emphasis, for the secondary actions beside it.
-- |
-- | It reports on click, carrying the data it was showing, under the name
-- | the app gives the action: `button @"Book the flight" {}`. Both parts
-- | of the face are optional — the label defaults to the case label
-- | verbatim, so the case *is* the copy (`label:` overrides with copy the
-- | case cannot be), `icon: "add"` puts a Material Icons glyph before the
-- | label.
button
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
button = buttonOf @l (Just "mdc-button--raised")

-- | `button` at medium emphasis — outlined, no fill: an important action
-- | that is not *the* action of the screen.
outlinedButton
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
outlinedButton = buttonOf @l (Just "mdc-button--outlined")

-- | `button` at the lowest emphasis — label only, no fill or outline: the
-- | dismissive or tertiary action (Cancel, Learn more), and what belongs in
-- | dialogs and cards.
textButton
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
textButton = buttonOf @l Nothing

buttonOf
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => Maybe String
  -> { | provided }
  -> PUI Web { | r } [ | cl ]
buttonOf mModifier provided = recordToCase @l $ eventLeaf $
  el "button" >>> cl "mdc-button" >>> modifier >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    static (span >>> cl "mdc-button__ripple")
    static (span >>> cl "mdc-button__focus-ring")
    case config.icon of
      Just icon' -> i >>> cl "material-icons" >>> cl "mdc-button__icon" >>> "aria-hidden" := "true" $ staticText icon'
      Nothing -> blank
    case config.label of
      Just label' -> span >>> cl "mdc-button__label" $ staticText label'
      Nothing -> blank
  where
  config = convertOptionsWithDefaults OptLabelIcon { label: Just (reflectSymbol (Proxy @l)), icon: Nothing } provided :: { label :: Maybe String, icon :: Maybe String }
  modifier = case mModifier of
    Just m -> cl m
    Nothing -> identity

-- the click-emitter protocol over any `{} → {}` element chrome: replay the
-- last value fed on click (a click before any value arrived is withheld) —
-- `clicked` over the input-freed chrome, the last-built element listening
eventLeaf :: forall r. PUI Web {} {} -> PUI Web { | r } { | r }
eventLeaf chrome = clicked chrome

-- | The **floating action button**: the one action a screen is *for*, kept
-- | in view above the content. Reports on click carrying what it was
-- | showing, like `button`. The `icon` is required — a FAB is recognised by
-- | its glyph; the `label` — the extended FAB's words beside the glyph —
-- | defaults to the case label verbatim (`label: Nothing` gives
-- | the icon-only FAB, `label:` copy overrides).
fab
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabel { label :: Maybe String } { | provided } { icon :: String, label :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
fab provided = recordToCase @l $ eventLeaf $
  el "button" >>> cl "mdc-fab" >>> extended >>> "aria-label" := fromMaybe config.icon config.label >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    static (div >>> cl "mdc-fab__ripple")
    static (span >>> cl "mdc-fab__focus-ring")
    span >>> cl "mdc-fab__icon" >>> cl "material-icons" $ staticText config.icon
    case config.label of
      Just label' -> span >>> cl "mdc-fab__label" $ staticText label'
      Nothing -> blank
  where
  config = convertOptionsWithDefaults OptLabel { label: Just (reflectSymbol (Proxy @l)) } provided :: { icon :: String, label :: Maybe String }
  extended = case config.label of
    Just _ -> cl "mdc-fab--extended"
    Nothing -> identity

-- | A compact **icon-only action**, for toolbars, list rows and card
-- | corners where a labelled button would not fit. `label` is not drawn —
-- | it is what assistive technology announces, defaulting to
-- | the case label verbatim. For an icon that stays pressed
-- | (favourite, mute), use `iconToggle` instead.
iconButton :: forall @l provided r cl. IsSymbol l => Cons l { | r } () cl => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { icon :: String, label :: String } => { | provided } -> PUI Web { | r } [ | cl ]
iconButton provided = recordToCase @l $ eventLeaf $
  el "button" >>> cl "mdc-icon-button" >>> cl "material-icons" >>> "aria-label" := config.label >>> "data-mdc-ripple-is-unbounded" := "" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ RecordToRecord.do
    static (div >>> cl "mdc-icon-button__ripple")
    static (span >>> cl "mdc-icon-button__focus-ring")
    staticText config.icon
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { icon :: String, label :: String }

-- | One choice in a `menu`: reports the data it was showing when picked,
-- | and the menu closes itself. The line's text defaults to
-- | the case label verbatim (`label:` overrides with real copy).
menuItem :: forall @l provided r cl. IsSymbol l => Cons l { | r } () cl => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } [ | cl ]
menuItem provided = recordToCase @l $ eventLeaf $
  li >>> cl "mdc-deprecated-list-item" >>> "role" := "menuitem" >>> "tabindex" := "-1" $ RecordToRecord.do
    static (span >>> cl "mdc-deprecated-list-item__ripple")
    span >>> cl "mdc-deprecated-list-item__text" $ staticText config.label
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { label :: String }

-- TODO support input types: email, text, password, number, search, tel, url
-- | The **filled text field** — Material's default single-line input.
-- | `floatingLabel` names the field and rises above the text once there is
-- | any, so the label is never lost while the field is filled in.
-- |
-- | Shows the string it is given and reports each edit; typing is never
-- | interrupted by values arriving from elsewhere. A whole-row citizen:
-- | fed the wide row, it edits field `l` and carries the rest.
filledTextField :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String } => { | provided } -> PUI Web { | r } { | r }
filledTextField provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "filled" Nothing config.floatingLabel)

-- | `filledTextField` in Material's outlined variant — a border instead of
-- | a fill. Same behaviour; pick one variant and keep to it across a form.
outlinedTextField :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String } => { | provided } -> PUI Web { | r } { | r }
outlinedTextField provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "outlined" Nothing config.floatingLabel)

-- | `filledTextField` that waits `ms` after the last keystroke before
-- | reporting — for a field that drives expensive work (a search, a
-- | recomputed preview) and should not fire once per character.
debouncedTextField :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String, ms :: Number } => { | provided } -> PUI Web { | r } { | r }
debouncedTextField provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "filled" (Just config.ms) config.floatingLabel)

-- the raw MD2 text field — scalar, so private; the documented markup per
-- variant plus an `MDCTextField` foundation, values written through the
-- foundation's `value` property so the label float and line ripple stay
-- foundation-managed. Focus-guarded like `Web.input`: model updates never
-- clobber the field being typed in, but still echo so the channel stays
-- live. Debouncing sits at the DOM boundary (`Web.onInputDebounced`),
-- in front of the wire rather than on it, so the field stays loop-safe.
textFieldLeaf :: String -> Maybe Number -> String -> PUI Web String String
textFieldLeaf variant mDebounce floatingLabel = wrap do
  labelId <- liftEffect uniqueId
  inputNode <- element "label" do
    if variant == "outlined"
      then void $ unwrap $ span >>> cl "mdc-notched-outline" $ RecordToRecord.do
        static (span >>> cl "mdc-notched-outline__leading")
        span >>> cl "mdc-notched-outline__notch" $
          span >>> cl "mdc-floating-label" >>> "id" := labelId $ staticText floatingLabel
        static (span >>> cl "mdc-notched-outline__trailing")
      else do
        _ <- unwrap (static (span >>> cl "mdc-text-field__ripple"))
        void $ unwrap (span >>> cl "mdc-floating-label" >>> "id" := labelId $ staticText floatingLabel)
    element "input" (pure unit)
    clazz "mdc-text-field__input"
    attribute "type" "text"
    attribute "aria-labelledby" labelId
    node <- gets _.sibling
    when (variant == "filled") $
      void $ unwrap (static (span >>> cl "mdc-line-ripple"))
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

-- | The **multi-line text field**, sized in `rows` and `columns` of text —
-- | a note, a description, a message. Otherwise `filledTextField`: shows a
-- | string, reports each edit, never interrupts typing.
filledTextArea :: forall @l r rest. IsSymbol l => Cons l String rest r => { columns :: Int, rows :: Int } -> PUI Web { | r } { | r }
filledTextArea { columns, rows } = field @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  inputNode <- element "label" do
    _ <- unwrap (static (span >>> cl "mdc-text-field__ripple"))
    node <- element "span" do
      element "textarea" (pure unit)
      clazz "mdc-text-field__input"
      attribute "rows" (show rows)
      attribute "cols" (show columns)
      attribute "aria-label" "Label"
      gets _.sibling
    clazz "mdc-text-field__resizer"
    _ <- unwrap (static (span >>> cl "mdc-line-ripple"))
    pure node
  clazz "mdc-text-field"
  clazz "mdc-text-field--filled"
  clazz "mdc-text-field--textarea"
  clazz "mdc-text-field--no-label"
  fieldNode <- gets _.sibling
  comp <- liftEffect $ newComponent material.textField."MDCTextField" fieldNode
  liftEffect $ textFieldWiring comp inputNode Nothing

-- | The Material **checkbox**, with its label beside it: the label is
-- | ordinary content (usually a `staticText`), properly associated, so
-- | clicking the words toggles the box and the whole line is a comfortable
-- | target.
-- |
-- | Ticked exactly while the field holds something — ticking reports the
-- | value, clearing reports nothing-chosen — so an optional part of the
-- | model *is* the box's state, with no second flag to keep in step. Use a
-- | checkbox for a fact the user states as part of a form; use
-- | `toggleSwitch` for a setting that takes effect at once.
-- |
-- | `ticked` is what the field holds once ticked, before the model has ever
-- | supplied a value — stated by the caller (`{ ticked: {} }` for a plain
-- | yes/no fact), never conjured from the type.
checkbox :: forall @l a r rest. IsSymbol l => Cons l (Maybe a) rest r => { ticked :: a } -> PUI Web {} {} -> PUI Web { | r } { | r }
checkbox { ticked } labelContent = field @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  checkboxId <- liftEffect uniqueId
  aRef <- liftEffect $ Ref.new ticked
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
        static (div >>> cl "mdc-checkbox__mixedmark")
      _ <- unwrap (static (div >>> cl "mdc-checkbox__ripple"))
      _ <- unwrap (static (div >>> cl "mdc-checkbox__focus-ring"))
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
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop ma
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        listenNode parts.inputNode "change" do
          checked <- getChecked parts.inputNode
          a <- Ref.read aRef
          prop (if checked then Just a else Nothing)
    }

-- | The Material **radio group**: one choice among a handful, every option
-- | visible and comparable at a glance. Beyond about five options, or where
-- | the options don't deserve the space, use `select`.
-- |
-- | Until the user picks there is no choice to show, so the field arrives as
-- | "maybe a choice" and leaves as the choice itself — say which with
-- | `# optional @"chosen" @"unchosen"` (the two states named by the
-- | application; nothing preselected, and whatever needs the choice adopts
-- | the made case) or `# required` (the model always has one).
-- | The options — the value and the words shown for it — belong to the
-- | control, not to the model.
radioButton :: forall @l a ri ro. IsSymbol l => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
radioButton options = field @l $ "name" := reflectSymbol (Proxy @l) $ (radioLeaf options)

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

-- | The Material **switch**: a setting that takes effect the moment it is
-- | flipped — notifications on, dark mode on. (A `checkbox` states a fact
-- | to be submitted with the rest of a form; a switch acts immediately.)
toggleSwitch :: forall @l r rest provided. IsSymbol l => Cons l Boolean rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
toggleSwitch provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (switchLeaf config.label)

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
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
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

-- | The **slider**: a quantity chosen by feel, where the range matters more
-- | than the exact number — a volume, a tip, a budget.
-- |
-- | The range is part of the quantity, not part of the screen:
-- | `{ current, min, max, step }` travels together as one business datum, so
-- | limits come from the data and can change while the app runs (a room's
-- | capacity, a plan's ceiling) — a slider is never silently out of range,
-- | and a range nobody supplied is a compile error rather than a wrong
-- | screen. A `step` makes it discrete, no step continuous.
-- |
-- | It reports on **release**, once per adjustment, so one drag is one
-- | entry in the history — one undo step, one audit line. For a readout
-- | that follows the thumb, use `sliderLive`.
slider :: forall @l r rest provided. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: Maybe Number } rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
slider provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (sliderLeaf false config.label)

-- | `slider` reporting continuously while the thumb moves — for a live
-- | readout or preview that has to follow the drag. Whatever it drives
-- | should be cheap to redo; a drag that should land in the history as one
-- | change needs the plain `slider`, or a `debounced` stage downstream.
sliderLive :: forall @l r rest provided. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: Maybe Number } rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
sliderLive provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (sliderLeaf true config.label)

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
    -- reads the *current* foundation out of the state ref rather than closing
    -- over one: a bounds change replaces the foundation, and a handler holding
    -- the old one would read a destroyed foundation
    emit = do
      st <- Ref.read stateRef
      for_ st \s -> do
        v <- getSliderValue s.comp
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
          Ref.write (Just { comp, quantity: q }) stateRef
          pure comp
  -- the foundation's events surface on the root node, and the node outlives
  -- every rebuild — so listen once here, not per rebuild inside `scoped`
  -- (`MDCComponent.listen` is `addEventListener` and `destroy` removes
  -- nothing, so re-listening per rebuild would leak a handler each time)
  liftEffect do
    when live $ listenNode node "MDCSlider:input" emit
    listenNode node "MDCSlider:change" emit
  pure
    { toUser: \q -> do
        comp <- scoped q
        setSliderValue comp q.current
        -- construction may have happened before styles applied; re-measure
        layoutComponent comp
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
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

-- | The Material **exposed dropdown**: one choice out of a list too long to
-- | lay out in the open. `floatingLabel` names the field and stays visible
-- | above the choice once one is made. For a handful of options worth
-- | comparing side by side, prefer `radioButton` or `segmentedButton`.
-- |
-- | Same contract as `radioButton`: nothing to show until the user picks,
-- | so say `# optional @"chosen" @"unchosen"` or `# required`; the options are part of the
-- | control, not of the model.
select :: forall @l a ri ro provided. IsSymbol l => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
select provided options = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (selectLeaf config options)

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

-- | The Material **segmented button**: two to five options joined in one
-- | control, all visible, one selected — a filter row, a view switch, a
-- | size. Compact where a radio group would be airy and a dropdown would
-- | hide the alternatives. Same picked/unpicked contract as `select`.
segmentedButton :: forall @l a ri ro. IsSymbol l => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
segmentedButton options = field @l $ "name" := reflectSymbol (Proxy @l) $ (segmentedLeaf options)

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

-- | The Material **filter chip**: a small tag the user switches on or off,
-- | showing a checkmark while on. Chips come in sets where any number may
-- | be active at once — dietary tags, categories, facets. Put them in a
-- | `chipSet`.
filterChip :: forall @l r rest provided. IsSymbol l => Cons l Boolean rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
filterChip provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (chipLeaf config.label)

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
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
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

-- | An **icon that stays pressed** — favourite, bookmark, mute, pin:
-- | `onIcon` is shown while it is on, `offIcon` while it is off, and
-- | `label` is what assistive technology announces. The compact form of a
-- | `toggleSwitch`, for list rows and toolbars.
iconToggle :: forall @l r rest provided. IsSymbol l => Cons l Boolean rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { onIcon :: String, offIcon :: String, label :: String } => { | provided } -> PUI Web { | r } { | r }
iconToggle provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (iconToggleLeaf config)

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
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
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

-- | The Material **tab bar**: the top-level sections of one screen, one
-- | open at a time. Arrow-key navigation and the sliding indicator come
-- | with it.
-- |
-- | Unlike `segmentedButton` a tab bar is never in a "nothing picked"
-- | state — some section is always open — which is what makes it the
-- | selector to build a sectioned editor around: the tab bar beside one
-- | `inCase @l` editor pane per section, each pane editing its own part of
-- | the model.
tabBar
  :: forall @l provided a r rest
   . IsSymbol l
  => Cons l a rest r
  => Eq a
  => ConvertOptionsWithDefaults OptIcon { icon :: Maybe String } { | provided } { value :: a, label :: String, icon :: Maybe String }
  => Array { | provided }
  -> PUI Web { | r } { | r }
tabBar options = field @l $ "name" := reflectSymbol (Proxy @l) $ (tabBarLeaf (convertOptionsWithDefaults OptIcon { icon: Nothing } <$> options))

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

-- | The **indeterminate progress bar**: work is under way and there is no
-- | telling how long — a request in flight, a file being processed. Shown
-- | while `busy`, gone when it isn't, so it is driven by the app's own
-- | notion of being busy rather than by a separate visibility flag.
indeterminateLinearProgress :: forall @l r. IsSymbol l => Cons l Boolean () r => PUI Web { | r } {}
indeterminateLinearProgress = wrap do
  _ <- unwrap $ div >>> "role" := "progressbar" >>> cl "mdc-linear-progress" >>> cl "mdc-linear-progress--indeterminate" >>> "aria-label" := "Progress Bar" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" >>> "aria-valuenow" := "0" $ linearProgressInnards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.linearProgress."MDCLinearProgress" node
  liftEffect $ close comp
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        if Record.get (Proxy @l) r then open comp else close comp
        -- display echo (like `text`): announce the `{}` per feed, so gated
        -- merges and whole-row editor stages keep flowing
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | The **determinate progress bar**: how far along something is, `value`
-- | running 0 to 1. As much a gauge as a progress indicator — a quiz's
-- | position, a budget's use, a quota — written as
-- | `linearProgress # projected fraction`, with the business function
-- | deciding what the fraction means.
linearProgress :: forall @l r. IsSymbol l => Cons l Number () r => PUI Web { | r } {}
linearProgress = wrap do
  _ <- unwrap $ div >>> "role" := "progressbar" >>> cl "mdc-linear-progress" >>> "aria-label" := "Progress" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" $ linearProgressInnards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.linearProgress."MDCLinearProgress" node
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setNumberProp "progress" comp (Record.get (Proxy @l) r)
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
    static (div >>> cl "mdc-linear-progress__buffer-bar")
    static (div >>> cl "mdc-linear-progress__buffer-dots")
  div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__primary-bar" $
    static (span >>> cl "mdc-linear-progress__bar-inner")
  div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__secondary-bar" $
    static (span >>> cl "mdc-linear-progress__bar-inner")

-- | The **spinner** — `indeterminateLinearProgress` in circular form, for
-- | inline and compact places (a button, a card corner) where a bar across
-- | the width would be too much.
indeterminateCircularProgress :: forall @l r. IsSymbol l => Cons l Boolean () r => PUI Web { | r } {}
indeterminateCircularProgress = wrap do
  _ <- unwrap $ div >>> cl "mdc-circular-progress" >>> cl "mdc-circular-progress--indeterminate" >>> "style" := "width: 48px; height: 48px;" >>> "role" := "progressbar" >>> "aria-label" := "Progress" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" $ staticHTML innards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.circularProgress."MDCCircularProgress" node
  liftEffect $ close comp
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        if Record.get (Proxy @l) r then open comp else close comp
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
-- prebuilt stylesheet — each step is also the matching HTML element, so
-- the document outline follows the visual hierarchy

-- | The largest display type — a splash figure or a hero number, at most
-- | once on a screen. Also the page's top-level heading.
headline1 :: Ocular (PUI Web)
headline1 w = h1 w # cl "mdc-typography--headline1"

-- | Display type one step down — still expressive, for a landing or
-- | marketing surface rather than a working screen.
headline2 :: Ocular (PUI Web)
headline2 w = h2 w # cl "mdc-typography--headline2"

-- | The smallest of the three display steps, where expressive type meets
-- | ordinary page titling.
headline3 :: Ocular (PUI Web)
headline3 w = h3 w # cl "mdc-typography--headline3"

-- | The workaday screen title — large enough to lead a page without
-- | shouting.
headline4 :: Ocular (PUI Web)
headline4 w = h4 w # cl "mdc-typography--headline4"

-- | A section heading within a screen.
headline5 :: Ocular (PUI Web)
headline5 w = h5 w # cl "mdc-typography--headline5"

-- | The smallest heading — a card title, a group label, a dialog title.
headline6 :: Ocular (PUI Web)
headline6 w = h6 w # cl "mdc-typography--headline6"

-- | A line of supporting text under a heading — a subtitle or a list row's
-- | primary line.
subtitle1 :: Ocular (PUI Web)
subtitle1 w = p w # cl "mdc-typography--subtitle1"

-- | Supporting text one step smaller than `subtitle1` — a list row's
-- | secondary line.
subtitle2 :: Ocular (PUI Web)
subtitle2 w = p w # cl "mdc-typography--subtitle2"

-- | Running text — the default for paragraphs a user reads.
body1 :: Ocular (PUI Web)
body1 w = p w # cl "mdc-typography--body1"

-- | Running text one step smaller than `body1`, for denser passages.
body2 :: Ocular (PUI Web)
body2 w = p w # cl "mdc-typography--body2"

-- | The smallest type: an annotation beside a control, a timestamp, a
-- | footnote — read only if looked for.
caption :: Ocular (PUI Web)
caption w = span w # cl "mdc-typography--caption"

-- | A short all-caps label above a group — a category or an eyebrow line.
overline :: Ocular (PUI Web)
overline w = span w # cl "mdc-typography--overline"

-- MD2 elevation levels as surface decorators (`mdc-elevation--z*` from the
-- prebuilt stylesheet; levels 10 and 20 pad like `PUI.Web.MDC3`'s
-- `elevation3`/`elevation5`)

-- | Lift the content onto a **barely raised surface** — the resting height
-- | of a card: enough shadow to separate it from the background.
elevation1 :: Ocular (PUI Web)
elevation1 w = div w # cl "mdc-elevation--z1"

-- | Lift the content onto a **clearly raised, padded panel** — a surface
-- | that reads as floating above the page, like a menu or a picked-up card.
elevation10 :: Ocular (PUI Web)
elevation10 w = div w # cl "mdc-elevation--z10" # "style" := "padding: 25px"

-- | Lift the content **highest**, onto a padded panel with a deep shadow —
-- | the topmost surface on the screen, for a modal-weight panel.
elevation20 :: Ocular (PUI Web)
elevation20 w = div w # cl "mdc-elevation--z20" # "style" := "padding: 25px"

-- | A **card**: a raised surface holding one subject's content and actions
-- | — an order, a product, a summary. Takes any content; put a row of
-- | buttons in `cardActions`.
-- |
-- | A plain ocular, with no config of its own: MD2 defines a card as a
-- | *surface* and gives it no title (every `mdc-card__*` class in the spec
-- | is optional structure, none of them a heading), so a card's heading is
-- | ordinary typography placed in its content — `headline6`, `subtitle1`,
-- | `caption` — exactly as MD2's own examples write it.
card :: Ocular (PUI Web)
card content =
  div >>> cl "mdc-card" >>> "style" := "padding: 10px; margin: 15px 0 15px 0; text-align: justify;" $ content

-- | The card's **action row**: the buttons belonging to the card, side by
-- | side at their natural width instead of stretched down its column.
cardActions :: Ocular (PUI Web)
cardActions = div >>> cl "mdc-card__actions"

-- | A **modal dialog** — dimmed backdrop, trapped focus, Esc to leave — for
-- | the decision that must be made before anything else continues.
-- |
-- | It opens the moment it is given something to show and closes when its
-- | content reports a decision. That *is* the interaction: show the dialog
-- | only for the case that calls for it, put the deciding buttons inside,
-- | and the decision both dismisses the dialog and travels on. So put only
-- | deciding controls at the end of its content — something that reports
-- | without the user deciding would dismiss the dialog as it opens.
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
    _ <- unwrap (static (div >>> cl "mdc-dialog__scrim"))
    pure result

-- | The **witness rung** of the assurance
-- | ladder, baked in as a component. `confirmed cfg content` is a
-- | fulfillment-gated pass-through `p { | row } { | row }` over a
-- | `{}`-output display, like every content slot in the family: feeding
-- | opens the modal and feeds the content (which reads a sub-row of the
-- | fed row, the family's subsumption); the flow is **withheld until the
-- | user confirms**, then the fed row is released — the release is the
-- | read receipt. Derived entirely from existing machinery,
-- | `simpleDialog cfg (shown content)`: the replay-on-confirm protocol
-- | over the instant rung — the ladder composes, witness rung = instant
-- | rung inside the modal. A dismiss without confirming releases nothing:
-- | a declined reading withholds, honestly.
confirmed :: forall read extra row. Union read extra row => { title :: String, confirm :: String } -> PUI Web { | read } {} -> PUI Web { | row } { | row }
confirmed cfg content = simpleDialog cfg (shown content)

-- | `dialog` with a **confirm button** built in — the confirmation step:
-- | show what is about to happen, and the button reports it. The content
-- | needs no button of its own; a content that only displays needs a
-- | gated display (`shown`) so there is something to confirm — `confirmed`
-- | is exactly that specialization.
-- | Not a full `Ocular`, deliberately: the confirm **replays** the
-- | content's last output, and replay is lawful over **records** only —
-- | an entity's last value may be re-said, a one-shot event may not (the
-- | `looped`/`observed` argument) — so the content's output is row-shaped.
simpleDialog :: forall i o. { title :: String, confirm :: String } -> PUI Web i { | o } -> PUI Web i { | o }
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
              static (div >>> cl "mdc-button__ripple")
              span >>> cl "mdc-button__label" $ staticText confirm
    _ <- unwrap (static (div >>> cl "mdc-dialog__scrim"))
    pure result

-- | The **snackbar**: a brief message at the bottom of the screen,
-- | dismissing itself, for something that has just happened and needs no
-- | reply ("Order placed"). It never interrupts — for something the user
-- | must acknowledge, use `banner` or a `dialog`.
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the snackbar is built — `snackbar # forCase @"booked" bookingLine` —
-- | and let the event carry the bare facts. One snackbar can serve several
-- | mutually exclusive outcomes with `forCases`.
snackbar :: PUI Web [ event :: String ] {}
snackbar = snackbarContainer $ text @"line" # projected eventText

-- opens on every message and auto-dismisses on the foundation's timeout;
-- closing on emission instead would race the open (the `text` leaf echoes
-- synchronously inside every `toUser`)
snackbarContainer :: Ocular (PUI Web)
snackbarContainer content =
  aside >>> cl "mdc-snackbar" >>> init (newComponent material.snackbar."MDCSnackbar") open mempty $
    div >>> cl "mdc-snackbar__surface" >>> "role" := "status" >>> "aria-relevant" := "additions" $
      div >>> cl "mdc-snackbar__label" >>> "aria-atomic" := "false" $
        content

-- | The **banner**: a prominent message at the top of the content that
-- | stays until the user dismisses it — for something they should actually
-- | read (an outage, a required action), where a `snackbar` would slip past
-- | unnoticed. Like the snackbar, the wording lives here and the event
-- | carries the bare facts.
-- |
-- | Material Design 2 only — MD3 dropped the banner, so `PUI.Web.MDC3` has
-- | none.
banner :: PUI Web [ event :: String ] {}
banner = bannerContainer $ text @"line" # projected eventText
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

-- | A **menu**: a labelled button that opens a short list of `menuItem`
-- | actions and closes again when one is picked. For actions; for choosing
-- | a value the model keeps, use `select`.
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

-- | The wrapping row a group of `filterChip`s sits in — chips are a set,
-- | never a lone control.
chipSet :: Ocular (PUI Web)
chipSet content =
  div >>> cl "mdc-chip-set" >>> cl "mdc-chip-set--filter" >>> "role" := "grid" $ content

-- | A **list**: rows of `listItem`s, with Material's row rhythm and keyboard
-- | navigation. For a list built from data, and clickable, use `listOf`.
list :: Ocular (PUI Web)
list content = wrap do
  w <- unwrap (ul >>> cl "mdc-deprecated-list" $ content)
  node <- gets _.sibling
  _ <- liftEffect $ newComponent material.list."MDCList" node
  liftEffect $ fixListTabIndexes node
  pure w

-- | One **list row**. Material's single-line row is 48 px tall and clips
-- | what does not fit — right for text, wrong for a row with a control in
-- | it. This row keeps 48 px as its floor but lets the content set the
-- | height and lays it out as one centred line, so a label beside a
-- | segmented button sits side by side unclipped, while a plain text row
-- | looks exactly as before.
listItem :: Ocular (PUI Web)
listItem content = li >>> cl "mdc-deprecated-list-item" >>> "style" := "height: auto; min-height: 48px;" $ wrap do
  _ <- unwrap (static (span >>> cl "mdc-deprecated-list-item__ripple"))
  unwrap (span >>> cl "mdc-deprecated-list-item__text" >>> "style" := "display: flex; align-items: center; gap: 16px; width: 100%; white-space: normal; overflow: visible;" $ content)

-- | A **list built from data**: one row per element of the collection the
-- | projection names, each row drawn by the given UI component. Rows matching
-- | `selected` take Material's selected styling — `listOf {}` selects
-- | nothing — and clicking a row reports *that row*, so the list is both
-- | how a collection is shown and how the user picks from it.
-- |
-- | Rows are updated in place as the collection changes rather than
-- | rebuilt, so the list can refresh under the user without flicker.
listOf
  :: forall provided i r o
   . ConvertOptionsWithDefaults OptSelected { selected :: { | r } -> Boolean } { | provided } { selected :: { | r } -> Boolean }
  -- the subsumption evidence the internal `clicked` needs at the exact
  -- element row (extra = ()); trivially discharged at every concrete call
  => Union r () r
  => { | provided }
  -> (i -> Array { | r })
  -> PUI Web { | r } o
  -> PUI Web i { | r }
listOf provided f item = wrap do
  w <- unwrap $ ul >>> cl "mdc-deprecated-list" >>> "style" := "overflow-y: auto;" $
    ( inRow ( clicked @r @() $ clWhen config.selected "mdc-deprecated-list-item--selected"
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

-- | A **data table**: values in rows and columns, where the column a value
-- | sits in is what says what it means. `columns` are the fixed headings
-- | and `label` is what assistive technology announces the table as; the
-- | body is `dataRow`s of `dataCell`s, usually one row per element of a
-- | collection.
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

-- | One row of a `dataTable` — a single record's line across the columns.
dataRow :: Ocular (PUI Web)
dataRow content = tr >>> cl "mdc-data-table__row" $ content

-- | One cell of a `dataRow`: the value under one column heading. Cells are
-- | written in the same order as the table's `columns`.
dataCell :: Ocular (PUI Web)
dataCell content = td >>> cl "mdc-data-table__cell" $ content

-- | An **image list**: pictures laid out in `columns` masonry columns, each
-- | one an `imageListItem` — a gallery, where the pictures are the content
-- | rather than an illustration of it.
imageList :: { columns :: Int } -> Ocular (PUI Web)
imageList config content =
  ul >>> cl "mdc-image-list" >>> cl "mdc-image-list--masonry" >>> "style" := ("column-count: " <> show config.columns <> "; column-gap: 16px; margin: 0;") $ content

-- | Material's **responsive layout grid**: the column grid a screen's
-- | regions are placed on, holding `layoutCell`s.
layoutGrid :: Ocular (PUI Web)
layoutGrid content = div >>> cl "mdc-layout-grid" $ div >>> cl "mdc-layout-grid__inner" $ content

-- | One region of a `layoutGrid`, `span` columns wide out of twelve — the
-- | grid reflows to fewer columns on narrow screens.
layoutCell :: { span :: Int } -> Ocular (PUI Web)
layoutCell config content = div >>> cl "mdc-layout-grid__cell" >>> cl ("mdc-layout-grid__cell--span-" <> show config.span) $ content

-- | The **top app bar**: the band carrying the screen's title, with the
-- | content laid out beneath it and clear of it.
topAppBar :: { title :: String } -> Ocular (PUI Web)
topAppBar config content = wrap do
  _ <- unwrap (staticHTML ("<header class=\"mdc-top-app-bar\"><div class=\"mdc-top-app-bar__row\"><section class=\"mdc-top-app-bar__section mdc-top-app-bar__section--align-start\"><span class=\"mdc-top-app-bar__title\">" <> config.title <> "</span></section></div></header>"))
  headerNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.topAppBar."MDCTopAppBar" headerNode
  unwrap (div >>> cl "mdc-top-app-bar--fixed-adjust" $ content)

-- | The permanent **navigation drawer**: a titled nav panel pinned beside
-- | the content. The nav is live, not a static menu — both sides see the
-- | same data and either can report, so a selectable nav (a `listOf` of
-- | sections) drives what is shown next to it.
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

-- | Attach a **tooltip** to a control: the short explanation that appears
-- | on hover or keyboard focus. For clarification only — never for
-- | information the user needs to complete the task, which belongs on the
-- | screen. Wrap a single control, and write it trailing so the control
-- | still reads first:
-- | `checkbox { ticked: {} } (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }`.
tooltip :: { text :: String } -> Ocular (PUI Web)
tooltip config content = wrap do
  tipId <- liftEffect uniqueId
  w <- unwrap ("aria-describedby" := tipId $ content)
  _ <- unwrap (staticHTML ("<div id=\"" <> tipId <> "\" class=\"mdc-tooltip\" role=\"tooltip\" aria-hidden=\"true\"><div class=\"mdc-tooltip__surface mdc-tooltip__surface-animation\">" <> config.text <> "</div></div>"))
  tipNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.tooltip."MDCTooltip" tipNode
  pure w

-- announcing statics (`{} → {}` chrome with a face)

-- | A **divider**: the hairline rule between list rows or card sections,
-- | for separating groups that belong to the same surface. Fixed
-- | decoration, carrying no data.
divider :: PUI Web {} {}
divider = staticHTML "<hr class=\"mdc-deprecated-list-divider\" style=\"width: 100%;\">"

-- | One picture in an `imageList`, with `label` shown as its caption and
-- | used as its alternative text.
imageListItem :: { src :: String, label :: String } -> PUI Web {} {}
imageListItem config = staticHTML $
  "<li class=\"mdc-image-list__item\" style=\"margin-bottom: 16px;\">"
    <> "<img class=\"mdc-image-list__image\" src=\"" <> config.src <> "\" alt=\"" <> config.label <> "\">"
    <> "<div class=\"mdc-image-list__supporting\"><span class=\"mdc-image-list__label\">" <> config.label <> "</span></div>"
    <> "</li>"

-- | One picture in an `imageList`, **fed through the channel**: the
-- | canonical `{ src, label }` row arrives as data, so a gallery is the
-- | retaining `foreach` over the pictures rather than a wholesale rebuild —
-- | `imagePane # foreach @"src" albumPhotos`, each item built once and its
-- | source and caption updated in place. `imageListItem`'s sibling, for the
-- | collection case; `imageListItem` stays the closure-known static.
imagePane :: PUI Web { src :: String, label :: String } {}
imagePane =
  li >>> cl "mdc-image-list__item" >>> "style" := "margin-bottom: 16px;" $ RecordToRecord.do
    imageFace
    div >>> cl "mdc-image-list__supporting" $ span >>> cl "mdc-image-list__label" $ text @"label"

imageFace :: PUI Web { src :: String, label :: String } {}
imageFace =
  img >>> cl "mdc-image-list__image" >>> attrWith "src" _.src >>> attrWith "alt" _.label $ blank

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
-- the same mechanism as `PUI.Web.MDC3`'s element properties
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
