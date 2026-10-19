-- | The **Material Design 2** vocabulary (https://m2.material.io) — the
-- | catalogue as far as MDC Web implements it, and the twin of
-- | `PUI.Web.MDC3`: where a concept survived into Material 3 it keeps the
-- | same name and the same signature there, so a screen changes design
-- | system by changing this one import.
-- |
-- | **The page must load** the prebuilt MDC Web stylesheet, the Material
-- | Icons font and Roboto. Unlike `PUI.Web.MDC3`, whose component styles
-- | ship in the bundle, Material 2's come from the page. The app enters
-- | through this vocabulary's `body`, which puts the MD2 typography
-- | baseline on the page body before mounting.
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
-- |     dismissed), `dialog`/`confirmed`/`acknowledged` (must be answered),
-- |     `linearProgress`, `indeterminateLinearProgress`,
-- |     `indeterminateCircularProgress`, `tooltip`
-- |   * **structure and surface** — `card`/`cardActions`, `list`/`listItem`,
-- |     `dataTable`/`dataRow`/`dataCell`, `imageList`/`imageListItem`,
-- |     `layoutGrid`/`layoutCell`, `topAppBar`, `drawer`, `chipSet`,
-- |     `divider`, the type scale (`headline1` … `overline`) and the
-- |     elevations (`elevation1`/`elevation10`/`elevation20`)
-- |   * **enter** — `body`, the page mounted and dressed for Material 2
-- |
-- | Anything in the Material 2 catalogue that MDC Web never implemented
-- | (backdrop, bottom app bar, bottom navigation, date pickers, navigation
-- | rail, sheets) is absent here too: this vocabulary offers only what the
-- | design system actually ships.
module PUI.Web.MDC2
  ( banner
  , body
  , body1
  , body2
  , button
  , button'
  , caption
  , card
  , cardActions
  , checkbox
  , chipSet
  , dataCell
  , dataRow
  , columnHeader
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
  , filledTextField'
  , filterChip
  , group
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
  , listOf'
  , listOfAt
  , listOfAt'
  , menu
  , menuItem
  , outlinedButton
  , outlinedTextField
  , outlinedTextField'
  , overline
  , radioButton
  , radio
  , radioButtonUnpicked
  , radioButtonOptional
  , segmentedButton
  , segment
  , segmentedButtonUnpicked
  , segmentedButtonOptional
  , select
  , option
  , selectUnpicked
  , selectOptional
  , confirmed
  , acknowledged
  , slider
  , sliderLive
  , snackbar
  , subtitle1
  , subtitle2
  , tabBar
  , tab
  , tab'
  , textButton
  , toggleSwitch
  , tooltip
  , topAppBar
  )
  where

import Data.Profunctor.Row.Structural (withStructuralOrd)
import Prelude hiding (div)

import Control.Monad.State (gets)
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import Data.Variant (inj, match, prj) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (fromHandles, handles, anyOf, isHole, Ocular, PUI, static, optionWiring, recordToRecordWiring, variantToRecordWiring, variantToVariantWiring, replayed, occurrencesOn, Gesture(..), statusOver, confirmedOver, acknowledgedOver, eventLeaf)
import PUI.Web.HTML (aside, div, h1, h2, h3, h4, h5, h6, i, img, label, li, p, span, table, tbody, td, th, thead, tr, ul)
import PUI.Web.HTML (body) as HTML
import PUI.Web (Members, clearedByRepress, eachMatching, hasClass, listenCapture, Node, OptCaption(..), OptLabelIcon(..), OptSelected(..), Web, addClass, attribute, attrWith, cl, clazz, clWhen, documentBody, el, element, escaped, getChecked, getValue, init, isFocused, listenNode, onInputDebounced, setAttribute, setChecked, staticHTML, staticText, text, statusText, uniqueId, separating, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Record as Record
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
--     of exactly one row shape:
--       `×→×` editors — `filledTextField @l`, `outlinedTextField @l` (the
--         MD2 variant pair), `filledTextArea @l`, `checkbox @l`,
--         `radioButton @l`, `toggleSwitch @l` (the MD2 Switch),
--         `slider @l`, `select @l` (the MD2 exposed dropdown),
--         `segmentedButton @l`, `tabBar @l` (the same-type selector — the
--         `looped`-ensemble citizen), `filterChip @l`, `iconToggle @l`;
--       `×→×` displays — the determinate `linearProgress`
--         (`{ | reads } → {}` over a read function);
--       `×→+` events — `button @l` (the contained/raised button — the
--         high-emphasis default; `outlinedButton`, `textButton` are the
--         other two MD2 emphasis levels), `fab @l`, `iconButton @l`,
--         `menuItem @l`;
--       `+→×` statuses — `snackbar`, `banner` (MD2 still has the banner;
--         MD3 dropped it, so `PUI.Web.MDC3` has no citizen for it), and
--         `indeterminateLinearProgress`/`indeterminateCircularProgress`
--         (`[ started, ended ] → { | r }` — the run's two occurrences, the shape
--         `PUI.action`'s progress slot dispatches).
--     No scalar or polymorphic component interfaces. Variant *editing* has
--     no `+→+` component citizens: the selection is a model field and each
--     case's payload fields sit beside it — a selection component followed
--     by editor panes, each `# inCaseAt @c @l` existing while the selection
--     sits at its case (order-form); `+→+` remains the dispatch shape
--     (`VariantToVariant.do` of action stages).
--   * **oculars** — shape-preserving decorators (`card`, `dialog`, `menu`,
--     `chipSet`, `list`/`listItem`, `dataTable`/`dataRow`/`dataCell`,
--     `imageList`, `layoutGrid`/`layoutCell`, `topAppBar`, `drawer`,
--     `tooltip`, the MD2 typography — `headline1` ... `overline` — and
--     elevations): no model of their own, any polarity.
--   * plus **announcing statics** (`{}`-output chrome with a face, at any row):
--     `divider`, `imageListItem`.
--
-- MD2 catalog entries with no MDC Web implementation (backdrop, bottom app
-- bar, bottom navigation, date pickers, navigation rail, sheets) are
-- absent here too.
--
-- Internally the live leaf of a compound is `focusField @l`-lifted — `focusField`
-- is the `Strong` field lens, so every editor is a whole-row citizen
-- `p { l | b } { l | b }`: fed the wide row it edits its field, and
-- each emission re-attaches the background the lens retains (runtime
-- completeness by construction; freshness rests on the enclosing loop's
-- re-broadcast) — and its chrome is hand-fused in the `Web` monad
-- (decoration as implementation technique — and a necessity: abstract
-- labels cannot flow through the merges' `Nub`, so a skolem-labeled
-- operand can't be merged); all-chrome groups (button content, progress
-- bars) have concrete rows and stay literal `RecordToRecord.do` merges of
-- announcing chrome (`staticText`/`staticHTML`/`static`, `{}`-output at any row).
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

-- | The **raised button** — Material's high-emphasis action and the default
-- | choice; `outlinedButton` and `textButton` are the same button at medium
-- | and low emphasis, for the secondary actions beside it.
-- |
-- | It reports on click, carrying the data it was showing, under the name
-- | the app gives the action: `button @"Book the flight"`, the case label
-- | verbatim as its caption, so the case *is* the copy. `button'`
-- | takes the face's options.
button
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } [ | v ]
button = buttonOf @l (Just "mdc-button--raised") {}

-- | `button` with options for its face: `icon: "add"` puts a Material
-- | Icons glyph before the label (`button' @"Place order" { icon:
-- | "local_cafe" }`), `label:` overrides the caption with copy the case
-- | cannot be.
button'
  :: forall @l provided r b v
   . IsSymbol l
  => Cons l { | r } b v
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | v ]
button' = buttonOf @l (Just "mdc-button--raised")

-- | `button` at medium emphasis — outlined, no fill: an important action
-- | that is not *the* action of the screen.
outlinedButton
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } [ | v ]
outlinedButton = buttonOf @l (Just "mdc-button--outlined") {}

-- | `button` at the lowest emphasis — label only, no fill or outline: the
-- | dismissive or tertiary action (Cancel, Learn more), and what belongs in
-- | dialogs and cards.
textButton
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } [ | v ]
textButton = buttonOf @l Nothing {}

buttonOf
  :: forall @l provided r b v
   . IsSymbol l
  => Cons l { | r } b v
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => Maybe String
  -> { | provided }
  -> PUI Web { | r } [ | v ]
buttonOf mModifier provided = eventLeaf @l $
  -- aria-label carries the caption VERBATIM: MD2's stylesheet uppercases the
  -- rendered label, and Chrome computes accessible names from rendered text,
  -- so without the stamp this button would announce and match as "COUNT"
  -- where its label says "Count" — presentation leaking into the name
  el "button" >>> cl "mdc-button" >>> modifier >>> "aria-label" := fromMaybe (reflectSymbol (Proxy @l)) config.label >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ Semigroupoid.do
    static (span >>> cl "mdc-button__ripple")
    static (span >>> cl "mdc-button__focus-ring")
    case config.icon of
      Just icon' -> i >>> cl "material-icons" >>> cl "mdc-button__icon" >>> "aria-hidden" := "true" $ staticText icon'
      Nothing -> identity
    case config.label of
      Just label' -> span >>> cl "mdc-button__label" $ staticText label'
      Nothing -> identity
  where
  config = convertOptionsWithDefaults OptLabelIcon { label: Just (reflectSymbol (Proxy @l)), icon: Nothing } provided :: { label :: Maybe String, icon :: Maybe String }
  modifier = case mModifier of
    Just m -> cl m
    Nothing -> identity

-- | The **floating action button**: the one action a screen is *for*, kept
-- | in view above the content. Reports on click carrying what it was
-- | showing, like `button`. The `icon` is required — a FAB is recognised by
-- | its glyph; the `label` — the extended FAB's words beside the glyph —
-- | defaults to the case label verbatim (`label: Nothing` gives
-- | the icon-only FAB, `label:` copy overrides).
fab
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => String
  -> PUI Web { | r } [ | v ]
fab icon = eventLeaf @l $
  -- the announced name is the CASE label even for the glyph-only face —
  -- the business action's word, never the icon ligature
  el "button" >>> cl "mdc-fab" >>> extended >>> "aria-label" := fromMaybe (reflectSymbol (Proxy @l)) config.label >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ Semigroupoid.do
    static (div >>> cl "mdc-fab__ripple")
    static (span >>> cl "mdc-fab__focus-ring")
    span >>> cl "mdc-fab__icon" >>> cl "material-icons" $ staticText icon
    case config.label of
      Just label' -> span >>> cl "mdc-fab__label" $ staticText label'
      Nothing -> identity
  where
  config = { label: Just (reflectSymbol (Proxy @l)) } :: { label :: Maybe String }
  extended = case config.label of
    Just _ -> cl "mdc-fab--extended"
    Nothing -> identity

-- | A compact **icon-only action**, for toolbars, list rows and card
-- | corners where a labelled button would not fit. `label` is not drawn —
-- | it is what assistive technology announces, defaulting to
-- | the case label verbatim. For an icon that stays pressed
-- | (favourite, mute), use `iconToggle` instead.
iconButton :: forall @l r b v. IsSymbol l => Cons l { | r } b v => String -> PUI Web { | r } [ | v ]
iconButton icon = eventLeaf @l $
  el "button" >>> cl "mdc-icon-button" >>> cl "material-icons" >>> "aria-label" := config.label >>> "data-mdc-ripple-is-unbounded" := "" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ Semigroupoid.do
    static (div >>> cl "mdc-icon-button__ripple")
    static (span >>> cl "mdc-icon-button__focus-ring")
    staticText icon
  where
  config = { label: reflectSymbol (Proxy @l) } :: { label :: String }

-- | One choice in a `menu`: reports the data it was showing when picked,
-- | and the menu closes itself. The line's text defaults to
-- | the case label verbatim (`label:` overrides with real copy).
menuItem :: forall @l r b v. IsSymbol l => Cons l { | r } b v => PUI Web { | r } [ | v ]
menuItem = eventLeaf @l $
  li >>> cl "mdc-deprecated-list-item" >>> "role" := "menuitem" >>> "tabindex" := "-1" $ Semigroupoid.do
    static (span >>> cl "mdc-deprecated-list-item__ripple")
    span >>> cl "mdc-deprecated-list-item__text" $ staticText config.label
  where
  config = { label: reflectSymbol (Proxy @l) } :: { label :: String }

-- TODO support input types: email, text, password, number, search, tel, url
-- | The **filled text field** — Material's default single-line input.
-- | `floatingLabel` names the field and rises above the text once there is
-- | any, so the label is never lost while the field is filled in.
-- |
-- | Shows the string it is given and reports each edit; typing is never
-- | interrupted by values arriving from elsewhere. A whole-row citizen:
-- | fed the wide row, it edits field `l` and carries the rest.
filledTextField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
filledTextField = let config = { floatingLabel: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "filled" Nothing config.floatingLabel "")

-- | `filledTextField` with a `hint` under it — a format or an example the
-- | label is not (`filledTextField' @"Start date" { hint: "DD.MM.YYYY" }`),
-- | so the label stays the field's name. Shown as MD2's persistent helper
-- | text, which describes the input.
filledTextField' :: forall @l r b. IsSymbol l => Cons l String b r => { hint :: String } -> PUI Web { | r } { | r }
filledTextField' { hint } = focusField @l $ "name" := reflectSymbol (Proxy @l) $ hinted $ (textFieldLeaf "filled" Nothing (reflectSymbol (Proxy @l)) hint)

-- | `filledTextField` in Material's outlined variant — a border instead of
-- | a fill. Same behaviour; pick one variant and keep to it across a form.
outlinedTextField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
outlinedTextField = let config = { floatingLabel: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "outlined" Nothing config.floatingLabel "")

-- | `outlinedTextField` with a `hint` under it, as `filledTextField'`.
outlinedTextField' :: forall @l r b. IsSymbol l => Cons l String b r => { hint :: String } -> PUI Web { | r } { | r }
outlinedTextField' { hint } = focusField @l $ "name" := reflectSymbol (Proxy @l) $ hinted $ (textFieldLeaf "outlined" Nothing (reflectSymbol (Proxy @l)) hint)

-- the field and its helper line are siblings in MD2's markup, so a hinted
-- field is wrapped to stay one element under the label's stamp
hinted :: Ocular (PUI Web)
hinted = el "div" >>> "style" := "display: inline-flex; flex-direction: column;"

-- | `filledTextField` that waits `ms` after the last keystroke before
-- | reporting — for a field that drives expensive work (a search, a
-- | recomputed preview) and should not fire once per character.
debouncedTextField :: forall @l r b. IsSymbol l => Cons l String b r => { ms :: Number } -> PUI Web { | r } { | r }
debouncedTextField settleTime = let config = { floatingLabel: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "filled" (if isHole settleTime then Nothing else Just settleTime.ms) config.floatingLabel "")

-- the raw MD2 text field — scalar, so private; the documented markup per
-- variant plus an `MDCTextField` foundation, values written through the
-- foundation's `value` property so the label float and line ripple stay
-- foundation-managed. Focus-guarded like `Web.input`: model updates never
-- clobber the field being typed in, but still echo so the channel stays
-- live. Debouncing sits at the DOM boundary (`Web.onInputDebounced`),
-- in front of the wire rather than on it, so the field stays loop-safe.
textFieldLeaf :: String -> Maybe Number -> String -> String -> PUI Web String String
textFieldLeaf variant mDebounce floatingLabel hint = fromHandles do
  labelId <- liftEffect uniqueId
  hintId <- liftEffect uniqueId
  inputNode <- element "label" do
    if variant == "outlined"
      then void $ handles $ span >>> cl "mdc-notched-outline" $ Semigroupoid.do
        static (span >>> cl "mdc-notched-outline__leading")
        span >>> cl "mdc-notched-outline__notch" $
          span >>> cl "mdc-floating-label" >>> "id" := labelId $ staticText floatingLabel
        static (span >>> cl "mdc-notched-outline__trailing")
      else do
        _ <- handles (static (span >>> cl "mdc-text-field__ripple"))
        void $ handles (span >>> cl "mdc-floating-label" >>> "id" := labelId $ staticText floatingLabel)
    element "input" (pure unit)
    clazz "mdc-text-field__input"
    attribute "type" "text"
    attribute "aria-labelledby" labelId
    when (hint /= "") $ attribute "aria-describedby" hintId
    node <- gets _.sibling
    when (variant == "filled") $
      void $ handles (static (span >>> cl "mdc-line-ripple"))
    pure node
  clazz "mdc-text-field"
  clazz ("mdc-text-field--" <> variant)
  fieldNode <- gets _.sibling
  comp <- liftEffect $ newComponent material.textField."MDCTextField" fieldNode
  when (hint /= "") $
    void $ handles (div >>> cl "mdc-text-field-helper-line" $ div >>> cl "mdc-text-field-helper-text" >>> cl "mdc-text-field-helper-text--persistent" >>> "id" := hintId $ staticText hint)
  liftEffect $ textFieldWiring comp inputNode mDebounce

-- the shared text-field/text-area wiring: write through the foundation's
-- `value` (focus-guarded, echoing), read the native input's events
textFieldWiring :: Component -> Node -> Maybe Number -> Effect { toUser :: String -> Effect Unit, fromUser :: (String -> Effect Unit) -> Effect Unit }
textFieldWiring comp inputNode mDebounce = recordToRecordWiring
  { show: setStringProp "value" comp
  , held: isFocused inputNode
  , reports: case mDebounce of
      Nothing -> listenNode inputNode "input"
      Just millis -> \report -> onInputDebounced inputNode millis (const report)
  , read: \_ -> getValue inputNode
  }

-- | The **multi-line text field**, sized in `rows` and `columns` of text (4 by 40 unless given) —
-- | a note, a description, a message. Otherwise `filledTextField`: the label
-- | floats (`floatingLabel` overrides it for real copy), shows a string,
-- | reports each edit, never interrupts typing.
filledTextArea :: forall @l r b provided. IsSymbol l => Cons l String b r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String, columns :: Int, rows :: Int } { | provided } { floatingLabel :: String, columns :: Int, rows :: Int } => { | provided } -> PUI Web { | r } { | r }
filledTextArea provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l), columns: 40, rows: 4 } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ fromHandles do
  labelId <- liftEffect uniqueId
  inputNode <- element "label" do
    _ <- handles (static (span >>> cl "mdc-text-field__ripple"))
    _ <- handles (span >>> cl "mdc-floating-label" >>> "id" := labelId $ staticText config.floatingLabel)
    node <- element "span" do
      element "textarea" (pure unit)
      clazz "mdc-text-field__input"
      attribute "rows" (show config.rows)
      attribute "cols" (show config.columns)
      attribute "aria-labelledby" labelId
      gets _.sibling
    clazz "mdc-text-field__resizer"
    _ <- handles (static (span >>> cl "mdc-line-ripple"))
    pure node
  clazz "mdc-text-field"
  clazz "mdc-text-field--filled"
  clazz "mdc-text-field--textarea"
  fieldNode <- gets _.sibling
  comp <- liftEffect $ newComponent material.textField."MDCTextField" fieldNode
  liftEffect $ textFieldWiring comp inputNode Nothing

-- | The Material **checkbox**, captioned beside it by its label verbatim,
-- | properly associated, so clicking the words toggles the box and the
-- | whole line is a comfortable target.
-- |
-- | The field is a **named two-case variant** the application spells:
-- | `checkbox @"Terms" @"accepted" @"declined"
-- | is ticked exactly while `"Terms"` sits at `accepted`; ticking reports
-- | `.accepted {}`, clearing reports `.declined {}` — so an optional part
-- | of the model *is* the box's state under its own names, with no second
-- | flag to keep in step and no `Maybe`. Use a checkbox for a fact the user
-- | states as part of a form; use `toggleSwitch` for a setting that takes
-- | effect at once.
checkbox :: forall @l @c @n r b v cr nr. IsSymbol l => IsSymbol c => IsSymbol n => Cons l [ | v ] b r => Cons c {} cr v => Cons n {} nr v => PUI Web { | r } { | r }
checkbox = focusField @l $ "name" := reflectSymbol (Proxy @l) $ fromHandles do
  checkboxId <- liftEffect uniqueId
  aRef <- liftEffect $ Ref.new {}
  parts <- element "div" do
    inputNode <- element "div" do
      element "input" (pure unit)
      clazz "mdc-checkbox__native-control"
      attribute "type" "checkbox"
      attribute "id" checkboxId
      node <- gets _.sibling
      _ <- handles $ div >>> cl "mdc-checkbox__background" $ Semigroupoid.do
        staticHTML """
          <svg class="mdc-checkbox__checkmark" viewBox="0 0 24 24">
            <path class="mdc-checkbox__checkmark-path" fill="none" d="M1.73,12.91 8.1,19.28 22.79,4.59"></path>
          </svg>""" -- Without raw HTML it doesn't work
        static (div >>> cl "mdc-checkbox__mixedmark")
      _ <- handles (static (div >>> cl "mdc-checkbox__ripple"))
      _ <- handles (static (div >>> cl "mdc-checkbox__focus-ring"))
      pure node
    clazz "mdc-checkbox"
    cbNode <- gets _.sibling
    cbComp <- liftEffect $ newComponent material.checkbox."MDCCheckbox" cbNode
    lbl <- handles (label >>> "for" := checkboxId $ staticText (reflectSymbol (Proxy @l)))
    pure { inputNode, cbComp, lbl }
  clazz "mdc-form-field"
  ffNode <- gets _.sibling
  ffComp <- liftEffect $ newComponent material.formField."MDCFormField" ffNode
  liftEffect $ setFormFieldInput ffComp parts.cbComp
  liftEffect $ recordToRecordWiring
    { show: \ma -> do
        parts.lbl.toUser {}
        case Variant.prj (Proxy @c) ma of
          Nothing -> setChecked parts.inputNode false
          Just newa -> do
            setChecked parts.inputNode true
            Ref.write newa aRef
    , held: pure false
    , reports: listenNode parts.inputNode "change"
    , read: \_ -> do
        checked <- getChecked parts.inputNode
        a <- Ref.read aRef
        pure (if checked then Variant.inj (Proxy @c) a else Variant.inj (Proxy @n) {})
    }

-- | The Material **radio group**: one choice among a handful, every option
-- | visible and comparable at a glance. Beyond about five options, or where
-- | the options don't deserve the space, use `select`.
-- |
-- | The options are leaves, one per case of the field, merged
-- | `VariantToVariant.do` in the order they are shown:
-- | `radioButton @"Roast" $ VariantToVariant.do radio @"Light"; radio @"Dark"`.
-- | The block closes the field's cases; the group lifts it onto field `l`,
-- | so the model always has one. Two siblings hold a variant instead:
-- | `radioButtonUnpicked @l @c` for a choice owed but not yet made,
-- | `radioButtonOptional @l @c @n` for one the user may leave unmade. Every
-- | one is an editor: every feed is answered with the row, every pick
-- | stored. The options — the value and the words shown for it — belong to
-- | the control, not to the model.
-- A selector group in three forms, over one clearing of its options:
-- every feed first shows nothing picked, then the one option owning the
-- fed case shows itself, so a case no option owns (an unpicked one) shows
-- nothing. The plain form's field holds the options' case; the `…Unpicked`
-- form's is a variant whose case `c` holds it, every other case unpicked;
-- the `…Optional` form adds a named none case `n` that the user's clearing
-- gesture stores.
radioButton :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioButton options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ radioGroupOf options

-- | `radioButton` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` holds the options' case, seeded at an unpicked case;
-- | nothing is checked until the user picks, and a pick cannot be taken back.
radioButtonUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioButtonUnpicked options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ radioGroupOf (focusCase @c options)

-- | `radioButton` for a choice the user may leave unmade: field `l` is a
-- | variant whose case `c` holds the options' case and case `n` none,
-- | seeded at `n`; pressing the checked radio again clears it, storing `n`.
radioButtonOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioButtonOptional options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ radioGroupOf (clearedByRepress @n radioMembers (focusCase @c options))

radioGroupOf :: forall a. PUI Web a a -> PUI Web a a
radioGroupOf options =
  div >>> "style" := "display: flex; flex-direction: column; align-items: flex-start;" >>> "role" := "radiogroup" $ fromHandles do
    container <- gets _.parent
    inner <- handles options
    liftEffect do
      groupName <- uniqueId
      eachMatching radioInput container \n -> setAttribute n "name" groupName
    pure
      { toUser: \a -> radioMembers.unpickAll container *> inner.toUser a
      , fromUser: inner.fromUser
      }

radioInput :: String
radioInput = "input.mdc-radio__native-control"

radioMembers :: Members
radioMembers =
  { selector: radioInput
  , isPicked: getChecked
  , unpickAll: \container -> eachMatching radioInput container \n -> setChecked n false
  , beforeClick: \container snapshot -> do
      listenNode container "pointerdown" snapshot
      listenNode container "keydown" snapshot
  }

-- | One option of a `radioButton` group: an MD2 radio captioned by its case.
-- | The native radio reports a change only when it becomes checked, so
-- | only a new pick leaves.
radio :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
radio = fromHandles do
  uid <- liftEffect uniqueId
  _ <- handles (staticHTML (markup uid))
  root <- gets _.sibling
  liftEffect do
    inputNode <- querySelectorIn root "input"
    radioNode <- querySelectorIn root ".mdc-radio"
    radioComp <- newComponent material.radio."MDCRadio" radioNode
    ffComp <- newComponent material.formField."MDCFormField" root
    setFormFieldInput ffComp radioComp
    optionWiring @c
      { show: setChecked inputNode true
      , reports: listenNode inputNode "change"
      , picked: getChecked inputNode
      }
  where
  markup uid =
    "<div class=\"mdc-form-field\">"
      <> "<div class=\"mdc-radio\">"
      <> "<input class=\"mdc-radio__native-control\" type=\"radio\" id=\"" <> uid <> "\">"
      <> "<div class=\"mdc-radio__background\"><div class=\"mdc-radio__outer-circle\"></div><div class=\"mdc-radio__inner-circle\"></div></div>"
      <> "<div class=\"mdc-radio__ripple\"></div>"
      <> "<div class=\"mdc-radio__focus-ring\"></div>"
      <> "</div>"
      <> "<label for=\"" <> uid <> "\">" <> escaped (reflectSymbol (Proxy @c)) <> "</label>"
      <> "</div>"

-- | The Material **switch**: a setting that takes effect the moment it is
-- | flipped — notifications on, dark mode on. (A `checkbox` states a fact
-- | to be submitted with the rest of a form; a switch acts immediately.)
toggleSwitch :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
toggleSwitch = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (switchLeaf config.label)

switchLeaf :: String -> PUI Web Boolean Boolean
switchLeaf lbl = div >>> "style" := "display: flex; align-items: center; gap: 8px;" $ fromHandles do
  switchId <- liftEffect uniqueId
  _ <- handles (staticHTML (switchMarkup switchId))
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.switchControl."MDCSwitch" node
  _ <- handles (staticHTML ("<label for=\"" <> switchId <> "\">" <> escaped lbl <> "</label>"))
  -- MDCSwitch toggles itself on click; read the post-toggle state
  liftEffect $ recordToRecordWiring
    { show: setBoolProp "selected" comp
    , held: pure false
    , reports: listenNode node "click"
    , read: \_ -> getBoolProp "selected" comp
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
-- | screen. `step` is `.discrete n`
-- | or `.continuous {}`, named like every other two-state field.
-- |
-- | It reports on **release**, once per adjustment, so one drag is one
-- | entry in the history — one undo step, one audit line. For a readout
-- | that follows the thumb, use `sliderLive`.
slider :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
slider = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (sliderLeaf false config.label)

-- | `slider` reporting continuously while the thumb moves — for a live
-- | readout or preview that has to follow the drag. Whatever it drives
-- | should be cheap to redo; a drag that should land in the history as one
-- | change needs the plain `slider`, or a `debounced` stage downstream.
sliderLive :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
sliderLive = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (sliderLeaf true config.label)

-- `MDCSlider`'s value API is method-based (`getValue`/`setValue`), the one
-- foundation here off the property-wiring convention; its bounds are
-- read from the DOM at construction only, so a bounds change rewrites the
-- input attributes and constructs a fresh foundation over the same markup
sliderLeaf :: Boolean -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
sliderLeaf live label = fromHandles do
  _ <- handles (staticHTML markup)
  node <- gets _.sibling
  stateRef <- liftEffect $ Ref.new Nothing
  let
    scoped q = do
      st <- Ref.read stateRef
      case st of
        Just s | s.quantity.min == q.min, s.quantity.max == q.max, s.quantity.step == q.step -> do
          Ref.write (Just s { quantity = q }) stateRef
          pure s.comp
        _ -> do
          for_ st \s -> destroyComponent s.comp
          Variant.match
            { discrete: \step -> configureMdcSlider node q.min q.max step true q.current
            , continuous: \_ -> configureMdcSlider node q.min q.max 0.0 false q.current
            } q.step
          comp <- newComponent material.slider."MDCSlider" node
          Ref.write (Just { comp, quantity: q }) stateRef
          pure comp
  -- the foundation's events surface on the root node, and the node outlives
  -- every rebuild — so listen once, not per rebuild inside `scoped`
  -- (`MDCComponent.listen` is `addEventListener` and `destroy` removes
  -- nothing, so re-listening per rebuild would leak a handler each time);
  -- the read goes through the *current* foundation in the state ref, since
  -- a bounds change replaces it
  liftEffect $ recordToRecordWiring
    { show: \q -> do
        comp <- scoped q
        setSliderValue comp q.current
        -- construction may have happened before styles applied; re-measure
        layoutComponent comp
    , held: pure false
    , reports: \report -> do
        when live $ listenNode node "MDCSlider:input" report
        listenNode node "MDCSlider:change" report
    , read: \q -> do
        st <- Ref.read stateRef
        case st of
          Just st' -> getSliderValue st'.comp <#> \v -> q { current = v }
          Nothing -> pure q
    }
  where
  markup =
    "<div class=\"mdc-slider\" style=\"min-width: 200px;\">"
      <> "<input class=\"mdc-slider__input\" type=\"range\" min=\"0\" max=\"100\" value=\"0\""
      <> " aria-label=\"" <> escaped label <> "\">"
      <> "<div class=\"mdc-slider__track\">"
      <> "<div class=\"mdc-slider__track--inactive\"></div>"
      <> "<div class=\"mdc-slider__track--active\"><div class=\"mdc-slider__track--active_fill\"></div></div>"
      <> "</div>"
      <> "<div class=\"mdc-slider__thumb\">"
      <> "<div class=\"mdc-slider__thumb-knob\"></div>"
      <> "</div>"
      <> "</div>"

-- | The Material **exposed dropdown**: one choice out of a list too long to
-- | lay out in the open. The field's label names it and stays visible
-- | above the choice once one is made. For a handful of options worth
-- | comparing side by side, prefer `radioButton` or `segmentedButton`.
-- |
-- | Same selection contract as `radioButton`, with the same two siblings
-- | (`selectUnpicked`, `selectOptional`); its options are `option` leaves.
select :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
select options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ selectGroupOf @l false options

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` holds the options' case, seeded at an unpicked case;
-- | nothing is picked until the user picks, and a pick cannot be taken back.
selectUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectUnpicked options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ selectGroupOf @l false (focusCase @c options)

-- | `select` for a choice the user may leave unmade: field `l` is a
-- | variant whose case `c` holds the options' case and case `n` none,
-- | seeded at `n`; an empty first option, which picked stores `n` again.
selectOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectOptional options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ selectGroupOf @l true (blankEmits @n (focusCase @c options))

-- MDCSelect reads its items at construction, so the group builds the
-- documented markup with the options' items in its list first, then the
-- foundation over it, stashed on the root for the options to reach. Every
-- programmatic pick is quiet (no MDCSelect:change), so the change event
-- is the user's alone.
selectGroupOf :: forall @l a. IsSymbol l => Boolean -> PUI Web a a -> PUI Web a a
selectGroupOf blank options = div >>> cl "mdc-select" >>> cl "mdc-select--filled" >>> "style" := "min-width: 200px;" $ fromHandles do
  root <- gets _.parent
  labelId <- liftEffect uniqueId
  textId <- liftEffect uniqueId
  _ <- handles (staticHTML (anchorMarkup labelId textId))
  inner <- element "div" do
    inner <- element "ul" do
      when blank $ void $ handles (staticHTML "<li class=\"mdc-deprecated-list-item\" data-value=\"\" role=\"option\" aria-selected=\"false\"><span class=\"mdc-deprecated-list-item__ripple\"></span></li>")
      handles options
    attribute "class" "mdc-deprecated-list"
    attribute "role" "listbox"
    pure inner
  attribute "class" "mdc-select__menu mdc-menu mdc-menu-surface mdc-menu-surface--fullwidth"
  comp <- liftEffect $ newComponent material.select."MDCSelect" root
  liftEffect $ stashComponent root comp
  pure
    { toUser: \a -> selectQuietly comp "" *> inner.toUser a
    , fromUser: inner.fromUser
    }
  where
  anchorMarkup labelId textId =
    "<div class=\"mdc-select__anchor\" role=\"button\" aria-haspopup=\"listbox\" aria-expanded=\"false\" aria-labelledby=\"" <> labelId <> " " <> textId <> "\">"
      <> "<span class=\"mdc-select__ripple\"></span>"
      <> "<span class=\"mdc-floating-label\" id=\"" <> labelId <> "\">" <> escaped (reflectSymbol (Proxy @l)) <> "</span> "
      <> "<span class=\"mdc-select__selected-text-container\"><span class=\"mdc-select__selected-text\" id=\"" <> textId <> "\"></span></span>"
      <> "<span class=\"mdc-select__dropdown-icon\">"
      <> "<svg class=\"mdc-select__dropdown-icon-graphic\" viewBox=\"7 10 10 5\" focusable=\"false\">"
      <> "<polygon class=\"mdc-select__dropdown-icon-inactive\" stroke=\"none\" fill-rule=\"evenodd\" points=\"7 10 12 15 17 10\"></polygon>"
      <> "<polygon class=\"mdc-select__dropdown-icon-active\" stroke=\"none\" fill-rule=\"evenodd\" points=\"7 15 12 10 17 15\"></polygon>"
      <> "</svg>"
      <> "</span>"
      <> "<span class=\"mdc-line-ripple\"></span>"
      <> "</div>"

-- The clearing of an optional select: picking the empty option emits `n`.
blankEmits :: forall @n b v. IsSymbol n => Cons n {} b v => PUI Web [ | v ] [ | v ] -> PUI Web [ | v ] [ | v ]
blankEmits options = fromHandles do
  items <- gets _.parent
  inner <- handles options
  pure
    { toUser: inner.toUser
    , fromUser: \prop -> do
        inner.fromUser prop
        root <- closestOf ".mdc-select" items
        listenNode root "MDCSelect:change" do
          value <- componentWithin ".mdc-select" root >>= getStringProp "value"
          when (value == "") $ prop (Variant.inj (Proxy @n) {})
    }

-- | One option of a `select`: a list item headed by its case, its value
-- | the case too.
option :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
option = fromHandles do
  _ <- handles (staticHTML markup)
  item <- gets _.sibling
  liftEffect $ optionWiring @c
    { show: componentWithin ".mdc-select" item >>= \comp -> selectQuietly comp tag
    , reports: \k -> closestOf ".mdc-select" item >>= \root -> listenNode root "MDCSelect:change" k
    , picked: componentWithin ".mdc-select" item >>= getStringProp "value" <#> (_ == tag)
    }
  where
  tag = reflectSymbol (Proxy @c)
  markup =
    "<li class=\"mdc-deprecated-list-item\" data-value=\"" <> escaped tag <> "\" role=\"option\" aria-selected=\"false\">"
      <> "<span class=\"mdc-deprecated-list-item__ripple\"></span>"
      <> "<span class=\"mdc-deprecated-list-item__text\">" <> escaped tag <> "</span>"
      <> "</li>"

-- | The Material **segmented button**: two to five options joined in one
-- | control, all visible, one selected — a filter row, a view switch, a
-- | size. Compact where a radio group would be airy and a dropdown would
-- | hide the alternatives. Same selection contract as `select`; its
-- | options are `segment` leaves.
segmentedButton :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
segmentedButton options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ segmentGroupOf options

-- | `segmentedButton` for a choice owed but not yet made: field `l` is a
-- | variant whose case `c` holds the options' case, seeded at an unpicked
-- | case; nothing is selected until the user picks, and a pick cannot be
-- | taken back.
segmentedButtonUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
segmentedButtonUnpicked options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ segmentGroupOf (focusCase @c options)

-- | `segmentedButton` for a choice the user may leave unmade: field `l` is
-- | a variant whose case `c` holds the options' case and case `n` none,
-- | seeded at `n`; clicking the selected segment again clears it, storing
-- | `n` again.
segmentedButtonOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
segmentedButtonOptional options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ segmentGroupOf (clearedByRepress @n segmentMembers (focusCase @c options))

segmentGroupOf :: forall a. PUI Web a a -> PUI Web a a
segmentGroupOf options = div >>> cl "mdc-segmented-button" >>> cl "mdc-segmented-button--single-select" >>> "role" := "radiogroup" $ fromHandles do
  container <- gets _.parent
  inner <- handles options
  pure
    { toUser: \a -> segmentMembers.unpickAll container *> inner.toUser a
    , fromUser: inner.fromUser
    }

-- | One segment of a `segmentedButton`, captioned by its case. Pressing
-- | the segment already selected picks nothing new.
segment :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
segment = fromHandles do
  container <- gets _.parent
  -- aria-label = the case verbatim: MD2 uppercases the rendered segment
  -- text, and the computed name follows rendering without the stamp
  _ <- handles (staticHTML ("<button class=\"mdc-segmented-button__segment\" role=\"radio\" aria-checked=\"false\" aria-label=\"" <> escaped tag <> "\"><div class=\"mdc-segmented-button__ripple\"></div><div class=\"mdc-segmented-button__label\">" <> escaped tag <> "</div></button>"))
  node <- gets _.sibling
  let
    selectSelf = do
      segmentMembers.unpickAll container
      setClassIf node segmentSelected true
      setAttribute node "aria-checked" "true"
  liftEffect $ optionWiring @c
    { show: selectSelf
    , reports: \k -> listenNode node "click" do
        already <- hasClass segmentSelected node
        unless already (selectSelf *> k)
    , picked: pure true
    }
  where
  tag = reflectSymbol (Proxy @c)

segmentSelected :: String
segmentSelected = "mdc-segmented-button__segment--selected"

segmentMembers :: Members
segmentMembers =
  { selector: ".mdc-segmented-button__segment"
  , isPicked: hasClass segmentSelected
  , unpickAll: \container -> eachMatching ".mdc-segmented-button__segment" container \n -> do
      setClassIf n segmentSelected false
      setAttribute n "aria-checked" "false"
  , beforeClick: \container snapshot -> listenCapture container "click" snapshot
  }

-- | The Material **filter chip**: a small tag the user switches on or off,
-- | showing a checkmark while on. Chips come in sets where any number may
-- | be active at once — dietary tags, categories, facets. Put them in a
-- | `chipSet`.
filterChip :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
filterChip = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (chipLeaf config.label)

-- deprecated `mdc-chip` markup on purpose: the prebuilt v14 CSS bundle has
-- no `mdc-evolution-chip` rules at all
chipLeaf :: String -> PUI Web Boolean Boolean
chipLeaf lbl = fromHandles do
  _ <- handles (staticHTML markup)
  node <- gets _.sibling
  actionNode <- liftEffect $ querySelectorIn node ".mdc-chip__primary-action"
  stateRef <- liftEffect $ Ref.new false
  let render b = do
        Ref.write b stateRef
        setClassIf node "mdc-chip--selected" b
        setAttribute actionNode "aria-checked" (if b then "true" else "false")
  -- the deprecated chip toggles nothing itself: a click flips the kept state
  liftEffect $ recordToRecordWiring
    { show: render
    , held: pure false
    , reports: listenNode node "click"
    , read: \_ -> do
        b <- not <$> Ref.read stateRef
        render b
        pure b
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
      <> "<span class=\"mdc-chip__text\">" <> escaped lbl <> "</span>"
      <> "</span>"
      <> "</span>"
      <> "</div>"

-- | An **icon that stays pressed** — favourite, bookmark, mute, pin:
-- | `onIcon` is shown while it is on, `offIcon` while it is off, and
-- | `label` is what assistive technology announces. The compact form of a
-- | `toggleSwitch`, for list rows and toolbars.
iconToggle :: forall @l r b provided. IsSymbol l => Cons l Boolean b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { onIcon :: String, offIcon :: String, label :: String } => { | provided } -> PUI Web { | r } { | r }
iconToggle provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (iconToggleLeaf config)

iconToggleLeaf :: { onIcon :: String, offIcon :: String, label :: String } -> PUI Web Boolean Boolean
iconToggleLeaf config = fromHandles do
  _ <- handles (staticHTML markup)
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.iconButton."MDCIconButtonToggle" node
  liftEffect $ recordToRecordWiring
    { show: setBoolProp "on" comp
    , held: pure false
    , reports: listen comp "MDCIconButtonToggle:change"
    , read: \_ -> getBoolProp "on" comp
    }
  where
  markup =
    "<button class=\"mdc-icon-button\" aria-label=\"" <> escaped config.label <> "\" aria-pressed=\"false\">"
      <> "<div class=\"mdc-icon-button__ripple\"></div>"
      <> "<span class=\"mdc-icon-button__focus-ring\"></span>"
      <> "<i class=\"material-icons mdc-icon-button__icon mdc-icon-button__icon--on\">" <> escaped config.onIcon <> "</i>"
      <> "<i class=\"material-icons mdc-icon-button__icon\">" <> escaped config.offIcon <> "</i>"
      <> "</button>"

-- | The Material **tab bar**: the top-level sections of one screen, one
-- | open at a time. Arrow-key navigation and the sliding indicator come
-- | with it. Its options are `tab` leaves (`tab'` with an icon).
-- |
-- | Unlike `segmentedButton` a tab bar is never in a "nothing picked"
-- | state — some section is always open — which is what makes it the
-- | selector to build a sectioned editor around: the tab bar beside one
-- | `inCase @l` editor pane per section, each pane editing its own part of
-- | the model.
-- MDCTabBar reads its tabs at construction, so the group builds the
-- documented markup with the tabs inside its scroll content first, then
-- the foundation over it, stashed on the root for the tabs to reach.
tabBar :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
tabBar options = focusField @l $ "name" := reflectSymbol (Proxy @l) $ div >>> cl "mdc-tab-bar" >>> "role" := "tablist" $ fromHandles do
  root <- gets _.parent
  inner <- element "div" do
    inner <- element "div" do
      inner <- element "div" (handles options)
      clazz "mdc-tab-scroller__scroll-content"
      pure inner
    clazz "mdc-tab-scroller__scroll-area"
    pure inner
  clazz "mdc-tab-scroller"
  comp <- liftEffect $ newComponent material.tabBar."MDCTabBar" root
  liftEffect $ stashComponent root comp
  pure inner

-- | One tab of a `tabBar`, captioned by its case.
tab :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
tab = tabWith @c Nothing

-- | `tab` with a Material icon drawn above its caption.
tab' :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => { icon :: String } -> PUI Web [ | v ] [ | o ]
tab' { icon } = tabWith @c (Just icon)

-- A programmatic activation fires MDCTabBar:activated too; the wiring's
-- busy guard drops it.
tabWith :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => Maybe String -> PUI Web [ | v ] [ | o ]
tabWith icon = fromHandles do
  _ <- handles (staticHTML markup)
  node <- gets _.sibling
  liftEffect $ optionWiring @c
    { show: componentWithin ".mdc-tab-bar" node >>= \comp -> activateTabNode comp node
    , reports: \k -> closestOf ".mdc-tab-bar" node >>= \bar -> listenNode bar "MDCTabBar:activated" k
    , picked: hasClass "mdc-tab--active" node
    }
  where
  tag = reflectSymbol (Proxy @c)
  markup =
    -- aria-label = the case verbatim: MD2 uppercases the rendered tab
    -- text, and the computed name follows rendering without the stamp
    "<button class=\"mdc-tab\" role=\"tab\" aria-selected=\"false\" tabindex=\"-1\" aria-label=\"" <> escaped tag <> "\">"
      <> "<span class=\"mdc-tab__content\">"
      <> maybe "" (\i -> "<span class=\"mdc-tab__icon material-icons\" aria-hidden=\"true\">" <> escaped i <> "</span>") icon
      <> "<span class=\"mdc-tab__text-label\">" <> escaped tag <> "</span>"
      <> "</span>"
      <> "<span class=\"mdc-tab-indicator\"><span class=\"mdc-tab-indicator__content mdc-tab-indicator__content--underline\"></span></span>"
      <> "<span class=\"mdc-tab__ripple\"></span>"
      <> "</button>"

-- | The **indeterminate progress bar**: work is under way and there is no
-- | telling how long — a request in flight, a file being processed. A
-- | **status**, not a display: it is fed the run's two occurrences,
-- | `started` and `ended`, and shows between them — exactly what
-- | `PUI.action`'s slot dispatches (`indeterminateLinearProgress # action
-- | submit`). No model owns a "busy" field, so nothing here is state, and
-- | it carries no label: it names no field and no case, and a caption is
-- | copy, which is a function, not a label.
indeterminateLinearProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateLinearProgress = fromHandles do
  _ <- handles $ div >>> "role" := "progressbar" >>> cl "mdc-linear-progress" >>> cl "mdc-linear-progress--indeterminate" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" >>> "aria-valuenow" := "0" $ linearProgressInnards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.linearProgress."MDCLinearProgress" node
  liftEffect $ close comp
  pure $ variantToRecordWiring $ Variant.match { started: \_ -> open comp, ended: \_ -> close comp }

-- | The **determinate progress bar**: how far along something is, `value`
-- | running 0 to 1. As much a gauge as a progress indicator — a quiz's
-- | position, a budget's use, a quota — written as
-- | `linearProgress progressFraction`: the value is a **read
-- | function** like every display's, since a fraction is derived (a ratio
-- | of source fields), not state.
linearProgress
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
linearProgress f = fromHandles do
  _ <- handles $ div >>> "role" := "progressbar" >>> cl "mdc-linear-progress" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" $ linearProgressInnards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.linearProgress."MDCLinearProgress" node
  liftEffect $ recordToRecordWiring { show: \r -> setNumberProp "progress" comp (f r), held: pure false, reports: \_ -> pure unit, read: pure }

-- the documented buffer/bar innards shared by both linear variants
linearProgressInnards :: PUI Web {} {}
linearProgressInnards = Semigroupoid.do
  div >>> cl "mdc-linear-progress__buffer" $ Semigroupoid.do
    static (div >>> cl "mdc-linear-progress__buffer-bar")
    static (div >>> cl "mdc-linear-progress__buffer-dots")
  div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__primary-bar" $
    static (span >>> cl "mdc-linear-progress__bar-inner")
  div >>> cl "mdc-linear-progress__bar" >>> cl "mdc-linear-progress__secondary-bar" $
    static (span >>> cl "mdc-linear-progress__bar-inner")

-- | The **spinner** — `indeterminateLinearProgress` in circular form, for
-- | inline and compact places (a button, a card corner) where a bar across
-- | the width would be too much.
indeterminateCircularProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateCircularProgress = fromHandles do
  _ <- handles $ div >>> cl "mdc-circular-progress" >>> cl "mdc-circular-progress--indeterminate" >>> "style" := "width: 48px; height: 48px;" >>> "role" := "progressbar" >>> "aria-valuemin" := "0" >>> "aria-valuemax" := "1" $ staticHTML innards
  node <- gets _.sibling
  comp <- liftEffect $ newComponent material.circularProgress."MDCCircularProgress" node
  liftEffect $ close comp
  pure $ variantToRecordWiring $ Variant.match { started: \_ -> open comp, ended: \_ -> close comp }
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
-- | `caption` — exactly as MD2's own examples write it. A card whose
-- | content is one model sub-record is not chrome but the labelled group,
-- | `group @l`.
card :: Ocular (PUI Web)
card content =
  div >>> cl "mdc-card" >>> "style" := "padding: 10px; margin: 15px 0 15px 0; text-align: justify;" $ content

-- | The card's **action row**: the buttons belonging to the card, side by
-- | side at their natural width instead of stretched down its column.
cardActions :: Ocular (PUI Web)
cardActions = div >>> cl "mdc-card__actions"

-- | A **labelled model group** — the card as visual wrapper *and*
-- | presentation-model wrapper, one word. `group @l w` renders `w` on a
-- | `card` headed by the label verbatim (`subtitle1`) and nests `w`'s model
-- | under field `l`, stamping the label as the accessible group name
-- | (`role="group"`, `aria-labelledby` the heading) — so the group's view
-- | line anchors at its field like every labelled leaf, and the words
-- | appear once. It draws the surface, so it **leads its lines like any
-- | container** (`group @"Customer" $ …`), never trailing as a `#` chain.
-- | Without it a labelled group is three hand-aligned spellings — the
-- | `card`, a `staticText` heading, a trailing `# focusField @l` — free to
-- | drift apart.
-- |
-- | Derived, not primitive:
-- |
-- | ```
-- | group @l w = card (heading >>> focusField @l w)      -- + the a11y stamp
-- | ```
-- |
-- | well-defined because chrome commutes with the strengths (`Ocular`'s
-- | admission law): `card w # focusField @l = card (w # focusField @l)`, so fusing
-- | surface and lens loses nothing. The focus is any type `focusField @l`
-- | accepts — an editor ensemble (`group @"Customer" $ …`, order-form's
-- | `group @"Fulfillment"` with its case panes), a collection's array
-- | (potluck's `group @"Guests"` over `acted`, reorder's
-- | `group @"Setlist"` over `acted`). Fusion is earned by the
-- | leaf criterion — the label does work a trailing `# focusField @l` cannot
-- | (heading copy, accessible name) — so a card grouping no model (a
-- | display card, a button row) stays the blind `card`.
group
  :: forall @l f f' b s t
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b t
  => PUI Web f f'
  -> PUI Web { | s } { | t }
group w = fromHandles do
  headingId <- liftEffect uniqueId
  handles $ "role" := "group" $ "aria-labelledby" := headingId $ card $
    ("id" := headingId $ subtitle1 $ staticText (reflectSymbol (Proxy @l))) >>> focusField @l w

-- | A **modal dialog** — dimmed backdrop, trapped focus, Esc to leave — for
-- | the decision that must be made before anything else continues.
-- |
-- | It opens the moment it is given something to show and closes when its
-- | content reports a decision. That *is* the interaction: show the dialog
-- | only for the case that calls for it, put the deciding buttons inside,
-- | and the decision both dismisses the dialog and travels on. So put only
-- | deciding controls at the end of its content — something that reports
-- | without the user deciding would dismiss the dialog as it opens.
dialog :: String -> Ocular (PUI Web)
dialog title content = fromHandles do
  titleId <- liftEffect uniqueId
  contentId <- liftEffect uniqueId
  handles $ div >>> cl "mdc-dialog" >>> init (newComponent material.dialog."MDCDialog") open close $ fromHandles do
    result <- handles $
      div >>> cl "mdc-dialog__container" $
        div >>> cl "mdc-dialog__surface" >>> "role" := "alertdialog" >>> "aria-modal" := "true" >>> "aria-labelledby" := titleId >>> "aria-describedby" := contentId $ fromHandles do
          _ <- handles (h2 >>> cl "mdc-dialog__title" >>> "id" := titleId $ staticText title)
          handles (div >>> cl "mdc-dialog__content" >>> "id" := contentId $ content)
    _ <- handles (static (div >>> cl "mdc-dialog__scrim"))
    pure result

-- | The **witness rung** of the assurance ladder, baked in as a
-- | component: `dialog` with a **confirm button** built in, the `+→+`
-- | dual of the lens editor. As an editor is `focusField @l` over a leaf
-- | that shows a field and lets the user change it, this is `focusCase
-- | @l` over a modal that shows an event's payload and lets the user
-- | decide its fate: opened by case `l`, its content — a stage over the
-- | payload, a display or editors — shown the row the button that opened
-- | it replayed, the flow **withheld until the user confirms** with the
-- | button the case **captions**, verbatim, as an emitter draws its
-- | label, when the case leaves again carrying the row the content last
-- | answered, to be folded in the fold block like any event (`confirmed
-- | @"Refund a customer" $ body1 $ text refundLine` among the stages,
-- | `fold @"Refund a customer" refundStandard` among the folds — cashbox;
-- | inbox's `"Delete message"`). The content states what is at stake, so
-- | the dialog has no title of its own; the case names it. Cancel (or
-- | Escape, or the scrim) consumes the case: a declined decision reaches
-- | no fold. Every other case passes through
-- | untouched, so the dialog names only its own case and the fold block
-- | stays typed at exactly what folds. The content needs no button of
-- | its own: a display releases the row it shows, so there is something
-- | to confirm; editors inside amend the payload before it goes on. Not
-- | an `Ocular`, deliberately: the confirm **replays** the row last
-- | shown, and replay is lawful over **records** only — an entity's last
-- | value may be re-said, a one-shot event may not (the `looped`
-- | argument) — so a dialog is opened by an event and shows a row. For a
-- | reading that changes nothing, `acknowledged`.
confirmed
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
confirmed = confirmedOver @l modal

-- | The **dismissed rung**: `dialog` with a **dismiss button** built in,
-- | for the outcome the user must read and can only close — a rejection
-- | and its reason, a notice. `onCase @l` over the modal: opened by case
-- | `l`, which it tells as its title, its content shown the payload for
-- | as long as the user wants it, and the case **consumed** — the row
-- | less `l` leaves, so what follows is typed at the cases that go on
-- | (`acknowledged @"About this dashboard" $ body1 $ text aboutLine`,
-- | weather, between the emitters and the fold). The dismiss is captioned
-- | "OK" and decides nothing — closing it, Escape and the scrim all
-- | consume the case — so, like `confirmed`'s Cancel, it is chrome and
-- | takes no caption. The persistent member of the status family: a
-- | `snackbar` tells its case and is gone, so it carries no copy; a
-- | message the user closes is not ephemeral, so its content may read
-- | the payload. Every other case
-- | passes through untouched.
acknowledged
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | b ]
acknowledged = acknowledgedOver @l modal

-- the modal both dialog leaves are built over: `variantToVariantWiring`
-- over the MDCDialog foundation's markup, an optional title, the content
-- stage, an optional cancel button and the deciding button, named by the
-- title or else by the decision; Escape and the scrim close the
-- foundation by itself, releasing nothing
modal
  :: forall r
   . { title :: Maybe String, decide :: String, cancel :: Maybe String }
  -> PUI Web { | r } { | r }
  -> PUI Web { | r } { | r }
modal { title, decide, cancel } content = fromHandles do
  contentId <- liftEffect uniqueId
  decideRef <- liftEffect $ Ref.new Nothing
  cancelRef <- liftEffect $ Ref.new Nothing
  shown <- handles $ div >>> cl "mdc-dialog" $ fromHandles do
    w <- handles $
      div >>> cl "mdc-dialog__container" $
        div >>> cl "mdc-dialog__surface" >>> "role" := "alertdialog" >>> "aria-modal" := "true" >>> "aria-label" := fromMaybe decide title >>> "aria-describedby" := contentId $ fromHandles do
          for_ title \t -> handles (h2 >>> cl "mdc-dialog__title" $ staticText t)
          w' <- handles (div >>> cl "mdc-dialog__content" >>> "id" := contentId $ content)
          _ <- handles $ div >>> cl "mdc-dialog__actions" $ fromHandles do
            for_ cancel \c -> do
              _ <- handles (dialogButton c)
              node <- gets _.sibling
              liftEffect $ Ref.write (Just node) cancelRef
            decision <- handles (dialogButton decide)
            node <- gets _.sibling
            liftEffect $ Ref.write (Just node) decideRef
            pure decision
          pure w'
    _ <- handles (static (div >>> cl "mdc-dialog__scrim"))
    pure w
  dialogNode <- gets _.sibling
  component <- liftEffect $ newComponent material.dialog."MDCDialog" dialogNode
  decideNode <- liftEffect $ Ref.read decideRef
  cancelNode <- liftEffect $ Ref.read cancelRef
  liftEffect $ variantToVariantWiring
    { open: open component
    , close: close component
    , content: shown
    , reports: \decided -> for_ decideNode \node -> listenNode node "click" decided
    , cancels: \cancelled -> for_ cancelNode \node -> listenNode node "click" cancelled
    }
  where
  dialogButton :: forall r1. String -> PUI Web { | r1 } { | r1 }
  dialogButton caption =
    el "button" >>> "type" := "button" >>> "aria-label" := caption >>> cl "mdc-button" >>> cl "mdc-dialog__button" >>> init (newComponent material.ripple."MDCRipple") mempty mempty $ Semigroupoid.do
      static (div >>> cl "mdc-button__ripple")
      span >>> cl "mdc-button__label" $ staticText caption

-- | The **snackbar**: a brief message at the bottom of the screen,
-- | dismissing itself, for something that has just happened and needs no
-- | reply ("Order placed"). It never interrupts — for something the user
-- | must acknowledge, use `banner` or a `dialog`.
-- |
-- | A status **tells its case**: its caption is the case label verbatim,
-- | so the outcome is named as what it tells (`snackbar @"Card charged"
-- | kept`) and the payload is read by nothing but the fold. Mutually
-- | exclusive outcomes are sibling snackbars, one per business case. A
-- | snackbar is a `+→×` leaf like `fold @l f`: it takes its fold of the
-- | payload — `identity` for an outcome that is the model, `kept` for a
-- | joined outcome that changes nothing, `(const x)` for a reset.
snackbar
  :: forall @l a v r
   . IsSymbol l
  => Cons l a () v
  => (a -> { | r })
  -> PUI Web [ | v ] { | r }
snackbar = statusOver @l snackbarFace

snackbarFace :: forall a r. String -> PUI Web a { | r }
snackbarFace caseLabel = snackbarContainer $ statusText caseLabel

-- opens on every message and auto-dismisses on the foundation's timeout;
-- closing on emission instead would race the open (the `text` leaf echoes
-- synchronously inside every `toUser`)
snackbarContainer :: Ocular (PUI Web)
snackbarContainer content =
  aside >>> cl "mdc-snackbar" >>> init (newComponent material.snackbar."MDCSnackbar") open mempty $
    div >>> cl "mdc-snackbar__surface" >>> "role" := "status" >>> "aria-relevant" := "additions" $
      div >>> cl "mdc-snackbar__label" >>> "aria-atomic" := "false" $
        content

-- | The **banner**: a prominent message at the top of the content for
-- | something the user should actually read — an outage, an empty inbox, a
-- | required action — where a `snackbar` would slip past unnoticed. A
-- | banner stays while its state holds, so it is a surface over a display,
-- | shown by a pane on the model (`( banner $ staticText "Inbox zero!" ) #
-- | inCase @"empty" inboxState`, inbox) and gone when the state resolves:
-- | an ocular, like `card`, with no model interface and no dismiss of its
-- | own — what resolves the state is the app's own emitter beside it.
-- |
-- | Material Design 2 only — MD3 dropped the banner, so `PUI.Web.MDC3` has
-- | none.
banner :: Ocular (PUI Web)
banner content =
  -- the foundation would size the root to its absolutely positioned content
  -- when opening; a banner shown by a pane is open while attached, so the
  -- content sits in flow and the root takes its height
  div >>> cl "mdc-banner" >>> cl "mdc-banner--open" >>> "style" := "height: auto;" >>> "role" := "banner" $
    div >>> cl "mdc-banner__content" >>> "style" := "position: static;" >>> "role" := "status" $
      div >>> cl "mdc-banner__graphic-text-wrapper" $ div >>> cl "mdc-banner__text" $ content

-- | A **menu**: a labelled button that opens a short list of `menuItem`
-- | actions and closes again when one is picked. For actions; for choosing
-- | a value the model keeps, use `select`.
menu :: String -> Ocular (PUI Web)
menu anchorCaption content = div >>> cl "mdc-menu-surface--anchor" >>> "style" := "display: inline-block;" $ fromHandles do
  _ <- handles (staticHTML ("<button class=\"mdc-button mdc-button--outlined\" aria-label=\"" <> escaped anchorCaption <> "\"><span class=\"mdc-button__ripple\"></span><span class=\"mdc-button__label\">" <> escaped anchorCaption <> "</span><i class=\"material-icons mdc-button__icon\" aria-hidden=\"true\">arrow_drop_down</i></button>"))
  anchorNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.ripple."MDCRipple" anchorNode
  w <- handles (div >>> cl "mdc-menu" >>> cl "mdc-menu-surface" $ ul >>> cl "mdc-deprecated-list" >>> "role" := "menu" >>> "aria-hidden" := "true" >>> "aria-orientation" := "vertical" $ content)
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
list content = fromHandles do
  w <- handles (ul >>> cl "mdc-deprecated-list" $ content)
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
listItem content = li >>> cl "mdc-deprecated-list-item" >>> "style" := "height: auto; min-height: 48px;" $ fromHandles do
  _ <- handles (static (span >>> cl "mdc-deprecated-list-item__ripple"))
  handles (span >>> cl "mdc-deprecated-list-item__text" >>> "style" := "display: flex; align-items: center; gap: 16px; width: 100%; white-space: normal; overflow: visible;" $ content)

-- | A **list built from data**: one row per element of the collection the
-- | projection names, each row drawn by the given UI component. Rows matching
-- | `selected` take Material's selected styling — `listOf` selects
-- | nothing — and clicking a row reports *that row*, as case `l` carrying
-- | the row's key field `k` (`listOf @"opened" @"id" …`), so the list is
-- | both how a collection is shown and how the user picks from it.
-- |
-- | The key is a field the projection builds into each row, named rather
-- | than computed: the UI decides its view model, so whatever a pick must
-- | carry is a field of the row. Rows are keyed by it — matched keys are
-- | updated in place, and a row's node moves with its key — so the list
-- | can refresh under the user without flicker; keys must be unique. The
-- | row each list item is drawn from is the projection's signature, in the
-- | view model module (`listOf @"opened" @"id" messagesOf …`).
listOf
  :: forall @l @k r i b1 b2 o key v
   . IsSymbol l
  => IsSymbol k
  => Cons l key b1 v
  => Cons k key b2 r
  => ({ | i } -> Array { | r })
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOf = listOf' @l @k {}


-- | `listOf` with options: `selected` takes a predicate on a row, and the
-- | rows it holds take the catalogue's selected styling (`listOf'
-- | @"Person picked" @"key" { selected: isSelected } entries item`).
listOf'
  :: forall @l @k r provided i b1 b2 o key v
   . IsSymbol l
  => IsSymbol k
  => Cons l key b1 v
  => Cons k key b2 r
  => ConvertOptionsWithDefaults OptSelected { selected :: { | r } -> Boolean } { | provided } { selected :: { | r } -> Boolean }
  => { | provided }
  -> ({ | i } -> Array { | r })
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOf' provided f item = withStructuralOrd @key $ fromHandles do
  w <- handles $ ul >>> cl "mdc-deprecated-list" >>> "style" := "overflow-y: auto;" $
    ( replayed @l (Record.get (Proxy @k)) $ occurrencesOn Click $ clWhen config.selected "mdc-deprecated-list-item--selected"
        $ li >>> cl "mdc-deprecated-list-item" >>> "style" := "cursor: pointer;" $ item
    ) # anyOf @k f
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

-- | `listOf` over an array the model stores: the rows are field `t` of the
-- | fed row, named by its label after the key
-- | (`listOfAt @"Message opened" @"id" @"messages" item`). Rows derived
-- | from the row are a projection, `listOf`.
listOfAt
  :: forall @l @k @t i r b1 b2 b3 o key v
   . IsSymbol l
  => IsSymbol k
  => IsSymbol t
  => Cons l key b1 v
  => Cons k key b2 r
  => Cons t (Array { | r }) b3 i
  => PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOfAt = listOfAt' @l @k @t {}

-- | `listOfAt` with options: `selected` takes a predicate on a row, and the
-- | rows it holds take the catalogue's selected styling
-- | (`listOfAt' @"Message opened" @"id" @"messages" { selected:
-- | highlighted } item`).
listOfAt'
  :: forall @l @k @t provided i r b1 b2 b3 o key v
   . IsSymbol l
  => IsSymbol k
  => IsSymbol t
  => Cons l key b1 v
  => Cons k key b2 r
  => Cons t (Array { | r }) b3 i
  => ConvertOptionsWithDefaults OptSelected { selected :: { | r } -> Boolean } { | provided } { selected :: { | r } -> Boolean }
  => { | provided }
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOfAt' provided = listOf' @l @k provided (Record.get (Proxy @t))

-- | A **data table**: values in rows and columns, where the column a value
-- | sits in is what says what it means. Its header slot holds the fixed
-- | headings, one `columnHeader "…"` each, and its first argument is what
-- | assistive technology announces the table as (`dataTable "Cart"`); the
-- | body is `dataRow`s of `dataCell`s, usually one row per element of a
-- | collection.
dataTable :: String -> PUI Web {} {} -> Ocular (PUI Web)
dataTable tableName headerCells content =
  div >>> cl "mdc-data-table" $
    div >>> cl "mdc-data-table__table-container" $
      table >>> cl "mdc-data-table__table" >>> "aria-label" := tableName $ fromHandles do
        _ <- handles (thead $ tr >>> cl "mdc-data-table__header-row" $ headerCells)
        handles (tbody >>> cl "mdc-data-table__content" $ content)

-- | One **column header** of a `dataTable` — static copy naming the column,
-- | its text a value like every static: `columnHeader "Qty"`.
columnHeader :: forall r. String -> PUI Web { | r } { | r }
columnHeader heading = th >>> cl "mdc-data-table__header-cell" >>> "role" := "columnheader" >>> "scope" := "col" $ staticText heading

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
imageList :: Int -> Ocular (PUI Web)
imageList columns content =
  ul >>> cl "mdc-image-list" >>> cl "mdc-image-list--masonry" >>> "style" := ("column-count: " <> show columns <> "; column-gap: 16px; margin: 0;") $ content

-- | Material's **responsive layout grid**: the column grid a screen's
-- | regions are placed on, holding `layoutCell`s.
layoutGrid :: Ocular (PUI Web)
layoutGrid content = div >>> cl "mdc-layout-grid" $ div >>> cl "mdc-layout-grid__inner" $ content

-- | One region of a `layoutGrid`, `span` columns wide out of twelve — the
-- | grid reflows to fewer columns on narrow screens.
layoutCell :: Int -> Ocular (PUI Web)
layoutCell cellSpan content = div >>> cl "mdc-layout-grid__cell" >>> cl ("mdc-layout-grid__cell--span-" <> show cellSpan) $ content

-- | The **top app bar**: the band carrying the screen's title, with the
-- | content laid out beneath it and clear of it.
topAppBar :: String -> Ocular (PUI Web)
topAppBar title content = fromHandles do
  _ <- handles (staticHTML ("<header class=\"mdc-top-app-bar\"><div class=\"mdc-top-app-bar__row\"><section class=\"mdc-top-app-bar__section mdc-top-app-bar__section--align-start\"><span class=\"mdc-top-app-bar__title\">" <> escaped title <> "</span></section></div></header>"))
  headerNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.topAppBar."MDCTopAppBar" headerNode
  handles (div >>> cl "mdc-top-app-bar--fixed-adjust" $ content)

-- | The permanent **navigation drawer**: a titled nav panel pinned beside
-- | the content, both fed what the drawer is fed. The nav is live, not a
-- | static menu — it is the stage, so what it emits is the drawer's
-- | output: a selectable nav (a `listOf` of sections, `# joined`) reports
-- | its pick to the app's loop, which folds it into the model and feeds
-- | the drawer again, and the content shows that model beside it
-- | (photo-gallery).
drawer :: forall r o. { title :: String, subtitle :: String } -> PUI Web { | r } o -> PUI Web { | r } { | r } -> PUI Web { | r } o
drawer config nav content = div >>> "style" := "display: flex;" $ fromHandles do
  nav' <- handles $ aside >>> cl "mdc-drawer" $ fromHandles do
    _ <- handles (staticHTML ("<div class=\"mdc-drawer__header\"><h3 class=\"mdc-drawer__title\">" <> escaped config.title <> "</h3><h6 class=\"mdc-drawer__subtitle\">" <> escaped config.subtitle <> "</h6></div>"))
    handles (div >>> cl "mdc-drawer__content" $ nav)
  content' <- handles (div >>> cl "mdc-drawer-app-content" >>> "style" := "flex: 1; padding: 16px;" $ content)
  -- the content is a display: its echoes are the row the nav already has
  liftEffect $ content'.fromUser \_ -> pure unit
  pure { toUser: \r -> nav'.toUser r *> content'.toUser r, fromUser: nav'.fromUser }

-- | Attach a **tooltip** to a control: the short explanation that appears
-- | on hover or keyboard focus. For clarification only — never for
-- | information the user needs to complete the task, which belongs on the
-- | screen. The tip is static: its text is fixed when the control is
-- | built, never read from data. Wrap a single control, and write it
-- | trailing so the control still reads first:
-- | `checkbox @"Loyalty member" @"member" @"guest" # tooltip "Members get 10% off"`.
tooltip :: String -> Ocular (PUI Web)
tooltip tipText content = fromHandles do
  tipId <- liftEffect uniqueId
  w <- handles ("aria-describedby" := tipId $ content)
  _ <- handles (staticHTML ("<div id=\"" <> tipId <> "\" class=\"mdc-tooltip\" role=\"tooltip\" aria-hidden=\"true\"><div class=\"mdc-tooltip__surface mdc-tooltip__surface-animation\">" <> escaped tipText <> "</div></div>"))
  tipNode <- gets _.sibling
  _ <- liftEffect $ newComponent material.tooltip."MDCTooltip" tipNode
  pure w

-- announcing statics (`{}`-output chrome with a face, at any row)

-- | A **divider**: the hairline rule between list rows or card sections,
-- | for separating groups that belong to the same surface. Fixed
-- | decoration, carrying no data.
dividerExact :: PUI Web {} {}
dividerExact = staticHTML "<hr class=\"mdc-deprecated-list-divider\" style=\"width: 100%;\">"

divider :: Ocular (PUI Web)
divider = separating dividerExact

-- | One picture in an `imageList`, with `label` shown as its caption and
-- | used as its alternative text.
imageListItem :: forall r. { src :: String, alt :: String } -> PUI Web { | r } { | r }
imageListItem config = staticHTML $
  "<li class=\"mdc-image-list__item\" style=\"margin-bottom: 16px;\">"
    <> "<img class=\"mdc-image-list__image\" src=\"" <> escaped config.src <> "\" alt=\"" <> escaped config.alt <> "\">"
    <> "<div class=\"mdc-image-list__supporting\"><span class=\"mdc-image-list__label\">" <> escaped config.alt <> "</span></div>"
    <> "</li>"


-- | One picture in an `imageList`, **fed through the channel**: like any
-- | display it takes its read function — the business row in, the picture's
-- | `{ src, alt }` out — so a gallery is the retaining `foreach` over the
-- | pictures rather than a wholesale rebuild —
-- | `imagePane developedShot # foreach @"shot" albumShots`, each item built once and its
-- | source and caption updated in place. `imageListItem`'s sibling, for the
-- | collection case; `imageListItem` stays the closure-known static.
imagePane :: forall r. ({ | r } -> { src :: String, alt :: String }) -> PUI Web { | r } { | r }
imagePane picture = imagePaneFace picture

imagePaneFace :: forall r. ({ | r } -> { src :: String, alt :: String }) -> PUI Web { | r } { | r }
imagePaneFace picture =
  li >>> cl "mdc-image-list__item" >>> "style" := "margin-bottom: 16px;" $ Semigroupoid.do
    img >>> cl "mdc-image-list__image" >>> attrWith "src" (_.src <<< picture) >>> attrWith "alt" (_.alt <<< picture) $ identity
    div >>> cl "mdc-image-list__supporting" $ span >>> cl "mdc-image-list__label" $ text (_.alt <<< picture)

-- the element adapter for the index-keyed internal collection: reads the
-- item out of the reconciler's { ix, item } row at the wiring level (the
-- closed-singleton adopters deliberately do not read from wider rows)

-- Private

foreign import data Component :: Type
foreign import data ComponentClass :: Type

-- foundation construction and its method-based protocols
foreign import newComponent :: ComponentClass -> Node -> Effect Component
foreign import open :: Component -> Effect Unit
foreign import close :: Component -> Effect Unit
foreign import layoutComponent :: Component -> Effect Unit
foreign import stashComponent :: Node -> Component -> Effect Unit
foreign import componentWithin :: String -> Node -> Effect Component
foreign import closestOf :: String -> Node -> Effect Node
foreign import selectQuietly :: Component -> String -> Effect Unit
foreign import activateTabNode :: Component -> Node -> Effect Unit
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
foreign import getStringProp :: String -> Component -> Effect String
foreign import setBoolProp :: String -> Component -> Boolean -> Effect Unit
foreign import getBoolProp :: String -> Component -> Effect Boolean

-- component/node events and DOM odds and ends
foreign import listen :: Component -> String -> Effect Unit -> Effect Unit
foreign import destroyComponent :: Component -> Effect Unit
foreign import configureMdcSlider :: Node -> Number -> Number -> Number -> Boolean -> Number -> Effect Unit
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

-- Entry point

-- | Mount the app in the page's `<body>`, dressed for Material 2: the MD2
-- | typography baseline (`mdc-typography`) goes on the page body, so the
-- | plain text between components rides the type scale too, and the app
-- | then mounts exactly as `PUI.Web.HTML.body` does — same signature, same
-- | closed-app demand (`PUI Web {} o`). The entry is a word of the
-- | vocabulary like any other, so a screen changes design system at its
-- | first line by changing the import, and nothing runs at import time.
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  page <- documentBody
  addClass page "mdc-typography"
  HTML.body ui
