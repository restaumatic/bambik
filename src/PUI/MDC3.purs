-- Material Design 3 (https://m3.material.io) components implemented as
-- PUI Web/UIOcular (PUI Web) datatypes — the MD3 sibling of `PUI.MDC`,
-- built on Google's official MD3 web implementation, the `@material/web`
-- custom elements (`<md-filled-button>`, `<md-checkbox>`, ...): importing
-- the FFI module registers the tags, so a component leaf is just
-- `element "md-..."` plus property/event wiring — no foundation classes,
-- no hand-fused ripple/label chrome. The vocabulary is two-sorted, with
-- the same citizenship and (where the concept survived into MD3) the same
-- names and signatures as `PUI.MDC`, so a demo switches design systems by
-- switching the import:
--
--   * **components** — widgets with a model interface, every one a citizen
--     of exactly one row direction:
--       `×→×` editors — `filledTextField @l`, `outlinedTextField @l` (the
--         MD3 variant pair), `filledTextArea @l`, `checkbox @l`,
--         `radioButton @l`, `toggleSwitch @l` (the MD3 Switch),
--         `slider @l`, `select @l` (the MD3 filled select),
--         `segmentedButton @l`, `tabBar @l` (the same-type selector — the
--         `looped`-ensemble citizen), `filterChip @l`, `iconToggle @l`;
--       `×→×` displays — `indeterminateLinearProgress`,
--         `indeterminateCircularProgress` (both `{ busy } → {}`) and the
--         determinate `linearProgress` (`{ value } → {}`);
--       `×→+` events — `button @l` (the filled button; `elevatedButton`,
--         `tonalButton`, `outlinedButton`, `textButton` are the other four
--         MD3 emphasis levels), `fab @l`, `iconButton @l`, `menuItem @l`;
--       `+→×` statuses — `snackbar @l`. MD3 dropped the banner from the
--         catalog, so `banner` has no citizen here.
--   * **oculars** — shape-preserving decorators (`card`, `dialog`, `menu`,
--     `chipSet`, `list`/`listItem`, `dataTable`/`dataRow`/`dataCell`,
--     `imageList`, `layoutGrid`/`layoutCell`, `topAppBar`, `drawer`,
--     `tooltip`, the MD3 typescale — `displayLarge` ... `labelSmall` —
--     and elevations): no model of their own, any polarity.
--   * plus **announcing statics** (`{} → {}` chrome with a face):
--     `divider` (the `<md-divider>`), `imageListItem`.
--
-- M3 catalog entries `@material/web` does not implement (segmented button,
-- snackbar, card, top app bar, navigation drawer, data table, image list,
-- tooltip) are hand-rolled here as minimal chrome over the `--md-sys-*`
-- design tokens, each injecting its stylesheet once via `ensureStyle`; a
-- page that themes the tokens themes them too. Entries with neither an
-- `@material/web` element nor a cheap hand-roll (date/time pickers,
-- bottom/side sheets, badges, navigation bar/rail, search, carousel) are
-- absent.
--
-- Page requirements: the Roboto font and — for `<md-icon>` faces — the
-- Material Symbols Outlined font
-- (https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined).
-- Everything else (component styles, the `.md-typescale-*` classes) ships
-- in the bundle; theming is `--md-sys-color-*`/`--md-sys-typescale-*`
-- custom properties on the page.
--
-- **The `dimap` round-trip contract for editors** holds as in `PUI.MDC`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (`rmap` a total
-- `Model -> Model` after `completed`), not in a leaf bracket.
module PUI.MDC3
  ( OptLabelIcon(..)
  , OptLabel(..)
  , OptIcon(..)
  , OptSelected(..)
  , OptStep(..)
  , bodyLarge
  , bodyMedium
  , bodySmall
  , button
  , card
  , cardActions
  , checkbox
  , chipSet
  , dataCell
  , dataRow
  , dataTable
  , debouncedTextField
  , dialog
  , displayLarge
  , displayMedium
  , displaySmall
  , divider
  , drawer
  , elevatedButton
  , elevation1
  , elevation3
  , elevation5
  , fab
  , filledTextArea
  , filledTextField
  , filterChip
  , headlineLarge
  , headlineMedium
  , headlineSmall
  , iconButton
  , iconToggle
  , imageList
  , imageListItem
  , indeterminateCircularProgress
  , indeterminateLinearProgress
  , labelLarge
  , labelMedium
  , labelSmall
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
  , radioButton
  , segmentedButton
  , select
  , simpleDialog
  , slider
  , sliderLive
  , snackbar
  , tabBar
  , textButton
  , titleLarge
  , titleMedium
  , titleSmall
  , toggleSwitch
  , tonalButton
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
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (field, pempty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Traversable (for)
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (PUI, constantly, foreach)
import PUI.HTML (aside, cl, clWhen, clicked, div, el, h1, h2, h3, init, label, p, span, staticHTML, staticText, table, tbody, td, text, th, thead, tr, (:=))
import PUI.Web (Node, Web, addEventListener, attribute, element, getChecked, getValue, isFocused, onInputDebounced, setAttribute, setChecked, setValue, uniqueId)
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))

-- UIs

-- Conversion tags scope which field names lift a bare value to `Just`, as
-- in `PUI.MDC`: one tag per distinct optional-field set — `OptLabelIcon`
-- (buttons), `OptLabel` (fab, caption via card), `OptStep` (sliders),
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

data OptStep = OptStep

instance ConvertOption OptStep "step" Number (Maybe Number) where
  convertOption _ _ = Just
else instance ConvertOption OptStep sym a a where
  convertOption _ _ = identity

-- | The `×→+` event button (the MD3 filled button — the high-emphasis
-- | default): reads the whole record it is shown and fires it as event
-- | case `l` on click. Both fields are optional and default to `Nothing`:
-- | `button {}` is bare, `button { label: "Count" }` labels it,
-- | `icon: "add"` adds a Material Symbols icon.
button
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
button = buttonOf "md-filled-button"

-- | `button` at the MD3 elevated emphasis.
elevatedButton
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
elevatedButton = buttonOf "md-elevated-button"

-- | `button` at the MD3 filled-tonal emphasis.
tonalButton
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
tonalButton = buttonOf "md-filled-tonal-button"

-- | `button` at the MD3 outlined emphasis.
outlinedButton
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
outlinedButton = buttonOf "md-outlined-button"

-- | `button` at the MD3 text (lowest) emphasis.
textButton
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
textButton = buttonOf "md-text-button"

buttonOf
  :: forall provided r
   . ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => String
  -> { | provided }
  -> PUI Web { | r } [ clicked :: { | r } ]
buttonOf tag provided = recordToCase @"clicked" $ eventLeaf $ el tag $ RecordToRecord.do
  case config.icon of
    Just icon' -> el "md-icon" >>> "slot" := "icon" $ staticText icon'
    Nothing -> pempty
  case config.label of
    Just label' -> staticText label'
    Nothing -> pempty
  where
  config = convertOptionsWithDefaults OptLabelIcon { label: Nothing, icon: Nothing } provided :: { label :: Maybe String, icon :: Maybe String }

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
  el "md-fab" >>> "aria-label" := fromMaybe config.icon config.label >>> extended $
    el "md-icon" >>> "slot" := "icon" $ staticText config.icon
  where
  config = convertOptionsWithDefaults OptLabel { label: Nothing } provided :: { icon :: String, label :: Maybe String }
  extended = case config.label of
    Just label' -> "label" := label'
    Nothing -> identity

-- | The `×→+` event icon button (for the toggling variant see the `×→×`
-- | editor `iconToggle @l`).
iconButton :: forall r. { icon :: String, label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
iconButton config = recordToCase @"clicked" $ eventLeaf $
  el "md-icon-button" >>> "aria-label" := config.label $
    el "md-icon" $ staticText config.icon

-- | The `×→+` event list item for the `menu` ocular: fires the record it
-- | is shown as event case `l` on click (the menu closes itself).
menuItem :: forall r. { label :: String } -> PUI Web { | r } [ clicked :: { | r } ]
menuItem config = recordToCase @"clicked" $ eventLeaf $
  el "md-menu-item" $
    div >>> "slot" := "headline" $ staticText config.label

filledTextField :: { floatingLabel :: String } -> PUI Web { value :: String } { value :: String }
filledTextField config = field @"value" (textFieldLeaf "md-filled-text-field" Nothing config.floatingLabel)

-- | `filledTextField` in the MD3 outlined variant.
outlinedTextField :: { floatingLabel :: String } -> PUI Web { value :: String } { value :: String }
outlinedTextField config = field @"value" (textFieldLeaf "md-outlined-text-field" Nothing config.floatingLabel)

-- | `filledTextField` over a debounced input listener: keystrokes coalesce
-- | at the DOM boundary (`Web.onInputDebounced`), so the field is loop-safe
-- | to debounce — the wire itself stays synchronous.
debouncedTextField :: { floatingLabel :: String, ms :: Number } -> PUI Web { value :: String } { value :: String }
debouncedTextField { floatingLabel, ms } = field @"value" (textFieldLeaf "md-filled-text-field" (Just ms) floatingLabel)

-- the raw MD3 text field — scalar, so private; the custom element carries
-- its own label/ripple chrome, so the leaf is property/event wiring only.
-- Focus-guarded like `Web.input`: model updates never clobber the field
-- being typed in (the element delegates focus, so the host is the
-- activeElement), but still echo so merge gates keep flowing.
textFieldLeaf :: String -> Maybe Number -> String -> PUI Web String String
textFieldLeaf tag mDebounce floatingLabel = wrap do
  element tag (pure unit)
  attribute "label" floatingLabel
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
        case mDebounce of
          Nothing -> void $ addEventListener "input" node $ const do
            value <- getValue node
            prop value
          Just millis -> onInputDebounced node millis prop
    }

filledTextArea :: { columns :: Int, rows :: Int } -> PUI Web { value :: String } { value :: String }
filledTextArea { columns, rows } = field @"value" $ wrap do
  element "md-filled-text-field" (pure unit)
  attribute "type" "textarea"
  attribute "rows" (show rows)
  attribute "style" ("width: " <> show columns <> "ch;")
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

-- | Label content is chrome (`{} → {}`, announcing); a real `<label>`
-- | wrapper associates it, so clicking the text toggles the box.
checkbox :: forall a. Default a => PUI Web {} {} -> PUI Web { value :: Maybe a } { value :: Maybe a }
checkbox labelContent = field @"value" $
  label >>> "style" := "display: inline-flex; align-items: center; gap: 12px;" $ wrap do
    aRef <- liftEffect $ Ref.new default
    mPropRef <- liftEffect $ Ref.new Nothing
    element "md-checkbox" (pure unit)
    node <- gets _.sibling
    lbl <- unwrap labelContent
    pure
      { toUser: \ma -> do
          lbl.toUser {}
          case ma of
            Nothing -> setChecked node false
            Just newa -> do
              setChecked node true
              Ref.write newa aRef
          -- leaf echo: announce what was received, so record-merge gates open
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop ma
      , fromUser: \prop -> do
          Ref.write (Just prop) mPropRef
          listenNode node "change" do
            checked <- getChecked node
            a <- Ref.read aRef
            prop (if checked then Just a else Nothing)
      }

-- | The MD3 radio group, a `×→×` editor. Type-changing like `select @l`:
-- | the input field holds the selection state (`Maybe a`), the output
-- | field the bare selection (`a`). One `<md-radio>` per option; the
-- | shared native `name` gives exclusivity, so each option's emission is
-- | its statically known value.
radioButton :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
radioButton options = field @"value" (radioLeaf options)

radioLeaf :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web (Maybe a) a
radioLeaf options =
  div >>> "style" := "display: flex; flex-direction: column; align-items: flex-start; gap: 8px;" $ wrap do
    groupName <- liftEffect uniqueId
    members <- for options \o -> do
      member <- element "label" do
        element "md-radio" (pure unit)
        radioNode <- gets _.sibling
        liftEffect do
          setAttribute radioNode "name" groupName
          setAttribute radioNode "aria-label" o.label
        _ <- unwrap (staticText o.label)
        pure { radioNode, value: o.value }
      attribute "style" "display: inline-flex; align-items: center; gap: 12px;"
      pure member
    mPropRef <- liftEffect $ Ref.new Nothing
    let render ma = for_ members \m -> setChecked m.radioNode (Just m.value == ma)
    liftEffect $ for_ members \m -> listenNode m.radioNode "change" do
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

-- | The MD3 Switch, a `×→×` `Boolean` editor (the name `switch` was
-- | already taken by the `+→+` case selector).
toggleSwitch :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
toggleSwitch config = field @"value" (switchLeaf config.label)

switchLeaf :: String -> PUI Web Boolean Boolean
switchLeaf lbl =
  label >>> "style" := "display: inline-flex; align-items: center; gap: 12px;" $ wrap do
    element "md-switch" (pure unit)
    node <- gets _.sibling
    _ <- unwrap (staticText lbl)
    mPropRef <- liftEffect $ Ref.new Nothing
    liftEffect $ listenNode node "change" do
      selected <- getBoolProp "selected" node
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop selected
    pure
      { toUser: \b -> do
          setBoolProp "selected" node b
          -- leaf echo: announce what was received, so record-merge gates open
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop b
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }

-- | The `×→×` `Number` editor. An optional `step` makes it the discrete
-- | slider. Emits on **commit** only (thumb release): one emission per
-- | adjustment, so an `updates` fold sees each drag as one transaction.
-- | For continuous mid-drag emissions (live readouts), use `sliderLive`.
slider
  :: forall provided
   . ConvertOptionsWithDefaults OptStep { label :: String, step :: Maybe Number } { | provided } { label :: String, min :: Number, max :: Number, step :: Maybe Number }
  => { | provided }
  -> PUI Web { value :: Number } { value :: Number }
slider provided = field @"value" (sliderLeaf false (sliderConfig provided))

-- | `slider` emitting continuously mid-drag (like mid-typing text); a
-- | consumer that doesn't want the burst wraps its stage in `debounced`.
sliderLive
  :: forall provided
   . ConvertOptionsWithDefaults OptStep { label :: String, step :: Maybe Number } { | provided } { label :: String, min :: Number, max :: Number, step :: Maybe Number }
  => { | provided }
  -> PUI Web { value :: Number } { value :: Number }
sliderLive provided = field @"value" (sliderLeaf true (sliderConfig provided))

sliderConfig
  :: forall provided
   . ConvertOptionsWithDefaults OptStep { label :: String, step :: Maybe Number } { | provided } { label :: String, min :: Number, max :: Number, step :: Maybe Number }
  => { | provided }
  -> { label :: String, min :: Number, max :: Number, step :: Maybe Number }
sliderConfig provided = convertOptionsWithDefaults OptStep { label: "", step: Nothing } provided

-- `<md-slider>` ships no text label of its own (`labeled` is the handle's
-- value indicator), so a non-empty config label renders visibly above the
-- slider, like a text field's floating label.
sliderLeaf :: Boolean -> { label :: String, min :: Number, max :: Number, step :: Maybe Number } -> PUI Web Number Number
sliderLeaf live config
  | config.label == "" = bareSliderLeaf live config
  | otherwise =
      div >>> "style" := "display: inline-flex; flex-direction: column; align-items: flex-start;" $ wrap do
        _ <- unwrap (span >>> cl "md-typescale-label-medium" >>> "style" := "color: var(--md-sys-color-on-surface-variant, #49454f); margin-left: 8px;" $ staticText config.label)
        unwrap (bareSliderLeaf live config)

bareSliderLeaf :: Boolean -> { label :: String, min :: Number, max :: Number, step :: Maybe Number } -> PUI Web Number Number
bareSliderLeaf live config = wrap do
  element "md-slider" (pure unit)
  attribute "min" (show config.min)
  attribute "max" (show config.max)
  attribute "labeled" ""
  attribute "aria-label" config.label
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  liftEffect $ for_ config.step \s -> setAttribute node "step" (show s)
  mPropRef <- liftEffect $ Ref.new Nothing
  when live $ liftEffect $ listenNode node "input" do
    v <- getNumberProp "value" node
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop v
  liftEffect $ listenNode node "change" do
    v <- getNumberProp "value" node
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop v
  pure
    { toUser: \v -> do
        setNumberProp "value" node v
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop v
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The MD3 filled select (exposed dropdown), a `×→×` editor. Type-changing
-- | like `radioButton @l`: the input field holds the selection state
-- | (`Maybe a`), the output field the bare selection (`a`). Options are
-- | design-system config.
select :: forall a. Eq a => { floatingLabel :: String } -> Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
select config options = field @"value" (selectLeaf config options)

selectLeaf :: forall a. Eq a => { floatingLabel :: String } -> Array { value :: a, label :: String } -> PUI Web (Maybe a) a
selectLeaf config options = wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic selection can fire change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "change" do
    busy <- Ref.read busyRef
    unless busy do
      idx <- getIntProp "selectedIndex" node
      for_ (options !! idx) \o -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop o.value
  pure
    { toUser: \ma -> do
        Ref.write true busyRef
        case ma of
          Just a' -> for_ (findIndex (\o -> o.value == a') options) \idx -> setIntProp "selectedIndex" node idx
          Nothing -> setIntProp "selectedIndex" node (-1)
        Ref.write false busyRef
        -- leaf echo (output is the bare selection, so only a `Just` echoes)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> for_ ma \a' -> prop a'
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  markup =
    "<md-filled-select label=\"" <> config.floatingLabel <> "\" style=\"min-width: 200px;\">"
      <> foldMapWithIndex optionMarkup options
      <> "</md-filled-select>"
  optionMarkup idx o =
    "<md-select-option value=\"" <> show idx <> "\"><div slot=\"headline\">" <> o.label <> "</div></md-select-option>"

-- | The MD3 single-select segmented button, a `×→×` editor. Type-changing
-- | like `select @l`. `@material/web` ships no segmented button, so the
-- | chrome is hand-rolled over the design tokens and the wiring is
-- | CSS-class-driven per segment, as in `PUI.MDC`.
segmentedButton :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web { value :: Maybe a } { value :: a }
segmentedButton options = field @"value" (segmentedLeaf options)

segmentedLeaf :: forall a. Eq a => Array { value :: a, label :: String } -> PUI Web (Maybe a) a
segmentedLeaf options =
  div >>> cl "md3-segmented-button" >>> "role" := "radiogroup" $ wrap do
    liftEffect $ ensureStyle "md3-segmented-button" segmentedButtonCss
    segments <- for options \o -> do
      _ <- unwrap (staticHTML ("<button class=\"md3-segmented-button__segment\" role=\"radio\" aria-checked=\"false\"><span class=\"md3-segmented-button__check\" aria-hidden=\"true\"></span><span>" <> o.label <> "</span></button>"))
      node <- gets _.sibling
      pure { node, value: o.value }
    mPropRef <- liftEffect $ Ref.new Nothing
    let render msel = for_ segments \seg -> do
          setClassIf seg.node "md3-segmented-button__segment--selected" (Just seg.value == msel)
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

segmentedButtonCss :: String
segmentedButtonCss = """
.md3-segmented-button { display: inline-flex; border: 1px solid var(--md-sys-color-outline, #79747e); border-radius: 100px; overflow: hidden; }
.md3-segmented-button__segment { display: inline-flex; align-items: center; justify-content: center; gap: 8px; height: 40px; padding: 0 16px; border: none; border-right: 1px solid var(--md-sys-color-outline, #79747e); background: transparent; color: var(--md-sys-color-on-surface, #1d1b20); font: 500 14px/20px Roboto, sans-serif; cursor: pointer; }
.md3-segmented-button__segment:last-child { border-right: none; }
.md3-segmented-button__segment--selected { background: var(--md-sys-color-secondary-container, #e8def8); color: var(--md-sys-color-on-secondary-container, #1d192b); }
.md3-segmented-button__segment--selected .md3-segmented-button__check::before { content: "✓"; }
"""

-- | The MD3 filter chip, a `×→×` `Boolean` editor. Group chips in the
-- | `chipSet` ocular.
filterChip :: { label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
filterChip config = field @"value" (chipLeaf config.label)

chipLeaf :: String -> PUI Web Boolean Boolean
chipLeaf lbl = wrap do
  element "md-filter-chip" (pure unit)
  attribute "label" lbl
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \b -> do
        setBoolProp "selected" node b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop b
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        -- the chip toggles `selected` as the click's default action; read
        -- it a microtask later, once the toggle has settled
        listenNodeDeferred node "click" do
          b <- getBoolProp "selected" node
          prop b
    }

-- | The MD3 icon button (toggle variant), a `×→×` `Boolean` editor —
-- | `onIcon` shows while `true`, `offIcon` while `false`.
iconToggle :: { onIcon :: String, offIcon :: String, label :: String } -> PUI Web { value :: Boolean } { value :: Boolean }
iconToggle config = field @"value" (iconToggleLeaf config)

iconToggleLeaf :: { onIcon :: String, offIcon :: String, label :: String } -> PUI Web Boolean Boolean
iconToggleLeaf config = wrap do
  -- the selected icon renders filled (the MD3 selected-state convention),
  -- so a same-glyph pair still reads as off/on
  _ <- unwrap $ el "md-icon-button" >>> "toggle" := "" >>> "aria-label" := config.label $ RecordToRecord.do
    el "md-icon" $ staticText config.offIcon
    el "md-icon" >>> "slot" := "selected" >>> "style" := "font-variation-settings: 'FILL' 1;" $ staticText config.onIcon
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  liftEffect $ listenNode node "change" do
    on' <- getBoolProp "selected" node
    mProp <- Ref.read mPropRef
    for_ mProp \prop -> prop on'
  pure
    { toUser: \b -> do
        setBoolProp "selected" node b
        -- leaf echo: announce what was received, so record-merge gates open
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The MD3 tab bar, a `×→×` editor like `segmentedButton @l` but
-- | **same-type** (`Cons l a () s`): the selection is always known from the
-- | input, so it echoes unconditionally and sits happily inside `looped`
-- | ensembles (selection field + `provided` payload panes). One
-- | `<md-primary-tab>` per option; `<md-tabs>` drives activation.
tabBar
  :: forall provided a
   . Eq a
  => ConvertOptionsWithDefaults OptIcon { icon :: Maybe String } { | provided } { value :: a, label :: String, icon :: Maybe String }
  => Array { | provided }
  -> PUI Web { value :: a } { value :: a }
tabBar options = field @"value" (tabBarLeaf (convertOptionsWithDefaults OptIcon { icon: Nothing } <$> options))

tabBarLeaf :: forall a. Eq a => Array { value :: a, label :: String, icon :: Maybe String } -> PUI Web a a
tabBarLeaf options = wrap do
  _ <- unwrap (staticHTML ("<md-tabs>" <> foldMap tabMarkup options <> "</md-tabs>"))
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  -- programmatic activation fires change too; guard the loop
  busyRef <- liftEffect $ Ref.new false
  liftEffect $ listenNode node "change" do
    busy <- Ref.read busyRef
    unless busy do
      idx <- getIntProp "activeTabIndex" node
      for_ (options !! idx) \o -> do
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop o.value
  pure
    { toUser: \a -> do
        for_ (findIndex (\o -> o.value == a) options) \idx -> do
          Ref.write true busyRef
          setIntProp "activeTabIndex" node idx
          Ref.write false busyRef
        -- leaf echo: the selection is always known, so always announce
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop a
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }
  where
  tabMarkup o =
    "<md-primary-tab>"
      <> maybe "" (\icon' -> "<md-icon slot=\"icon\">" <> icon' <> "</md-icon>") o.icon
      <> o.label
      <> "</md-primary-tab>"

-- | The `×→×` display citizen for async progress: `{ busy } → {}`, the
-- | shape `PUI.action`'s progress slot expects. Hidden while idle (the
-- | MD3 element has no open/close protocol, so visibility does it).
indeterminateLinearProgress :: PUI Web { busy :: Boolean } {}
indeterminateLinearProgress = wrap do
  element "md-linear-progress" (pure unit)
  attribute "indeterminate" ""
  attribute "aria-label" "Progress Bar"
  attribute "style" hiddenStyle
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setAttribute node "style" (if r.busy then visibleStyle else hiddenStyle)
        -- display echo (like `text`): announce the `{}` per feed, so gated
        -- merges and `tapped`/`completed` stages keep flowing
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }
  where
  visibleStyle = "min-width: 200px;"
  hiddenStyle = "min-width: 200px; visibility: hidden;"

-- | The **determinate** linear progress display, a `{ value :: Number } → {}`
-- | display citizen: `value` is the filled fraction (0.0–1.0). The gauge
-- | shape: `linearProgress # projection fraction`.
linearProgress :: PUI Web { value :: Number } {}
linearProgress = wrap do
  element "md-linear-progress" (pure unit)
  attribute "aria-label" "Progress"
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setNumberProp "value" node r.value
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | `indeterminateLinearProgress`'s circular sibling — the same
-- | `{ busy } → {}` display citizen.
indeterminateCircularProgress :: PUI Web { busy :: Boolean } {}
indeterminateCircularProgress = wrap do
  element "md-circular-progress" (pure unit)
  attribute "indeterminate" ""
  attribute "aria-label" "Progress"
  attribute "style" hiddenStyle
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setAttribute node "style" (if r.busy then visibleStyle else hiddenStyle)
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }
  where
  visibleStyle = "width: 48px; height: 48px;"
  hiddenStyle = "width: 48px; height: 48px; visibility: hidden;"

-- UIOculars

-- the MD3 typescale, via the `.md-typescale-*` classes the FFI module
-- adopts at load (from `@material/web/typography/md-typescale-styles.js`)

displayLarge :: Ocular (PUI Web)
displayLarge w = h1 w # cl "md-typescale-display-large"

displayMedium :: Ocular (PUI Web)
displayMedium w = h1 w # cl "md-typescale-display-medium"

displaySmall :: Ocular (PUI Web)
displaySmall w = h1 w # cl "md-typescale-display-small"

headlineLarge :: Ocular (PUI Web)
headlineLarge w = h2 w # cl "md-typescale-headline-large"

headlineMedium :: Ocular (PUI Web)
headlineMedium w = h2 w # cl "md-typescale-headline-medium"

headlineSmall :: Ocular (PUI Web)
headlineSmall w = h2 w # cl "md-typescale-headline-small"

titleLarge :: Ocular (PUI Web)
titleLarge w = h3 w # cl "md-typescale-title-large"

titleMedium :: Ocular (PUI Web)
titleMedium w = h3 w # cl "md-typescale-title-medium"

titleSmall :: Ocular (PUI Web)
titleSmall w = h3 w # cl "md-typescale-title-small"

bodyLarge :: Ocular (PUI Web)
bodyLarge w = p w # cl "md-typescale-body-large"

bodyMedium :: Ocular (PUI Web)
bodyMedium w = p w # cl "md-typescale-body-medium"

bodySmall :: Ocular (PUI Web)
bodySmall w = p w # cl "md-typescale-body-small"

labelLarge :: Ocular (PUI Web)
labelLarge w = span w # cl "md-typescale-label-large"

labelMedium :: Ocular (PUI Web)
labelMedium w = span w # cl "md-typescale-label-medium"

labelSmall :: Ocular (PUI Web)
labelSmall w = span w # cl "md-typescale-label-small"

-- MD3 elevation levels as surface decorators (box shadows over the tokens;
-- levels 3 and 5 pad like `PUI.MDC`'s `elevation10`/`elevation20`)

elevation1 :: Ocular (PUI Web)
elevation1 = elevationOf "md3-elevation-1"

elevation3 :: Ocular (PUI Web)
elevation3 = elevationOf "md3-elevation-3"

elevation5 :: Ocular (PUI Web)
elevation5 = elevationOf "md3-elevation-5"

elevationOf :: String -> Ocular (PUI Web)
elevationOf klass w = wrap do
  liftEffect $ ensureStyle "md3-elevation" elevationCss
  unwrap (div w # cl klass)

elevationCss :: String
elevationCss = """
.md3-elevation-1 { border-radius: 12px; background: var(--md-sys-color-surface, #fef7ff); box-shadow: 0 1px 2px rgba(0,0,0,.3), 0 1px 3px 1px rgba(0,0,0,.15); }
.md3-elevation-3 { border-radius: 12px; background: var(--md-sys-color-surface, #fef7ff); box-shadow: 0 1px 3px rgba(0,0,0,.3), 0 4px 8px 3px rgba(0,0,0,.15); padding: 25px; }
.md3-elevation-5 { border-radius: 12px; background: var(--md-sys-color-surface, #fef7ff); box-shadow: 0 4px 4px rgba(0,0,0,.3), 0 8px 12px 6px rgba(0,0,0,.15); padding: 25px; }
"""

-- | A card with an optional caption — the caption is design-system config
-- | (like `filledTextField`'s `floatingLabel`). The card is content-agnostic
-- | (any polarity), so its caption chrome is hand-fused, not merged. The
-- | caption defaults to none: `card {}` is captionless, `card { caption:
-- | "Title" }` labels it. `@material/web` ships no card, so the chrome is
-- | hand-rolled over the tokens (the MD3 elevated card).
card
  :: forall provided
   . ConvertOptionsWithDefaults OptLabel { caption :: Maybe String } { | provided } { caption :: Maybe String }
  => { | provided }
  -> Ocular (PUI Web)
card provided content = wrap do
  liftEffect $ ensureStyle "md3-card" cardCss
  unwrap $ div >>> cl "md3-card" $ wrap do
    for_ mCaption \c -> void $ unwrap (div >>> cl "md-typescale-title-medium" $ staticText c)
    unwrap content
  where
  { caption: mCaption } = convertOptionsWithDefaults OptLabel { caption: Nothing } provided :: { caption :: Maybe String }

-- a flex column with a gap: MD3 custom elements carry no margins of their
-- own, so the card supplies the vertical rhythm between its children
cardCss :: String
cardCss = """
.md3-card { background: var(--md-sys-color-surface-container-low, #f7f2fa); color: var(--md-sys-color-on-surface, #1d1b20); border-radius: 12px; box-shadow: 0 1px 2px rgba(0,0,0,.3), 0 1px 3px 1px rgba(0,0,0,.15); padding: 16px; margin: 15px 0; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; }
.md3-card > p { margin: 0; }
"""

-- | The MD3 card button-row area: a flex row for a group of buttons, so they
-- | sit inline at their natural width instead of stretching down the card's
-- | flex column. Wrap a button group: `cardActions $ RecordToVariant.do …`.
cardActions :: Ocular (PUI Web)
cardActions = div >>> "style" := "display: flex; gap: 8px; align-items: center;"

-- | Modal ocular with the open-on-feed/close-on-emission protocol: the
-- | dialog opens (`<md-dialog>.show()` — animation, scrim, Esc) whenever
-- | a value is fed, and closes when its content emits, so feed it
-- | selectively (behind an event case), put the deciding emitters inside,
-- | and the emission both closes the dialog and flows on. The content's
-- | final stage must emit only on decision (buttons, `clicked`) — an
-- | echoing display there would close the dialog the moment it opens.
dialog :: { title :: String } -> Ocular (PUI Web)
dialog { title } content =
  el "md-dialog" >>> init pure showDialog closeDialog $ wrap do
    _ <- unwrap (div >>> "slot" := "headline" $ staticText title)
    unwrap (div >>> "slot" := "content" $ content)

-- | `dialog` with a built-in confirm action: same open-on-feed protocol,
-- | and the confirm button is a `clicked` pass-through — clicking it
-- | emits the content's last output (so give displays a `# tapped`),
-- | which closes the dialog and flows on.
simpleDialog :: { title :: String, confirm :: String } -> Ocular (PUI Web)
simpleDialog { title, confirm } content =
  el "md-dialog" >>> init pure showDialog closeDialog $ Semigroupoid.do
    wrap do
      _ <- unwrap (div >>> "slot" := "headline" $ staticText title)
      unwrap (div >>> "slot" := "content" $ content)
    div >>> "slot" := "actions" $ clicked ((el "md-text-button" $ staticText confirm) # constantly {})

-- | The `+→×` status receiver: shows message case `l` in a snackbar,
-- | contributing no fields (`text` echoes its `{}`, so it announces).
-- | `@material/web` ships no snackbar, so the chrome is hand-rolled over
-- | the tokens: fixed at the bottom, auto-dismissing after 5s (re-feeding
-- | resets the timer).
snackbar :: PUI Web [ event :: String ] {}
snackbar = wrap do
  liftEffect $ ensureStyle "md3-snackbar" snackbarCss
  w <- unwrap $ div >>> cl "md3-snackbar" >>> "role" := "status" $
    lcmap (\v -> { value: Variant.on (Proxy @"event") identity Variant.case_ v }) text
  node <- gets _.sibling
  pure
    { toUser: \i -> do
        w.toUser i
        autoDismiss node "md3-snackbar--open" 5000
    , fromUser: w.fromUser
    }

snackbarCss :: String
snackbarCss = """
.md3-snackbar { position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); background: var(--md-sys-color-inverse-surface, #322f35); color: var(--md-sys-color-inverse-on-surface, #f5eff7); font: 400 14px/20px Roboto, sans-serif; padding: 14px 16px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,.3), 0 4px 8px 3px rgba(0,0,0,.15); visibility: hidden; opacity: 0; transition: opacity .15s; z-index: 1000; }
.md3-snackbar--open { visibility: visible; opacity: 1; }
"""

-- | Anchor button plus `<md-menu>` around a merge of `menuItem @l`s; the
-- | menu closes itself on item selection.
menu :: { label :: String } -> Ocular (PUI Web)
menu config content =
  span >>> "style" := "position: relative; display: inline-block;" $ wrap do
    _ <- unwrap (staticHTML ("<md-outlined-button trailing-icon>" <> config.label <> "<md-icon slot=\"icon\">arrow_drop_down</md-icon></md-outlined-button>"))
    anchorNode <- gets _.sibling
    w <- unwrap (el "md-menu" $ content)
    menuNode <- gets _.sibling
    liftEffect $ listenNode anchorNode "click" (openMenuAnchoredTo menuNode anchorNode)
    pure w

-- | Chrome for a group of `filterChip @l`s.
chipSet :: Ocular (PUI Web)
chipSet = el "md-chip-set"

list :: Ocular (PUI Web)
list = el "md-list"

-- | The MD3 list item; the default slot takes any content, so mixed rows
-- | (typography beside a control) sit side by side.
listItem :: Ocular (PUI Web)
listItem = el "md-list-item"

-- | The MD3 list as a **dynamic collection component**: one item widget per
-- | array element; items satisfying `selected` get the MD3 selected
-- | styling (optional — `listOf {}` selects nothing); every item is a
-- | click emitter replaying its own value, so the component's output is
-- | the clicked item.
listOf
  :: forall provided a o
   . ConvertOptionsWithDefaults OptSelected { selected :: a -> Boolean } { | provided } { selected :: a -> Boolean }
  => { | provided }
  -> PUI Web a o
  -> PUI Web (Array a) a
listOf provided item = wrap do
  liftEffect $ ensureStyle "md3-list" listCss
  unwrap $ el "md-list" >>> "style" := "overflow-y: auto;" $
    ( ( lcmap _.item
          ( clicked $ clWhen config.selected "md3-list-item--selected"
              $ el "md-list-item" >>> "type" := "button" $ item
          ) # foreach @"ix"
      ) # lcmap (mapWithIndex \ix it -> { ix, item: it })
    )
  where
  config = convertOptionsWithDefaults OptSelected { selected: const false } provided

listCss :: String
listCss = """
md-list-item.md3-list-item--selected { --md-list-item-container-color: var(--md-sys-color-secondary-container, #e8def8); }
"""

-- | Table chrome with a static header from config; rows are `dataRow`s of
-- | `dataCell`s. `@material/web` ships no data table, so the chrome is
-- | hand-rolled over the tokens.
dataTable :: { label :: String, columns :: Array String } -> Ocular (PUI Web)
dataTable config content = wrap do
  liftEffect $ ensureStyle "md3-data-table" dataTableCss
  unwrap $ div >>> cl "md3-data-table" $
    table >>> "aria-label" := config.label $ wrap do
      _ <- unwrap (thead $ tr $ headerCells)
      unwrap (tbody $ content)
  where
  headerCells :: PUI Web {} {}
  headerCells = wrap do
    for_ config.columns \c -> void $ unwrap (th >>> "role" := "columnheader" >>> "scope" := "col" $ staticText c)
    pure
      { toUser: mempty
      , fromUser: \prop -> prop {}
      }

dataTableCss :: String
dataTableCss = """
.md3-data-table { border: 1px solid var(--md-sys-color-outline-variant, #cac4d0); border-radius: 12px; overflow: hidden; display: inline-block; }
.md3-data-table table { border-collapse: collapse; width: 100%; font: 400 14px/20px Roboto, sans-serif; color: var(--md-sys-color-on-surface, #1d1b20); }
.md3-data-table th { font-weight: 500; text-align: left; height: 56px; padding: 0 16px; }
.md3-data-table td { height: 52px; padding: 0 16px; border-top: 1px solid var(--md-sys-color-outline-variant, #cac4d0); }
"""

dataRow :: Ocular (PUI Web)
dataRow = tr

dataCell :: Ocular (PUI Web)
dataCell = td

-- | Masonry image list (CSS columns, like `PUI.MDC`'s).
imageList :: { columns :: Int } -> Ocular (PUI Web)
imageList config content = wrap do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  unwrap $ el "ul" >>> cl "md3-image-list" >>> "style" := ("column-count: " <> show config.columns <> "; column-gap: 16px; margin: 0;") $ content

imageListCss :: String
imageListCss = """
.md3-image-list { list-style: none; padding: 0; }
.md3-image-list__item { margin-bottom: 16px; break-inside: avoid; }
.md3-image-list__image { width: 100%; border-radius: 12px; display: block; }
.md3-image-list__label { font: 500 14px/20px Roboto, sans-serif; color: var(--md-sys-color-on-surface, #1d1b20); }
"""

layoutGrid :: Ocular (PUI Web)
layoutGrid = div >>> "style" := "display: grid; grid-template-columns: repeat(12, 1fr); gap: 16px; padding: 16px;"

layoutCell :: { span :: Int } -> Ocular (PUI Web)
layoutCell config = div >>> "style" := ("grid-column: span " <> show config.span <> ";")

-- | Top app bar chrome over the tokens (`@material/web` ships none).
topAppBar :: { title :: String } -> Ocular (PUI Web)
topAppBar config content = wrap do
  liftEffect $ ensureStyle "md3-top-app-bar" topAppBarCss
  _ <- unwrap (staticHTML ("<header class=\"md3-top-app-bar\"><span class=\"md3-top-app-bar__title\">" <> config.title <> "</span></header>"))
  unwrap (div >>> cl "md3-top-app-bar-content" $ content)

topAppBarCss :: String
topAppBarCss = """
.md3-top-app-bar { display: flex; align-items: center; height: 64px; padding: 0 16px; background: var(--md-sys-color-surface-container, #f3edf7); color: var(--md-sys-color-on-surface, #1d1b20); font: 400 22px/28px Roboto, sans-serif; }
.md3-top-app-bar-content { padding: 16px; }
"""

-- | The permanent navigation drawer with a **live nav slot**: nav and
-- | content are sibling stages over the same types — both see every value
-- | fed, and either side's emissions exit the drawer, so a selectable nav
-- | (a `listOf` of sections folded via `updates`) drives the content
-- | beside it. Static chrome nav embeds via `muted`. Hand-rolled chrome
-- | over the tokens (`@material/web` ships no drawer).
drawer :: forall i o. { title :: String, subtitle :: String } -> PUI Web i o -> PUI Web i o -> PUI Web i o
drawer config nav content = div >>> "style" := "display: flex;" $ wrap do
  liftEffect $ ensureStyle "md3-drawer" drawerCss
  nav' <- unwrap (aside >>> cl "md3-drawer" $ wrap do
    _ <- unwrap (staticHTML ("<div class=\"md3-drawer__header\"><h3 class=\"md3-drawer__title\">" <> config.title <> "</h3><h6 class=\"md3-drawer__subtitle\">" <> config.subtitle <> "</h6></div>"))
    unwrap (div $ nav))
  content' <- unwrap (div >>> "style" := "flex: 1; padding: 16px;" $ content)
  pure
    { toUser: \i -> do
        nav'.toUser i
        content'.toUser i
    , fromUser: \prop -> do
        nav'.fromUser prop
        content'.fromUser prop
    }

drawerCss :: String
drawerCss = """
.md3-drawer { width: 256px; flex: none; box-sizing: border-box; background: var(--md-sys-color-surface, #fef7ff); border-right: 1px solid var(--md-sys-color-outline-variant, #cac4d0); padding: 12px; }
.md3-drawer__header { padding: 16px 12px; }
.md3-drawer__title { font: 500 16px/24px Roboto, sans-serif; margin: 0; color: var(--md-sys-color-on-surface, #1d1b20); }
.md3-drawer__subtitle { font: 400 14px/20px Roboto, sans-serif; margin: 0; color: var(--md-sys-color-on-surface-variant, #49454f); }
"""

-- | Attach a hover plain tooltip to the wrapped content (`@material/web`
-- | ships no tooltip; CSS-hover chrome over the tokens). An annotation, not
-- | a container — it reads best trailing, widget first:
-- | `checkbox (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }`.
tooltip :: { text :: String } -> Ocular (PUI Web)
tooltip config content =
  span >>> cl "md3-tooltip-anchor" $ wrap do
    liftEffect $ ensureStyle "md3-tooltip" tooltipCss
    w <- unwrap content
    _ <- unwrap (staticHTML ("<div class=\"md3-tooltip\" role=\"tooltip\">" <> config.text <> "</div>"))
    pure w

tooltipCss :: String
tooltipCss = """
.md3-tooltip-anchor { position: relative; display: inline-block; }
.md3-tooltip { position: absolute; top: 100%; left: 50%; transform: translateX(-50%); margin-top: 4px; background: var(--md-sys-color-inverse-surface, #322f35); color: var(--md-sys-color-inverse-on-surface, #f5eff7); font: 400 12px/16px Roboto, sans-serif; padding: 4px 8px; border-radius: 4px; white-space: nowrap; visibility: hidden; opacity: 0; transition: opacity .15s; z-index: 1000; pointer-events: none; }
.md3-tooltip-anchor:hover .md3-tooltip, .md3-tooltip-anchor:focus-within .md3-tooltip { visibility: visible; opacity: 1; }
"""

-- announcing statics (`{} → {}` chrome with a face)

divider :: PUI Web {} {}
divider = staticHTML "<md-divider style=\"width: 100%;\"></md-divider>"

imageListItem :: { src :: String, label :: String } -> PUI Web {} {}
imageListItem config = wrap do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  unwrap $ staticHTML $
    "<li class=\"md3-image-list__item\">"
      <> "<img class=\"md3-image-list__image\" src=\"" <> config.src <> "\" alt=\"" <> config.label <> "\">"
      <> "<span class=\"md3-image-list__label\">" <> config.label <> "</span>"
      <> "</li>"

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import setIntProp :: String -> Node -> Int -> Effect Unit
foreign import getIntProp :: String -> Node -> Effect Int
foreign import setBoolProp :: String -> Node -> Boolean -> Effect Unit
foreign import getBoolProp :: String -> Node -> Effect Boolean
foreign import listenNode :: Node -> String -> Effect Unit -> Effect Unit
foreign import listenNodeDeferred :: Node -> String -> Effect Unit -> Effect Unit
foreign import setClassIf :: Node -> String -> Boolean -> Effect Unit
foreign import showDialog :: Node -> Effect Unit
foreign import closeDialog :: Node -> Effect Unit
foreign import openMenuAnchoredTo :: Node -> Node -> Effect Unit
foreign import ensureStyle :: String -> String -> Effect Unit
foreign import autoDismiss :: Node -> String -> Int -> Effect Unit
