-- | The **Material Design 3** vocabulary (https://m3.material.io), built on
-- | Google's own `@material/web` components — the twin of `PUI.Web.MDC2`:
-- | where a concept survived from Material 2 it keeps the same name and
-- | signature, so a screen changes design system by changing this one
-- | import. Where Material 3 renamed things this follows the catalogue (the
-- | type scale is `displayLarge` … `labelSmall`, the elevations are
-- | `elevation1`/`elevation3`/`elevation5`), and where Material 3 dropped
-- | something it is simply missing — there is no `banner`.
-- |
-- | **The page must load** the Roboto and Material Symbols Outlined fonts.
-- | Component styles ship in the bundle, so there is no design-system
-- | stylesheet to link; theming is the `--md-sys-color-*` and
-- | `--md-sys-typescale-*` custom properties on the page.
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
-- |   * **act** — `button`, with `elevatedButton`, `tonalButton`,
-- |     `outlinedButton` and `textButton` at the lower emphasis levels;
-- |     `fab`; `iconButton`; `menu`/`menuItem`
-- |   * **be told something** — `snackbar` (passing),
-- |     `dialog`/`simpleDialog` (must be answered), `linearProgress`,
-- |     `indeterminateLinearProgress`, `indeterminateCircularProgress`,
-- |     `tooltip`
-- |   * **structure and surface** — `card`/`cardActions`, `list`/`listItem`,
-- |     `dataTable`/`dataRow`/`dataCell`, `imageList`/`imageListItem`,
-- |     `layoutGrid`/`layoutCell`, `topAppBar`, `drawer`, `chipSet`,
-- |     `divider`, the type scale (`displayLarge` … `labelSmall`) and the
-- |     elevations
-- |
-- | Material 3 entries `@material/web` does not implement (segmented button,
-- | snackbar, card, top app bar, navigation drawer, data table, image list,
-- | tooltip) are hand-rolled here over the same design tokens, so they theme
-- | with everything else. Entries with neither an implementation nor a cheap
-- | hand-roll (date and time pickers, sheets, badges, navigation bar and
-- | rail, search, carousel) are absent.
module PUI.Web.MDC3
  ( OptLabelIcon(..)
  , OptLabel(..)
  , OptIcon(..)
  , OptSelected(..)
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
  , group
  , headlineLarge
  , headlineMedium
  , headlineSmall
  , iconButton
  , iconToggle
  , imageList
  , imageListItem
  , imagePane
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
  , confirmed
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
import Data.Foldable (foldMap, for_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (recordToCase)
import Data.Traversable (for)
import Data.Variant (case_, inj, match, on, prj) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (Ocular, PUI, blank, foreach)
import PUI.Web.HTML (aside, attrWith, cl, clWhen, clicked, div, el, h1, h2, h3, img, init, label, p, shown, span, staticText, table, tbody, td, textOf, th, thead, tr, (:=))
import PUI.Web (Node, Web, OptCaption(..), staticHTML, addEventListener, attribute, element, getChecked, getValue, isFocused, onInputDebounced, removeAttribute, setAttribute, setChecked, setValue, uniqueId)
import QualifiedDo.Semigroupoid as Semigroupoid
import Prim.Row (class Cons, class Union)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Record (get) as Record
import Type.Proxy (Proxy(..))

-- Implementation notes — the reference above is the contract.
--
-- Material Design 3 (https://m3.material.io) components implemented as
-- PUI Web/UIOcular (PUI Web) datatypes — the MD3 sibling of `PUI.Web.MDC2`,
-- built on Google's official MD3 web implementation, the `@material/web`
-- custom elements (`<md-filled-button>`, `<md-checkbox>`, ...): importing
-- the FFI module registers the tags, so a component leaf is just
-- `element "md-..."` plus property/event wiring — no foundation classes,
-- no hand-fused ripple/label chrome. The vocabulary is two-sorted, with
-- the same citizenship and (where the concept survived into MD3) the same
-- names and signatures as `PUI.Web.MDC2`, so a demo switches design systems by
-- switching the import:
--
--   * **components** — UI components with a model interface, every one a citizen
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
-- **The `dimap` round-trip contract for editors** holds as in `PUI.Web.MDC2`:
-- an editor bracketed by `dimap f g` behaves as an iso lens; conversions
-- that can fail or lose information belong in the model (a `settled`
-- normalization on the whole-row stage), not in a leaf bracket.

-- UIs

-- Conversion tags scope which field names lift a bare value to `Just`, as
-- in `PUI.Web.MDC2`: one tag per distinct optional-field set — `OptLabelIcon`
-- (buttons), `OptLabel` (fab), `OptSelected` (listOf),
-- `OptIcon` (tabBar options).
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


-- | The **filled button** — Material 3's high-emphasis action and the
-- | default choice; `elevatedButton`, `tonalButton`, `outlinedButton` and
-- | `textButton` are the same button at the four lower emphasis levels, for
-- | the actions beside it.
-- |
-- | It reports on click, carrying the data it was showing, under the name
-- | the app gives the action: `button @"Book the flight" {}`. Both parts
-- | of the face are optional — the label defaults to the case label
-- | verbatim, so the case *is* the copy (`label:` overrides with copy the
-- | case cannot be), `icon: "add"` puts a Material Symbols glyph before
-- | the label.
button
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
button = buttonOf @l "md-filled-button"

-- | `button` **elevated**: a shadow lifts it off the surface — for an
-- | important action over a busy or patterned background, where a flat fill
-- | would not separate.
elevatedButton
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
elevatedButton = buttonOf @l "md-elevated-button"

-- | `button` **tonal**: a softer fill, one step below filled — the second
-- | action next to a filled one (Save beside Publish).
tonalButton
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
tonalButton = buttonOf @l "md-filled-tonal-button"

-- | `button` **outlined**: a border and no fill — an important action that
-- | is not *the* action of the screen.
outlinedButton
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
outlinedButton = buttonOf @l "md-outlined-button"

-- | `button` at the lowest emphasis — label only: the dismissive or
-- | tertiary action (Cancel, Learn more), and what belongs in dialogs and
-- | cards.
textButton
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | cl ]
textButton = buttonOf @l "md-text-button"

buttonOf
  :: forall @l provided r cl
   . IsSymbol l
  => Cons l { | r } () cl
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => String
  -> { | provided }
  -> PUI Web { | r } [ | cl ]
buttonOf tag provided = recordToCase @l $ eventLeaf $ el tag $ RecordToRecord.do
  case config.icon of
    Just icon' -> el "md-icon" >>> "slot" := "icon" $ staticText icon'
    Nothing -> blank
  case config.label of
    Just label' -> staticText label'
    Nothing -> blank
  where
  config = convertOptionsWithDefaults OptLabelIcon { label: Just (reflectSymbol (Proxy @l)), icon: Nothing } provided :: { label :: Maybe String, icon :: Maybe String }

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
  el "md-fab" >>> "aria-label" := fromMaybe config.icon config.label >>> extended $
    el "md-icon" >>> "slot" := "icon" $ staticText config.icon
  where
  config = convertOptionsWithDefaults OptLabel { label: Just (reflectSymbol (Proxy @l)) } provided :: { icon :: String, label :: Maybe String }
  extended = case config.label of
    Just label' -> "label" := label'
    Nothing -> identity

-- | A compact **icon-only action**, for toolbars, list rows and card
-- | corners where a labelled button would not fit. `label` is not drawn —
-- | it is what assistive technology announces, defaulting to
-- | the case label verbatim. For an icon that stays pressed
-- | (favourite, mute), use `iconToggle` instead.
iconButton :: forall @l provided r cl. IsSymbol l => Cons l { | r } () cl => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { icon :: String, label :: String } => { | provided } -> PUI Web { | r } [ | cl ]
iconButton provided = recordToCase @l $ eventLeaf $
  el "md-icon-button" >>> "aria-label" := config.label $
    el "md-icon" $ staticText config.icon
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { icon :: String, label :: String }

-- | One choice in a `menu`: reports the data it was showing when picked,
-- | and the menu closes itself. The line's text defaults to
-- | the case label verbatim (`label:` overrides with real copy).
menuItem :: forall @l provided r cl. IsSymbol l => Cons l { | r } () cl => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } [ | cl ]
menuItem provided = recordToCase @l $ eventLeaf $
  el "md-menu-item" $
    div >>> "slot" := "headline" $ staticText config.label
  where
  config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided :: { label :: String }

-- | The **filled text field** — Material's default single-line input.
-- | `floatingLabel` names the field and rises above the text once there is
-- | any, so the label is never lost while the field is filled in.
-- |
-- | Shows the string it is given and reports each edit; typing is never
-- | interrupted by values arriving from elsewhere. A whole-row citizen:
-- | fed the wide row, it edits field `l` and carries the rest.
filledTextField :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String } => { | provided } -> PUI Web { | r } { | r }
filledTextField provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "md-filled-text-field" Nothing config.floatingLabel)

-- | `filledTextField` in Material's outlined variant — a border instead of
-- | a fill. Same behaviour; pick one variant and keep to it across a form.
outlinedTextField :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String } => { | provided } -> PUI Web { | r } { | r }
outlinedTextField provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "md-outlined-text-field" Nothing config.floatingLabel)

-- | `filledTextField` that waits `ms` after the last keystroke before
-- | reporting — for a field that drives expensive work (a search, a
-- | recomputed preview) and should not fire once per character.
debouncedTextField :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String, ms :: Number } => { | provided } -> PUI Web { | r } { | r }
debouncedTextField provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "md-filled-text-field" (Just config.ms) config.floatingLabel)

-- the raw MD3 text field — scalar, so private; the custom element carries
-- its own label/ripple chrome, so the leaf is property/event wiring only.
-- Focus-guarded like `Web.input`: model updates never clobber the field
-- being typed in (the element delegates focus, so the host is the
-- activeElement), but still echo so the channel stays live. Debouncing
-- sits at the DOM boundary (`Web.onInputDebounced`), in front of the wire
-- rather than on it, so the field stays loop-safe.
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

-- | The **multi-line text field**, sized in `rows` and `columns` of text —
-- | a note, a description, a message. Otherwise `filledTextField`: the label
-- | floats (`floatingLabel` overrides it for real copy), shows a string,
-- | reports each edit, never interrupts typing.
filledTextArea :: forall @l r rest provided. IsSymbol l => Cons l String rest r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String, columns :: Int, rows :: Int } => { | provided } -> PUI Web { | r } { | r }
filledTextArea provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "md-filled-text-field" (pure unit)
  attribute "type" "textarea"
  attribute "label" config.floatingLabel
  attribute "rows" (show config.rows)
  attribute "style" ("width: " <> show config.columns <> "ch;")
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

-- | The Material **checkbox**, with its label beside it: the label is
-- | ordinary content (usually a `staticText`), properly associated, so
-- | clicking the words toggles the box and the whole line is a comfortable
-- | target.
-- |
-- | The field is a **named two-case variant** the application spells:
-- | `checkbox @"Terms" @"accepted" @"declined" { ticked: {} } (staticText …)`
-- | is ticked exactly while `"Terms"` sits at `accepted`; ticking reports
-- | `.accepted ticked`, clearing reports `.declined {}` — so an optional part
-- | of the model *is* the box's state under its own names, with no second
-- | flag to keep in step and no `Maybe`. Use a checkbox for a fact the user
-- | states as part of a form; use `toggleSwitch` for a setting that takes
-- | effect at once.
-- |
-- | `ticked` is the ticked case's payload before the model has ever supplied
-- | one — stated by the caller (`{ ticked: {} }` for a plain yes/no fact),
-- | never conjured from the type.
checkbox :: forall @l @c @n a r rest v cr nr. IsSymbol l => IsSymbol c => IsSymbol n => Cons l [ | v ] rest r => Cons c a cr v => Cons n {} nr v => { ticked :: a } -> PUI Web {} {} -> PUI Web { | r } { | r }
checkbox { ticked } labelContent = field @l $ "name" := reflectSymbol (Proxy @l) $
  label >>> "style" := "display: inline-flex; align-items: center; gap: 12px;" $ wrap do
    aRef <- liftEffect $ Ref.new ticked
    mPropRef <- liftEffect $ Ref.new Nothing
    element "md-checkbox" (pure unit)
    node <- gets _.sibling
    lbl <- unwrap labelContent
    pure
      { toUser: \ma -> do
          lbl.toUser {}
          case Variant.prj (Proxy @c) ma of
            Nothing -> setChecked node false
            Just newa -> do
              setChecked node true
              Ref.write newa aRef
          -- leaf echo: announce what was received, so the lifted stage releases
          -- the row and any enclosing merge gate opens
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop ma
      , fromUser: \prop -> do
          Ref.write (Just prop) mPropRef
          listenNode node "change" do
            checked <- getChecked node
            a <- Ref.read aRef
            prop (if checked then Variant.inj (Proxy @c) a else Variant.inj (Proxy @n) {})
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

-- | The Material **switch**: a setting that takes effect the moment it is
-- | flipped — notifications on, dark mode on. (A `checkbox` states a fact
-- | to be submitted with the rest of a form; a switch acts immediately.)
toggleSwitch :: forall @l r rest provided. IsSymbol l => Cons l Boolean rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
toggleSwitch provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (switchLeaf config.label)

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
          -- leaf echo: announce what was received, so the lifted stage releases
          -- the row and any enclosing merge gate opens
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop b
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }

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
slider :: forall @l r rest provided. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
slider provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (sliderLeaf false config.label)

-- | `slider` reporting continuously while the thumb moves — for a live
-- | readout or preview that has to follow the drag. Whatever it drives
-- | should be cheap to redo; a drag that should land in the history as one
-- | change needs the plain `slider`, or a `debounced` stage downstream.
sliderLive :: forall @l r rest provided. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
sliderLive provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (sliderLeaf true config.label)

-- `<md-slider>` ships no text label of its own (`labeled` is the handle's
-- value indicator), so a non-empty config label renders visibly above the
-- slider, like a text field's floating label.
sliderLeaf :: Boolean -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
sliderLeaf live label
  | label == "" = bareSliderLeaf live label
  | otherwise =
      div >>> "style" := "display: inline-flex; flex-direction: column; align-items: flex-start;" $ wrap do
        _ <- unwrap (span >>> cl "md-typescale-label-medium" >>> "style" := "color: var(--md-sys-color-on-surface-variant, #49454f); margin-left: 8px;" $ staticText label)
        unwrap (bareSliderLeaf live label)

bareSliderLeaf :: Boolean -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
bareSliderLeaf live label = wrap do
  element "md-slider" (pure unit)
  attribute "labeled" ""
  attribute "aria-label" label
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  qRef <- liftEffect $ Ref.new Nothing
  let emit = do
        v <- getNumberProp "value" node
        mq <- Ref.read qRef
        for_ mq \q -> do
          mProp <- Ref.read mPropRef
          for_ mProp \prop -> prop (q { current = v })
  when live $ liftEffect $ listenNode node "input" emit
  liftEffect $ listenNode node "change" emit
  pure
    { toUser: \q -> do
        Ref.write (Just q) qRef
        setNumberProp "min" node q.min
        setNumberProp "max" node q.max
        Variant.match { discrete: \s -> setNumberProp "step" node s, continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "value" node q.current
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop q
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The Material **filled select** (exposed dropdown): one choice out of a
-- | list too long to lay out in the open. `floatingLabel` names the field
-- | and stays visible above the choice once one is made. For a handful of
-- | options worth comparing side by side, prefer `radioButton` or
-- | `segmentedButton`.
-- |
-- | Same contract as `radioButton`: nothing to show until the user picks,
-- | so say `# optional @"chosen" @"unchosen"` or `# required`; the options are part of the
-- | control, not of the model.
select :: forall @l a ri ro provided. IsSymbol l => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String } { | provided } { floatingLabel :: String } => { | provided } -> Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
select provided options = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (selectLeaf config options)

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

-- | The Material **segmented button**: two to five options joined in one
-- | control, all visible, one selected — a filter row, a view switch, a
-- | size. Compact where a radio group would be airy and a dropdown would
-- | hide the alternatives. Same picked/unpicked contract as `select`.
segmentedButton :: forall @l a ri ro. IsSymbol l => Cons l (Maybe a) () ri => Cons l a () ro => Eq a => Array { value :: a, label :: String } -> PUI Web { | ri } { | ro }
segmentedButton options = field @l $ "name" := reflectSymbol (Proxy @l) $ (segmentedLeaf options)

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

-- | The Material **filter chip**: a small tag the user switches on or off,
-- | showing a checkmark while on. Chips come in sets where any number may
-- | be active at once — dietary tags, categories, facets. Put them in a
-- | `chipSet`.
filterChip :: forall @l r rest provided. IsSymbol l => Cons l Boolean rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { label :: String } => { | provided } -> PUI Web { | r } { | r }
filterChip provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (chipLeaf config.label)

chipLeaf :: String -> PUI Web Boolean Boolean
chipLeaf lbl = wrap do
  element "md-filter-chip" (pure unit)
  attribute "label" lbl
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \b -> do
        setBoolProp "selected" node b
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
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

-- | An **icon that stays pressed** — favourite, bookmark, mute, pin:
-- | `onIcon` is shown while it is on, `offIcon` while it is off, and
-- | `label` is what assistive technology announces. The on glyph renders
-- | filled, so the same glyph in both slots still reads as off and on. The
-- | compact form of a `toggleSwitch`, for list rows and toolbars.
iconToggle :: forall @l r rest provided. IsSymbol l => Cons l Boolean rest r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { onIcon :: String, offIcon :: String, label :: String } => { | provided } -> PUI Web { | r } { | r }
iconToggle provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in field @l $ "name" := reflectSymbol (Proxy @l) $ (iconToggleLeaf config)

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
        -- leaf echo: announce what was received, so the lifted stage releases
        -- the row and any enclosing merge gate opens
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop b
    , fromUser: \prop -> Ref.write (Just prop) mPropRef
    }

-- | The Material **tab bar**: the top-level sections of one screen, one
-- | open at a time. Keyboard navigation and the sliding indicator come with
-- | it.
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

-- | The **indeterminate progress bar**: work is under way and there is no
-- | telling how long — a request in flight, a file being processed. Shown
-- | while `busy`, gone when it isn't, so it is driven by the app's own
-- | notion of being busy rather than by a separate visibility flag.
indeterminateLinearProgress :: forall @l r. IsSymbol l => Cons l Boolean () r => PUI Web { | r } {}
indeterminateLinearProgress = wrap do
  element "md-linear-progress" (pure unit)
  attribute "indeterminate" ""
  attribute "aria-label" (reflectSymbol (Proxy @l))
  attribute "style" hiddenStyle
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setAttribute node "style" (if Record.get (Proxy @l) r then visibleStyle else hiddenStyle)
        -- display echo (like `text`): announce the `{}` per feed, so gated
        -- merges and whole-row editor stages keep flowing
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }
  where
  visibleStyle = "min-width: 200px;"
  hiddenStyle = "min-width: 200px; visibility: hidden;"

-- | The **determinate progress bar**: how far along something is, `value`
-- | running 0 to 1. As much a gauge as a progress indicator — a quiz's
-- | position, a budget's use, a quota — written as
-- | `linearProgress @"Progress" progressFraction`: the value is a **read
-- | function** like every display's, since a fraction is derived (a ratio
-- | of source fields), not state. The label is the accessible name only,
-- | so it is copy, never a field reference.
linearProgress
  :: forall @l reads
   . IsSymbol l
  => ({ | reads } -> Number) -> PUI Web { | reads } {}
linearProgress f = wrap do
  element "md-linear-progress" (pure unit)
  attribute "aria-label" (reflectSymbol (Proxy @l))
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setNumberProp "value" node (f r)
        -- display echo (like `text`)
        mProp <- Ref.read mPropRef
        for_ mProp \prop -> prop {}
    , fromUser: \prop -> do
        Ref.write (Just prop) mPropRef
        prop {}
    }

-- | The **spinner** — `indeterminateLinearProgress` in circular form, for
-- | inline and compact places (a button, a card corner) where a bar across
-- | the width would be too much.
indeterminateCircularProgress :: forall @l r. IsSymbol l => Cons l Boolean () r => PUI Web { | r } {}
indeterminateCircularProgress = wrap do
  element "md-circular-progress" (pure unit)
  attribute "indeterminate" ""
  attribute "aria-label" (reflectSymbol (Proxy @l))
  attribute "style" hiddenStyle
  node <- gets _.sibling
  mPropRef <- liftEffect $ Ref.new Nothing
  pure
    { toUser: \r -> do
        setAttribute node "style" (if Record.get (Proxy @l) r then visibleStyle else hiddenStyle)
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

-- | **Display** type at its largest — a hero number or a splash figure, at
-- | most once on a screen. Also the page's top-level heading.
displayLarge :: Ocular (PUI Web)
displayLarge w = h1 w # cl "md-typescale-display-large"

-- | **Display** type, one step down from `displayLarge`.
displayMedium :: Ocular (PUI Web)
displayMedium w = h1 w # cl "md-typescale-display-medium"

-- | The smallest **display** step — expressive type that still leads a
-- | page.
displaySmall :: Ocular (PUI Web)
displaySmall w = h1 w # cl "md-typescale-display-small"

-- | **Headline**: the screen's title — large enough to lead without the
-- | drama of display type.
headlineLarge :: Ocular (PUI Web)
headlineLarge w = h2 w # cl "md-typescale-headline-large"

-- | **Headline**, one step down — a major section of a screen.
headlineMedium :: Ocular (PUI Web)
headlineMedium w = h2 w # cl "md-typescale-headline-medium"

-- | The smallest **headline** step.
headlineSmall :: Ocular (PUI Web)
headlineSmall w = h2 w # cl "md-typescale-headline-small"

-- | **Title**: a heading inside the content — a card's title, a group's
-- | name, a dialog's subject.
titleLarge :: Ocular (PUI Web)
titleLarge w = h3 w # cl "md-typescale-title-large"

-- | **Title**, one step down — a subheading within a section.
titleMedium :: Ocular (PUI Web)
titleMedium w = h3 w # cl "md-typescale-title-medium"

-- | The smallest **title** step, for dense groupings.
titleSmall :: Ocular (PUI Web)
titleSmall w = h3 w # cl "md-typescale-title-small"

-- | **Body** text at its most readable — long passages meant to be read.
bodyLarge :: Ocular (PUI Web)
bodyLarge w = p w # cl "md-typescale-body-large"

-- | **Body** text at the default size — the workaday paragraph.
bodyMedium :: Ocular (PUI Web)
bodyMedium w = p w # cl "md-typescale-body-medium"

-- | The smallest **body** step — fine print, timestamps, footnotes.
bodySmall :: Ocular (PUI Web)
bodySmall w = p w # cl "md-typescale-body-small"

-- | **Label** type: the words on and around controls — a button's text, a
-- | field's caption. Not for running text.
labelLarge :: Ocular (PUI Web)
labelLarge w = span w # cl "md-typescale-label-large"

-- | **Label** type, one step down — a compact caption beside a control.
labelMedium :: Ocular (PUI Web)
labelMedium w = span w # cl "md-typescale-label-medium"

-- | The smallest **label** step — an overline or a tiny annotation.
labelSmall :: Ocular (PUI Web)
labelSmall w = span w # cl "md-typescale-label-small"

-- MD3 elevation levels as surface decorators (box shadows over the tokens;
-- levels 3 and 5 pad like `PUI.Web.MDC2`'s `elevation10`/`elevation20`)

-- | Lift the content onto a **barely raised surface** — the resting height
-- | of a card: enough shadow to separate it from the background.
elevation1 :: Ocular (PUI Web)
elevation1 = elevationOf "md3-elevation-1"

-- | Lift the content onto a **clearly raised, padded panel** — a surface
-- | that reads as floating above the page, like a menu or a picked-up card.
elevation3 :: Ocular (PUI Web)
elevation3 = elevationOf "md3-elevation-3"

-- | Lift the content **highest**, onto a padded panel with a deep shadow —
-- | the topmost surface on the screen, for a modal-weight panel.
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

-- | A **card**: a raised surface holding one subject's content and actions
-- | — an order, a product, a summary. Takes any content; put a row of
-- | buttons in `cardActions`.
-- |
-- | A plain ocular, with no config of its own: MD3 defines a card as a
-- | *surface* and gives it no title — `@material/web`'s own card element is
-- | a bare `<slot>` with no properties — so a card's heading is ordinary
-- | typography placed in its content (`titleMedium`, `headlineSmall`). A
-- | card whose content is one model sub-record is not chrome but the
-- | labelled group, `group @l`.
card :: Ocular (PUI Web)
card content = wrap do
  liftEffect $ ensureStyle "md3-card" cardCss
  unwrap $ div >>> cl "md3-card" $ content

-- a flex column with a gap: MD3 custom elements carry no margins of their
-- own, so the card supplies the vertical rhythm between its children
cardCss :: String
cardCss = """
.md3-card { background: var(--md-sys-color-surface-container-low, #f7f2fa); color: var(--md-sys-color-on-surface, #1d1b20); border-radius: 12px; box-shadow: 0 1px 2px rgba(0,0,0,.3), 0 1px 3px 1px rgba(0,0,0,.15); padding: 16px; margin: 15px 0; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; }
.md3-card > p { margin: 0; }
"""

-- | The card's **action row**: the buttons belonging to the card, side by
-- | side at their natural width instead of stretched down its column.
cardActions :: Ocular (PUI Web)
cardActions = div >>> "style" := "display: flex; gap: 8px; align-items: center;"

-- | A **labelled model group** — the card as visual wrapper *and*
-- | presentation-model wrapper, one word. `group @l w` renders `w` on a
-- | `card` headed by the label verbatim (`titleMedium`) and nests `w`'s
-- | model under field `l`, stamping the label as the accessible group name
-- | (`role="group"`, `aria-labelledby` the heading) — so the group's view
-- | line anchors at its field like every labelled leaf, and the words
-- | appear once. Without it a labelled group is three hand-aligned
-- | spellings — the `card`, a `staticText` heading, a trailing `# field @l`
-- | — free to drift apart.
-- |
-- | Derived, not primitive:
-- |
-- | ```
-- | group @l w = card (heading >>> field @l w)      -- + the a11y stamp
-- | ```
-- |
-- | well-defined because chrome commutes with the strengths (`Ocular`'s
-- | admission law): `card w # field @l = card (w # field @l)`, so fusing
-- | surface and lens loses nothing. The focus is any type `field @l`
-- | accepts — an editor ensemble (`# group @"Customer"`), a `bracketed`
-- | variant editor (`# group @"Fulfillment"`), a collection's array
-- | (potluck's `# group @"Guests"` over `acted`, reorder's
-- | `# group @"Setlist"` over `edited`). Fusion is earned by the
-- | leaf criterion — the label does work a trailing `# field @l` cannot
-- | (heading copy, accessible name) — so a card grouping no model (a
-- | display card, a button row) stays the blind `card`, and a flat
-- | sub-row focus stays `subStrong` under caller-chosen chrome.
group
  :: forall @l f f' b s t
   . IsSymbol l
  => Cons l f b s
  => Cons l f' b t
  => PUI Web f f'
  -> PUI Web { | s } { | t }
group w = wrap do
  headingId <- liftEffect uniqueId
  unwrap $ "role" := "group" $ "aria-labelledby" := headingId $ card $
    shown ("id" := headingId $ titleMedium $ staticText (reflectSymbol (Proxy @l))) >>> field @l w

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
dialog { title } content =
  el "md-dialog" >>> init pure showDialog closeDialog $ wrap do
    _ <- unwrap (div >>> "slot" := "headline" $ staticText title)
    unwrap (div >>> "slot" := "content" $ content)

-- | The witness rung — see `PUI.Web.MDC2.confirmed`.
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
simpleDialog { title, confirm } content =
  el "md-dialog" >>> init pure showDialog closeDialog $ Semigroupoid.do
    wrap do
      _ <- unwrap (div >>> "slot" := "headline" $ staticText title)
      unwrap (div >>> "slot" := "content" $ content)
    div >>> "slot" := "actions" $ clicked ((el "md-text-button" $ staticText confirm))

-- | The **snackbar**: a brief message at the bottom of the screen that
-- | dismisses itself after a few seconds, for something that has just
-- | happened and needs no reply ("Order placed"). It never interrupts — for
-- | something the user must acknowledge, use a `dialog`.
-- |
-- | The wording belongs to the UI, not to the event: write the copy where
-- | the snackbar is built — `snackbar # forCase @"brewed" brewedLine` — and
-- | let the event carry the bare facts. One snackbar can serve several
-- | mutually exclusive outcomes with `forCases`.
snackbar :: PUI Web [ event :: String ] {}
snackbar = wrap do
  liftEffect $ ensureStyle "md3-snackbar" snackbarCss
  w <- unwrap $ div >>> cl "md3-snackbar" >>> "role" := "status" $
    textOf eventText
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

-- | A **menu**: a labelled button that opens a short list of `menuItem`
-- | actions and closes again when one is picked. For actions; for choosing
-- | a value the model keeps, use `select`.
menu :: { label :: String } -> Ocular (PUI Web)
menu config content =
  span >>> "style" := "position: relative; display: inline-block;" $ wrap do
    _ <- unwrap (staticHTML ("<md-outlined-button trailing-icon>" <> config.label <> "<md-icon slot=\"icon\">arrow_drop_down</md-icon></md-outlined-button>"))
    anchorNode <- gets _.sibling
    w <- unwrap (el "md-menu" $ content)
    menuNode <- gets _.sibling
    liftEffect $ listenNode anchorNode "click" (openMenuAnchoredTo menuNode anchorNode)
    pure w

-- | The wrapping row a group of `filterChip`s sits in — chips are a set,
-- | never a lone control.
chipSet :: Ocular (PUI Web)
chipSet = el "md-chip-set"

-- | A **list**: rows of `listItem`s, with Material's row rhythm and
-- | keyboard navigation. For a list built from data, and clickable, use
-- | `listOf`.
list :: Ocular (PUI Web)
list = el "md-list"

-- | One **list row**, taking any content — so a label beside a control sits
-- | side by side on one line.
listItem :: Ocular (PUI Web)
listItem = el "md-list-item"

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
  liftEffect $ ensureStyle "md3-list" listCss
  unwrap $ el "md-list" >>> "style" := "overflow-y: auto;" $
    ( inRow ( clicked @r @() $ clWhen config.selected "md3-list-item--selected"
          $ el "md-list-item" >>> "type" := "button" $ item
      ) # foreach @"ix" (mapWithIndex (\ix it -> { ix, item: it }) <<< f)
    )
  where
  config = convertOptionsWithDefaults OptSelected { selected: const false } provided
-- the canonical status payload, read into the text leaf as its projection
eventText :: [ event :: String ] -> String
eventText = Variant.on (Proxy @"event") identity Variant.case_


listCss :: String
listCss = """
md-list-item.md3-list-item--selected { --md-list-item-container-color: var(--md-sys-color-secondary-container, #e8def8); }
"""

-- | A **data table**: values in rows and columns, where the column a value
-- | sits in is what says what it means. `columns` are the fixed headings
-- | and `label` is what assistive technology announces the table as; the
-- | body is `dataRow`s of `dataCell`s, usually one row per element of a
-- | collection.
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

-- | One row of a `dataTable` — a single record's line across the columns.
dataRow :: Ocular (PUI Web)
dataRow = tr

-- | One cell of a `dataRow`: the value under one column heading. Cells are
-- | written in the same order as the table's `columns`.
dataCell :: Ocular (PUI Web)
dataCell = td

-- | An **image list**: pictures laid out in `columns` masonry columns, each
-- | one an `imageListItem` — a gallery, where the pictures are the content
-- | rather than an illustration of it.
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

-- | The twelve-column **layout grid** a screen's regions are placed on,
-- | holding `layoutCell`s.
layoutGrid :: Ocular (PUI Web)
layoutGrid = div >>> "style" := "display: grid; grid-template-columns: repeat(12, 1fr); gap: 16px; padding: 16px;"

-- | One region of a `layoutGrid`, `span` columns wide out of twelve.
layoutCell :: { span :: Int } -> Ocular (PUI Web)
layoutCell config = div >>> "style" := ("grid-column: span " <> show config.span <> ";")

-- | The **top app bar**: the band carrying the screen's title, with the
-- | content laid out beneath it.
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

-- | The permanent **navigation drawer**: a titled nav panel pinned beside
-- | the content. The nav is live, not a static menu — both sides see the
-- | same data and either can report, so a selectable nav (a `listOf` of
-- | sections) drives what is shown next to it.
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

-- | Attach a **tooltip** to a control: the short explanation that appears
-- | on hover or keyboard focus. For clarification only — never for
-- | information the user needs to complete the task, which belongs on the
-- | screen. Wrap a single control, and write it trailing so the control
-- | still reads first:
-- | `checkbox @"Loyalty" @"member" @"guest" { ticked: {} } (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }`.
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

-- | A **divider**: the hairline rule between list rows or card sections,
-- | for separating groups that belong to the same surface. Fixed
-- | decoration, carrying no data.
divider :: PUI Web {} {}
divider = staticHTML "<md-divider style=\"width: 100%;\"></md-divider>"

-- | One picture in an `imageList`, with `label` shown as its caption and
-- | used as its alternative text.
imageListItem :: { src :: String, label :: String } -> PUI Web {} {}
imageListItem config = wrap do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  unwrap $ staticHTML $
    "<li class=\"md3-image-list__item\">"
      <> "<img class=\"md3-image-list__image\" src=\"" <> config.src <> "\" alt=\"" <> config.label <> "\">"
      <> "<span class=\"md3-image-list__label\">" <> config.label <> "</span>"
      <> "</li>"

-- | One picture in an `imageList`, **fed through the channel**: the
-- | canonical `{ src, label }` row arrives as data, so a gallery is the
-- | retaining `foreach` over the pictures rather than a wholesale rebuild —
-- | `imagePane # foreach @"src" albumPhotos`, each item built once and its
-- | source and caption updated in place. `imageListItem`'s sibling, for the
-- | collection case; `imageListItem` stays the closure-known static.
imagePane :: PUI Web { src :: String, label :: String } {}
imagePane = wrap do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  unwrap $ el "li" >>> cl "md3-image-list__item" $ RecordToRecord.do
    imageFace
    span >>> cl "md3-image-list__label" $ (textOf _.label :: PUI Web { src :: String, label :: String } {})

imageFace :: PUI Web { src :: String, label :: String } {}
imageFace =
  img >>> cl "md3-image-list__image" >>> attrWith "src" _.src >>> attrWith "alt" _.label $ blank

-- the element adapter for the index-keyed internal collection: reads the
-- item out of the reconciler's { ix, item } row at the wiring level (the
-- closed-singleton adopters deliberately do not read from wider rows)
inRow :: forall a o. PUI Web a o -> PUI Web { ix :: Int, item :: a } o
inRow w = wrap $ unwrap w <#> \w' -> { toUser: \r -> w'.toUser r.item, fromUser: w'.fromUser }

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
