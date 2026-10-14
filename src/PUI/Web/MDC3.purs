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
-- | `--md-sys-typescale-*` custom properties on the page. The app enters
-- | through this vocabulary's `body`, which adopts the typescale stylesheet
-- | before mounting; nothing happens at import time.
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
-- |   * **enter** — `body`, the page mounted and dressed for Material 3
-- |
-- | Material 3 entries `@material/web` does not implement (segmented button,
-- | snackbar, card, top app bar, navigation drawer, data table, image list,
-- | tooltip) are hand-rolled here over the same design tokens, so they theme
-- | with everything else. Entries with neither an implementation nor a cheap
-- | hand-roll (date and time pickers, sheets, badges, navigation bar and
-- | rail, search, carousel) are absent.
module PUI.Web.MDC3
  ( body
  , bodyLarge
  , bodyMedium
  , bodySmall
  , button
  , button'
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
  , listOf'
  , listOfAt
  , listOfAt'
  , menu
  , menuItem
  , outlinedButton
  , outlinedTextField
  , radioButton
  , radioButtonUnpicked
  , radioButtonOptional
  , segmentedButton
  , segmentedButtonUnpicked
  , segmentedButtonOptional
  , select
  , selectUnpicked
  , selectOptional
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

import Data.Profunctor.Row.Structural (withStructuralEq, withStructuralOrd)
import Prelude hiding (div)

import Control.Monad.State (gets)
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Array (findIndex, (!!))
import Data.Foldable (foldMap, for_, traverse_)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row (atCase)
import Data.Traversable (for)
import Data.Variant (inj, match, prj) as Variant
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (anyOf, folding, isHole, Ocular, PUI)
import PUI.Web.HTML (aside, div, h1, h2, h3, img, label, p, span, table, tbody, td, th, thead, tr)
import PUI.Web.HTML (body) as HTML
import PUI.Web (Node, OptCaption(..), OptIcon(..), OptLabelIcon(..), OptSelected(..), Web, attrWith, attribute, autoDismiss, cl, clWhen, clearedOnRepress, clicked, displayWiring, editorWiring, el, element, ensureStyle, escaped, eventLeaf, getChecked, getValue, init, isFocused, listenNode, listenNodeDeferred, onInputDebounced, removeAttribute, selectedAt, selectedOptionalAt, selectedUnpickedAt, selectorWiring, separating, setAttribute, setChecked, setValue, staticHTML, staticText, statusWiring, text, statusText, uniqueId, witnessed, (:=))
import QualifiedDo.Semigroupoid as Semigroupoid
import Prim.Row (class Cons)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Record as Record
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
--     of exactly one row shape:
--       `×→×` editors — `filledTextField @l`, `outlinedTextField @l` (the
--         MD3 variant pair), `filledTextArea @l`, `checkbox @l`,
--         `radioButton @l`, `toggleSwitch @l` (the MD3 Switch),
--         `slider @l`, `select @l` (the MD3 filled select),
--         `segmentedButton @l`, `tabBar @l` (the same-type selector — the
--         `looped`-ensemble citizen), `filterChip @l`, `iconToggle @l`;
--       `×→×` displays — the determinate `linearProgress`
--         (`{ | reads } → {}` over a read function);
--       `×→+` events — `button @l` (the filled button; `elevatedButton`,
--         `tonalButton`, `outlinedButton`, `textButton` are the other four
--         MD3 emphasis levels), `fab @l`, `iconButton @l`, `menuItem @l`;
--       `+→×` statuses — `snackbar` (MD3 dropped the banner from the
--         catalog, so `banner` has no citizen here) and
--         `indeterminateLinearProgress`/`indeterminateCircularProgress`
--         (`[ started, ended ] → { | r }` — the run's two occurrences, the shape
--         `PUI.action`'s progress slot dispatches).
--   * **oculars** — shape-preserving decorators (`card`, `dialog`, `menu`,
--     `chipSet`, `list`/`listItem`, `dataTable`/`dataRow`/`dataCell`,
--     `imageList`, `layoutGrid`/`layoutCell`, `topAppBar`, `drawer`,
--     `tooltip`, the MD3 typescale — `displayLarge` ... `labelSmall` —
--     and elevations): no model of their own, any polarity.
--   * plus **announcing statics** (`{}`-output chrome with a face, at any row):
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

-- | The **filled button** — Material 3's high-emphasis action and the
-- | default choice; `elevatedButton`, `tonalButton`, `outlinedButton` and
-- | `textButton` are the same button at the four lower emphasis levels, for
-- | the actions beside it.
-- |
-- | It reports on click, carrying the data it was showing, under the name
-- | the app gives the action: `button @"Book the flight"`, the case label
-- | verbatim as its caption, so the case *is* the copy. `button'`
-- | takes the face's options.
button
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => PUI Web { | r } [ | v ]
button = buttonOf @l "md-filled-button" {}

-- | `button` with options for its face: `icon: "add"` puts a Material
-- | Symbols glyph before the label (`button' @"Place order" { icon:
-- | "local_cafe" }`), `label:` overrides the caption with copy the case
-- | cannot be.
button'
  :: forall @l provided r v
   . IsSymbol l
  => Cons l { | r } () v
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => { | provided }
  -> PUI Web { | r } [ | v ]
button' = buttonOf @l "md-filled-button"

-- | `button` **elevated**: a shadow lifts it off the surface — for an
-- | important action over a busy or patterned background, where a flat fill
-- | would not separate.
elevatedButton
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => PUI Web { | r } [ | v ]
elevatedButton = buttonOf @l "md-elevated-button" {}

-- | `button` **tonal**: a softer fill, one step below filled — the second
-- | action next to a filled one (Save beside Publish).
tonalButton
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => PUI Web { | r } [ | v ]
tonalButton = buttonOf @l "md-filled-tonal-button" {}

-- | `button` **outlined**: a border and no fill — an important action that
-- | is not *the* action of the screen.
outlinedButton
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => PUI Web { | r } [ | v ]
outlinedButton = buttonOf @l "md-outlined-button" {}

-- | `button` at the lowest emphasis — label only: the dismissive or
-- | tertiary action (Cancel, Learn more), and what belongs in dialogs and
-- | cards.
textButton
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => PUI Web { | r } [ | v ]
textButton = buttonOf @l "md-text-button" {}

buttonOf
  :: forall @l provided r v
   . IsSymbol l
  => Cons l { | r } () v
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String } { | provided } { label :: Maybe String, icon :: Maybe String }
  => String
  -> { | provided }
  -> PUI Web { | r } [ | v ]
buttonOf tag provided = eventLeaf @l $ el tag $ Semigroupoid.do
  case config.icon of
    Just icon' -> el "md-icon" >>> "slot" := "icon" $ staticText icon'
    Nothing -> identity
  case config.label of
    Just label' -> staticText label'
    Nothing -> identity
  where
  config = convertOptionsWithDefaults OptLabelIcon { label: Just (reflectSymbol (Proxy @l)), icon: Nothing } provided :: { label :: Maybe String, icon :: Maybe String }

-- | The **floating action button**: the one action a screen is *for*, kept
-- | in view above the content. Reports on click carrying what it was
-- | showing, like `button`. The `icon` is required — a FAB is recognised by
-- | its glyph; the `label` — the extended FAB's words beside the glyph —
-- | defaults to the case label verbatim (`label: Nothing` gives
-- | the icon-only FAB, `label:` copy overrides).
fab
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => String
  -> PUI Web { | r } [ | v ]
fab icon = eventLeaf @l $
  el "md-fab" >>> "aria-label" := fromMaybe icon config.label >>> extended $
    el "md-icon" >>> "slot" := "icon" $ staticText icon
  where
  config = { label: Just (reflectSymbol (Proxy @l)) } :: { label :: Maybe String }
  extended = case config.label of
    Just label' -> "label" := label'
    Nothing -> identity

-- | A compact **icon-only action**, for toolbars, list rows and card
-- | corners where a labelled button would not fit. `label` is not drawn —
-- | it is what assistive technology announces, defaulting to
-- | the case label verbatim. For an icon that stays pressed
-- | (favourite, mute), use `iconToggle` instead.
iconButton :: forall @l r v. IsSymbol l => Cons l { | r } () v => String -> PUI Web { | r } [ | v ]
iconButton icon = eventLeaf @l $
  el "md-icon-button" >>> "aria-label" := config.label $
    el "md-icon" $ staticText icon
  where
  config = { label: reflectSymbol (Proxy @l) } :: { label :: String }

-- | One choice in a `menu`: reports the data it was showing when picked,
-- | and the menu closes itself. The line's text defaults to
-- | the case label verbatim (`label:` overrides with real copy).
menuItem :: forall @l r v. IsSymbol l => Cons l { | r } () v => PUI Web { | r } [ | v ]
menuItem = eventLeaf @l $
  el "md-menu-item" $
    div >>> "slot" := "headline" $ staticText config.label
  where
  config = { label: reflectSymbol (Proxy @l) } :: { label :: String }

-- | The **filled text field** — Material's default single-line input.
-- | `floatingLabel` names the field and rises above the text once there is
-- | any, so the label is never lost while the field is filled in.
-- |
-- | Shows the string it is given and reports each edit; typing is never
-- | interrupted by values arriving from elsewhere. A whole-row citizen:
-- | fed the wide row, it edits field `l` and carries the rest.
filledTextField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
filledTextField = let config = { floatingLabel: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "md-filled-text-field" Nothing config.floatingLabel)

-- | `filledTextField` in Material's outlined variant — a border instead of
-- | a fill. Same behaviour; pick one variant and keep to it across a form.
outlinedTextField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
outlinedTextField = let config = { floatingLabel: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "md-outlined-text-field" Nothing config.floatingLabel)

-- | `filledTextField` that waits `ms` after the last keystroke before
-- | reporting — for a field that drives expensive work (a search, a
-- | recomputed preview) and should not fire once per character.
debouncedTextField :: forall @l r b. IsSymbol l => Cons l String b r => { ms :: Number } -> PUI Web { | r } { | r }
debouncedTextField settleTime = let config = { floatingLabel: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (textFieldLeaf "md-filled-text-field" (if isHole settleTime then Nothing else Just settleTime.ms) config.floatingLabel)

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
  liftEffect $ editorWiring
    { show: setValue node
    , held: isFocused node
    , reports: case mDebounce of
        Nothing -> listenNode node "input"
        Just millis -> \report -> onInputDebounced node millis (const report)
    , read: \_ -> getValue node
    }

-- | The **multi-line text field**, sized in `rows` and `columns` of text (4 by 40 unless given) —
-- | a note, a description, a message. Otherwise `filledTextField`: the label
-- | floats (`floatingLabel` overrides it for real copy), shows a string,
-- | reports each edit, never interrupts typing.
filledTextArea :: forall @l r b provided. IsSymbol l => Cons l String b r => ConvertOptionsWithDefaults OptCaption { floatingLabel :: String, columns :: Int, rows :: Int } { | provided } { floatingLabel :: String, columns :: Int, rows :: Int } => { | provided } -> PUI Web { | r } { | r }
filledTextArea provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l), columns: 40, rows: 4 } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ wrap do
  element "md-filled-text-field" (pure unit)
  attribute "type" "textarea"
  attribute "label" config.floatingLabel
  attribute "rows" (show config.rows)
  -- `columns` is the preferred width; the card (or any container) wins on a
  -- narrow viewport — a leaf never escapes its surface
  attribute "style" ("width: " <> show config.columns <> "ch; max-width: 100%;")
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: setValue node
    , held: isFocused node
    , reports: listenNode node "input"
    , read: \_ -> getValue node
    }

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
checkbox = focusField @l $ "name" := reflectSymbol (Proxy @l) $
  label >>> "style" := "display: inline-flex; align-items: center; gap: 12px;" $ wrap do
    aRef <- liftEffect $ Ref.new {}
    element "md-checkbox" (pure unit)
    node <- gets _.sibling
    lbl <- unwrap (staticText (reflectSymbol (Proxy @l)))
    -- the wrapping <label> names the host but not the input inside its shadow
    -- root, so the caption is stamped as the box's accessible name
    liftEffect $ setAttribute node "aria-label" (reflectSymbol (Proxy @l))
    liftEffect $ editorWiring
      { show: \ma -> do
          lbl.toUser {}
          case Variant.prj (Proxy @c) ma of
            Nothing -> setChecked node false
            Just newa -> do
              setChecked node true
              Ref.write newa aRef
      , held: pure false
      , reports: listenNode node "change"
      , read: \_ -> do
          checked <- getChecked node
          a <- Ref.read aRef
          pure (if checked then Variant.inj (Proxy @c) a else Variant.inj (Proxy @n) {})
      }

-- | The Material **radio group**: one choice among a handful, every option
-- | visible and comparable at a glance. Beyond about five options, or where
-- | the options don't deserve the space, use `select`.
-- |
-- | The field holds the option itself, so the model always has one.
-- | Two siblings hold a variant instead: `radioButtonUnpicked @l @c` for a
-- | choice owed but not yet made, `radioButtonOptional @l @c @n` for one
-- | the user may leave unmade. Every one is an editor: every feed is
-- | answered with the row, every pick stored.
-- | The options — the value and the words shown for it — belong to the
-- | control, not to the model.
radioButton :: forall @l a b r. IsSymbol l => Cons l a b r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
radioButton options = withStructuralEq @a (radioButtonWith @l false (selectedAt @l) options)

-- | `radioButton` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
radioButtonUnpicked :: forall @l @c a b1 v b2 r. IsSymbol l => IsSymbol c => Cons c a b1 v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
radioButtonUnpicked options = withStructuralEq @a (radioButtonWith @l false (selectedUnpickedAt @l @c) options)

-- | `radioButton` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | pressing the checked option again clears it, storing `n` again.
radioButtonOptional :: forall @l @c @n a b1 t v b2 r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} t v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
radioButtonOptional options = withStructuralEq @a (radioButtonWith @l true (selectedOptionalAt @l @c @n) options)

radioButtonWith :: forall @l a i o. IsSymbol l => Eq a => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> Array { value :: a, label :: String } -> PUI Web i o
radioButtonWith clearable lift options = lift $ "name" := reflectSymbol (Proxy @l) $ (radioLeaf clearable options)

radioLeaf :: forall a. Eq a => Boolean -> Array { value :: a, label :: String } -> PUI Web (Maybe a) (Maybe a)
radioLeaf clearable options =
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
      labelNode <- gets _.sibling
      pure { press: labelNode, click: member.radioNode, radioNode: member.radioNode, value: member.value }
    mPropRef <- liftEffect $ Ref.new Nothing
    selRef <- liftEffect $ Ref.new Nothing
    let
      render ma = do
        Ref.write ma selRef
        for_ members \m -> setChecked m.radioNode (Just m.value == ma)
      emit ma = Ref.read mPropRef >>= traverse_ (_ $ ma)
    liftEffect $ for_ members \m -> listenNode m.radioNode "change" do
      checked <- getChecked m.radioNode
      sel <- Ref.read selRef
      when (checked && sel /= Just m.value) do
        Ref.write (Just m.value) selRef
        emit (Just m.value)
    liftEffect $ when clearable $ clearedOnRepress selRef members (render Nothing *> emit Nothing)
    pure
      { toUser: \ma -> do
          render ma
      , fromUser: \prop -> Ref.write (Just prop) mPropRef
      }

-- | The Material **switch**: a setting that takes effect the moment it is
-- | flipped — notifications on, dark mode on. (A `checkbox` states a fact
-- | to be submitted with the rest of a form; a switch acts immediately.)
toggleSwitch :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
toggleSwitch = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (switchLeaf config.label)

switchLeaf :: String -> PUI Web Boolean Boolean
switchLeaf lbl =
  label >>> "style" := "display: inline-flex; align-items: center; gap: 12px;" $ wrap do
    element "md-switch" (pure unit)
    node <- gets _.sibling
    liftEffect $ setAttribute node "aria-label" lbl
    _ <- unwrap (staticText lbl)
    liftEffect $ editorWiring
      { show: setBoolProp "selected" node
      , held: pure false
      , reports: listenNode node "change"
      , read: \_ -> getBoolProp "selected" node
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
slider :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
slider = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (sliderLeaf false config.label)

-- | `slider` reporting continuously while the thumb moves — for a live
-- | readout or preview that has to follow the drag. Whatever it drives
-- | should be cheap to redo; a drag that should land in the history as one
-- | change needs the plain `slider`, or a `debounced` stage downstream.
sliderLive :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
sliderLive = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (sliderLeaf true config.label)

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
  liftEffect $ editorWiring
    { show: \q -> do
        setNumberProp "min" node q.min
        setNumberProp "max" node q.max
        Variant.match { discrete: \s -> setNumberProp "step" node s, continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "value" node q.current
    , held: pure false
    , reports: \report -> do
        when live $ listenNode node "input" report
        listenNode node "change" report
    , read: \q -> getNumberProp "value" node <#> \v -> q { current = v }
    }

-- | The Material **filled select** (exposed dropdown): one choice out of a
-- | list too long to lay out in the open. `floatingLabel` names the field
-- | and stays visible above the choice once one is made. For a handful of
-- | options worth comparing side by side, prefer `radioButton` or
-- | `segmentedButton`.
-- |
-- | Same selection contract as `radioButton`, with the same two siblings
-- | (`selectUnpicked`, `selectOptional`); the options are part of the
-- | control, not of the model.
select :: forall @l a b r. IsSymbol l => Cons l a b r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
select options = withStructuralEq @a (selectOf @l false (selectedAt @l) options)

-- | `select` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
selectUnpicked :: forall @l @c a b1 v b2 r. IsSymbol l => IsSymbol c => Cons c a b1 v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectUnpicked options = withStructuralEq @a (selectOf @l false (selectedUnpickedAt @l @c) options)

-- | `select` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | an empty first option clears it, storing `n` again.
selectOptional :: forall @l @c @n a b1 t v b2 r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} t v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
selectOptional options = withStructuralEq @a (selectOf @l true (selectedOptionalAt @l @c @n) options)

selectOf :: forall @l a i o. IsSymbol l => Eq a => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> Array { value :: a, label :: String } -> PUI Web i o
selectOf clearable lift options = let config = { floatingLabel: reflectSymbol (Proxy @l) } in lift $ "name" := reflectSymbol (Proxy @l) $ (selectLeaf clearable config options)

selectLeaf :: forall a. Eq a => Boolean -> { floatingLabel :: String } -> Array { value :: a, label :: String } -> PUI Web (Maybe a) (Maybe a)
selectLeaf clearable config options = wrap do
  _ <- unwrap (staticHTML markup)
  node <- gets _.sibling
  -- programmatic selection can fire change too; the wiring's busy guard
  -- drops it
  liftEffect $ selectorWiring options
    { show: setIntProp "selectedIndex" node <<< maybe (-1) (_ + offset)
    , reports: listenNode node "change"
    , read: getIntProp "selectedIndex" node <#> \idx -> Just (idx - offset)
    }
  where
  offset = if clearable then 1 else 0
  markup =
    "<md-filled-select label=\"" <> escaped config.floatingLabel <> "\" style=\"min-width: 200px;\">"
      <> (if clearable then "<md-select-option value=\"\"></md-select-option>" else "")
      <> foldMapWithIndex optionMarkup options
      <> "</md-filled-select>"
  optionMarkup idx o =
    "<md-select-option value=\"" <> show idx <> "\"><div slot=\"headline\">" <> escaped o.label <> "</div></md-select-option>"

-- | The Material **segmented button**: two to five options joined in one
-- | control, all visible, one selected — a filter row, a view switch, a
-- | size. Compact where a radio group would be airy and a dropdown would
-- | hide the alternatives. Same selection contract as `select`.
segmentedButton :: forall @l a b r. IsSymbol l => Cons l a b r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
segmentedButton options = withStructuralEq @a (segmentedButtonWith @l false (selectedAt @l) options)

-- | `segmentedButton` for a choice owed but not yet made: field `l` is a variant
-- | whose case `c` is the made choice, seeded at an unpicked case; nothing
-- | is checked until the user picks, and a pick cannot be taken back.
segmentedButtonUnpicked :: forall @l @c a b1 v b2 r. IsSymbol l => IsSymbol c => Cons c a b1 v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
segmentedButtonUnpicked options = withStructuralEq @a (segmentedButtonWith @l false (selectedUnpickedAt @l @c) options)

-- | `segmentedButton` for a choice the user may leave unmade: field `l` is a variant
-- | whose case `c` is the made choice and case `n` none, seeded at `n`;
-- | clicking the selected segment again clears it, storing `n` again.
segmentedButtonOptional :: forall @l @c @n a b1 t v b2 r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c a b1 v => Cons n {} t v => Cons l [ | v ] b2 r => Array { value :: a, label :: String } -> PUI Web { | r } { | r }
segmentedButtonOptional options = withStructuralEq @a (segmentedButtonWith @l true (selectedOptionalAt @l @c @n) options)

segmentedButtonWith :: forall @l a i o. IsSymbol l => Eq a => Boolean -> (PUI Web (Maybe a) (Maybe a) -> PUI Web i o) -> Array { value :: a, label :: String } -> PUI Web i o
segmentedButtonWith clearable lift options = lift $ "name" := reflectSymbol (Proxy @l) $ (segmentedLeaf clearable options)

segmentedLeaf :: forall a. Eq a => Boolean -> Array { value :: a, label :: String } -> PUI Web (Maybe a) (Maybe a)
segmentedLeaf clearable options =
  div >>> cl "md3-segmented-button" >>> "role" := "radiogroup" $ wrap do
    liftEffect $ ensureStyle "md3-segmented-button" segmentedButtonCss
    segments <- for options \o -> do
      _ <- unwrap (staticHTML ("<button class=\"md3-segmented-button__segment\" role=\"radio\" aria-checked=\"false\"><span class=\"md3-segmented-button__check\" aria-hidden=\"true\"></span><span>" <> escaped o.label <> "</span></button>"))
      node <- gets _.sibling
      pure { node, value: o.value }
    mPropRef <- liftEffect $ Ref.new Nothing
    selRef <- liftEffect $ Ref.new Nothing
    let render msel = do
          Ref.write msel selRef
          for_ segments \seg -> do
            setClassIf seg.node "md3-segmented-button__segment--selected" (Just seg.value == msel)
            setAttribute seg.node "aria-checked" (if Just seg.value == msel then "true" else "false")
    liftEffect $ for_ segments \seg -> listenNode seg.node "click" do
      sel <- Ref.read selRef
      let picked = if clearable && sel == Just seg.value then Nothing else Just seg.value
      render picked
      mProp <- Ref.read mPropRef
      for_ mProp \prop -> prop picked
    pure
      { toUser: \ma -> do
          render ma
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
filterChip :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
filterChip = let config = { label: reflectSymbol (Proxy @l) } in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (chipLeaf config.label)

chipLeaf :: String -> PUI Web Boolean Boolean
chipLeaf lbl = wrap do
  element "md-filter-chip" (pure unit)
  attribute "label" lbl
  node <- gets _.sibling
  -- the chip toggles `selected` as the click's default action; read it a
  -- microtask later, once the toggle has settled
  liftEffect $ editorWiring
    { show: setBoolProp "selected" node
    , held: pure false
    , reports: listenNodeDeferred node "click"
    , read: \_ -> getBoolProp "selected" node
    }

-- | An **icon that stays pressed** — favourite, bookmark, mute, pin:
-- | `onIcon` is shown while it is on, `offIcon` while it is off, and
-- | `label` is what assistive technology announces. The on glyph renders
-- | filled, so the same glyph in both slots still reads as off and on. The
-- | compact form of a `toggleSwitch`, for list rows and toolbars.
iconToggle :: forall @l r b provided. IsSymbol l => Cons l Boolean b r => ConvertOptionsWithDefaults OptCaption { label :: String } { | provided } { onIcon :: String, offIcon :: String, label :: String } => { | provided } -> PUI Web { | r } { | r }
iconToggle provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in focusField @l $ "name" := reflectSymbol (Proxy @l) $ (iconToggleLeaf config)

iconToggleLeaf :: { onIcon :: String, offIcon :: String, label :: String } -> PUI Web Boolean Boolean
iconToggleLeaf config = wrap do
  -- the selected icon renders filled (the MD3 selected-state convention),
  -- so a same-glyph pair still reads as off/on
  _ <- unwrap $ el "md-icon-button" >>> "toggle" := "" >>> "aria-label" := config.label $ Semigroupoid.do
    el "md-icon" $ staticText config.offIcon
    el "md-icon" >>> "slot" := "selected" >>> "style" := "font-variation-settings: 'FILL' 1;" $ staticText config.onIcon
  node <- gets _.sibling
  liftEffect $ editorWiring
    { show: setBoolProp "selected" node
    , held: pure false
    , reports: listenNode node "change"
    , read: \_ -> getBoolProp "selected" node
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
tabBar :: forall @l provided a r b . IsSymbol l => Cons l a b r => ConvertOptionsWithDefaults OptIcon { icon :: Maybe String } { | provided } { value :: a, label :: String, icon :: Maybe String } => Array { | provided } -> PUI Web { | r } { | r }
tabBar options = withStructuralEq @a (focusField @l $ "name" := reflectSymbol (Proxy @l) $ (tabBarLeaf (convertOptionsWithDefaults OptIcon { icon: Nothing } <$> options)))

tabBarLeaf :: forall a. Eq a => Array { value :: a, label :: String, icon :: Maybe String } -> PUI Web a a
tabBarLeaf options = wrap do
  _ <- unwrap (staticHTML ("<md-tabs>" <> foldMap tabMarkup options <> "</md-tabs>"))
  node <- gets _.sibling
  -- programmatic activation fires change too; the wiring's busy guard drops it
  liftEffect $ editorWiring
    { show: \a -> for_ (findIndex (\o -> o.value == a) options) (setIntProp "activeTabIndex" node)
    , held: pure false
    , reports: listenNode node "change"
    , read: \a -> getIntProp "activeTabIndex" node <#> \idx -> maybe a _.value (options !! idx)
    }
  where
  tabMarkup o =
    "<md-primary-tab>"
      <> maybe "" (\icon' -> "<md-icon slot=\"icon\">" <> escaped icon' <> "</md-icon>") o.icon
      <> escaped o.label
      <> "</md-primary-tab>"

-- | The **indeterminate progress bar**: work is under way and there is no
-- | telling how long — a request in flight, a file being processed. A
-- | **status**, not a display: it is fed the run's two occurrences,
-- | `started` and `ended`, and shows between them — exactly what
-- | `PUI.action`'s slot dispatches (`indeterminateLinearProgress # action
-- | submit`). No model owns a "busy" field, so nothing here is state, and
-- | it carries no label: it names no field and no case, and a caption is
-- | copy, which is a function, not a label.
indeterminateLinearProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateLinearProgress = wrap do
  element "md-linear-progress" (pure unit)
  attribute "indeterminate" ""
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ statusWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "min-width: 200px;"
  hiddenStyle = "min-width: 200px; visibility: hidden;"

-- | The **determinate progress bar**: how far along something is, `value`
-- | running 0 to 1. As much a gauge as a progress indicator — a quiz's
-- | position, a budget's use, a quota — written as
-- | `linearProgress progressFraction`: the value is a **read
-- | function** like every display's, since a fraction is derived (a ratio
-- | of source fields), not state.
linearProgress
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
linearProgress f = wrap do
  element "md-linear-progress" (pure unit)
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  liftEffect $ displayWiring \r -> setNumberProp "value" node (f r)

-- | The **spinner** — `indeterminateLinearProgress` in circular form, for
-- | inline and compact places (a button, a card corner) where a bar across
-- | the width would be too much.
indeterminateCircularProgress :: forall r. PUI Web [ started :: {}, ended :: {} ] { | r }
indeterminateCircularProgress = wrap do
  element "md-circular-progress" (pure unit)
  attribute "indeterminate" ""
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ statusWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
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
group w = wrap do
  headingId <- liftEffect uniqueId
  unwrap $ "role" := "group" $ "aria-labelledby" := headingId $ card $
    ("id" := headingId $ titleMedium $ staticText (reflectSymbol (Proxy @l))) >>> focusField @l w

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
dialog title content =
  el "md-dialog" >>> init pure showDialog closeDialog $ wrap do
    _ <- unwrap (div >>> "slot" := "headline" $ staticText title)
    unwrap (div >>> "slot" := "content" $ content)

-- | The witness rung — see `PUI.Web.MDC2.confirmed`.
confirmed :: forall row. String -> String -> PUI Web { | row } { | row } -> PUI Web { | row } { | row }
confirmed confirmCaption title content = simpleDialog confirmCaption title content

-- | `dialog` with a **confirm button** built in — the confirmation step:
-- | show what is about to happen, and the button reports it. The content
-- | needs no button of its own: a display releases the row it shows, so
-- | there is something to confirm — `confirmed` is exactly that
-- | specialization.
-- | Not a full `Ocular`, deliberately: the confirm **replays** the
-- | content's last output, and replay is lawful over **records** only —
-- | an entity's last value may be re-said, a one-shot event may not (the
-- | `looped` argument) — so the content's output is row-shaped.
simpleDialog :: forall i o. String -> String -> PUI Web { | i } { | o } -> PUI Web { | i } { | o }
simpleDialog confirmCaption title content =
  el "md-dialog" >>> init pure showDialog closeDialog $ Semigroupoid.do
    wrap do
      _ <- unwrap (div >>> "slot" := "headline" $ staticText title)
      unwrap (div >>> "slot" := "content" $ content)
    div >>> "slot" := "actions" $ witnessed (el "md-text-button" $ staticText confirmCaption)

-- | The **snackbar**: a brief message at the bottom of the screen that
-- | dismisses itself after a few seconds, for something that has just
-- | happened and needs no reply ("Order placed"). It never interrupts — for
-- | something the user must acknowledge, use a `dialog`.
-- |
-- | A status **tells its case**: its caption is the case label verbatim,
-- | so the outcome is named as what it tells (`snackbar @"Order placed"
-- | (const usualOrder)`) and the payload is read by nothing but the fold. Mutually
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
snackbar f = snackbarFace (reflectSymbol (Proxy @l)) # atCase @l # folding @l f

snackbarFace :: forall a r. String -> PUI Web a { | r }
snackbarFace caseLabel = wrap do
  liftEffect $ ensureStyle "md3-snackbar" snackbarCss
  w <- unwrap $ div >>> cl "md3-snackbar" >>> "role" := "status" $
    statusText caseLabel
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
menu :: String -> Ocular (PUI Web)
menu anchorCaption content =
  span >>> "style" := "position: relative; display: inline-block;" $ wrap do
    _ <- unwrap (staticHTML ("<md-outlined-button trailing-icon>" <> escaped anchorCaption <> "<md-icon slot=\"icon\">arrow_drop_down</md-icon></md-outlined-button>"))
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
-- | row is a visible type argument after the key, so the view can declare
-- | the view model each row is drawn from
-- | (`listOf @"opened" @"id" @( id :: Int, subject :: String ) …`).
listOf
  :: forall @l @k @r i b o key v
   . IsSymbol l
  => IsSymbol k
  => Cons l key () v
  => Cons k key b r
  => ({ | i } -> Array { | r })
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOf = listOf' @l @k @r {}


-- | `listOf` with options: `selected` takes a predicate on a row, and the
-- | rows it holds take the catalogue's selected styling (`listOf'
-- | @"Person picked" @"key" @( … ) { selected: isSelected } entries item`).
listOf'
  :: forall @l @k @r provided i b o key v
   . IsSymbol l
  => IsSymbol k
  => Cons l key () v
  => Cons k key b r
  => ConvertOptionsWithDefaults OptSelected { selected :: { | r } -> Boolean } { | provided } { selected :: { | r } -> Boolean }
  => { | provided }
  -> ({ | i } -> Array { | r })
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOf' provided f item = withStructuralOrd @key $ wrap do
  liftEffect $ ensureStyle "md3-list" listCss
  unwrap $ el "md-list" >>> "style" := "overflow-y: auto;" $
    ( clicked @l @k $ clWhen config.selected "md3-list-item--selected"
        $ el "md-list-item" >>> "type" := "button" $ item
    ) # anyOf @k f
  where
  config = convertOptionsWithDefaults OptSelected { selected: const false } provided

-- | `listOf` over an array the model stores: the rows are field `t` of the
-- | fed row, named by its label after the key
-- | (`listOfAt @"Message opened" @"id" @"messages" item`). Rows derived
-- | from the row are a projection, `listOf`.
listOfAt
  :: forall @l @k @t i r b1 b2 o key v
   . IsSymbol l
  => IsSymbol k
  => IsSymbol t
  => Cons l key () v
  => Cons k key b1 r
  => Cons t (Array { | r }) b2 i
  => PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOfAt = listOfAt' @l @k @t {}

-- | `listOfAt` with options: `selected` takes a predicate on a row, and the
-- | rows it holds take the catalogue's selected styling
-- | (`listOfAt' @"Message opened" @"id" @"messages" { selected:
-- | highlighted } item`).
listOfAt'
  :: forall @l @k @t provided i r b1 b2 o key v
   . IsSymbol l
  => IsSymbol k
  => IsSymbol t
  => Cons l key () v
  => Cons k key b1 r
  => Cons t (Array { | r }) b2 i
  => ConvertOptionsWithDefaults OptSelected { selected :: { | r } -> Boolean } { | provided } { selected :: { | r } -> Boolean }
  => { | provided }
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOfAt' provided = listOf' @l @k @r provided (Record.get (Proxy @t))

listCss :: String
listCss = """
md-list-item.md3-list-item--selected { --md-list-item-container-color: var(--md-sys-color-secondary-container, #e8def8); }
"""

-- | A **data table**: values in rows and columns, where the column a value
-- | sits in is what says what it means. Its header slot holds the fixed
-- | headings, one `columnHeader "…"` each, and its first argument is what
-- | assistive technology announces the table as (`dataTable "Cart"`); the
-- | body is `dataRow`s of `dataCell`s, usually one row per element of a
-- | collection.
dataTable :: String -> PUI Web {} {} -> Ocular (PUI Web)
dataTable tableName headerCells content = wrap do
  liftEffect $ ensureStyle "md3-data-table" dataTableCss
  unwrap $ div >>> cl "md3-data-table" $
    table >>> "aria-label" := tableName $ wrap do
      _ <- unwrap (thead $ tr $ headerCells)
      unwrap (tbody $ content)

-- | One **column header** of a `dataTable` — static copy naming the column,
-- | its text a value like every static: `columnHeader "Qty"`.
columnHeader :: forall r. String -> PUI Web { | r } { | r }
columnHeader heading = th >>> "role" := "columnheader" >>> "scope" := "col" $ staticText heading

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
imageList :: Int -> Ocular (PUI Web)
imageList columns content = wrap do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  unwrap $ el "ul" >>> cl "md3-image-list" >>> "style" := ("column-count: " <> show columns <> "; column-gap: 16px; margin: 0;") $ content

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
layoutCell :: Int -> Ocular (PUI Web)
layoutCell cellSpan = div >>> "style" := ("grid-column: span " <> show cellSpan <> ";")

-- | The **top app bar**: the band carrying the screen's title, with the
-- | content laid out beneath it.
topAppBar :: String -> Ocular (PUI Web)
topAppBar title content = wrap do
  liftEffect $ ensureStyle "md3-top-app-bar" topAppBarCss
  _ <- unwrap (staticHTML ("<header class=\"md3-top-app-bar\"><span class=\"md3-top-app-bar__title\">" <> escaped title <> "</span></header>"))
  unwrap (div >>> cl "md3-top-app-bar-content" $ content)

topAppBarCss :: String
topAppBarCss = """
.md3-top-app-bar { display: flex; align-items: center; height: 64px; padding: 0 16px; background: var(--md-sys-color-surface-container, #f3edf7); color: var(--md-sys-color-on-surface, #1d1b20); font: 400 22px/28px Roboto, sans-serif; }
.md3-top-app-bar-content { padding: 16px; }
"""

-- | The permanent **navigation drawer**: a titled nav panel pinned beside
-- | the content, both fed what the drawer is fed. The nav is live, not a
-- | static menu — it is the stage, so what it emits is the drawer's
-- | output: a selectable nav (a `listOf` of sections, `# joined`) reports
-- | its pick to the app's loop, which folds it into the model and feeds
-- | the drawer again, and the content shows that model beside it
-- | (photo-gallery).
drawer :: forall r o. { title :: String, subtitle :: String } -> PUI Web { | r } o -> PUI Web { | r } { | r } -> PUI Web { | r } o
drawer config nav content = div >>> "style" := "display: flex;" $ wrap do
  liftEffect $ ensureStyle "md3-drawer" drawerCss
  nav' <- unwrap $ aside >>> cl "md3-drawer" $ wrap do
    _ <- unwrap (staticHTML ("<div class=\"md3-drawer__header\"><h3 class=\"md3-drawer__title\">" <> escaped config.title <> "</h3><h6 class=\"md3-drawer__subtitle\">" <> escaped config.subtitle <> "</h6></div>"))
    unwrap (div $ nav)
  content' <- unwrap (div >>> "style" := "flex: 1; padding: 16px;" $ content)
  -- the content is a display: its echoes are the row the nav already has
  liftEffect $ content'.fromUser \_ -> pure unit
  pure { toUser: \r -> nav'.toUser r *> content'.toUser r, fromUser: nav'.fromUser }

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
-- | screen. The tip is static: its text is fixed when the control is
-- | built, never read from data. Wrap a single control, and write it
-- | trailing so the control still reads first:
-- | `checkbox @"Loyalty member" @"member" @"guest" # tooltip "Members get 10% off"`.
tooltip :: String -> Ocular (PUI Web)
tooltip tipText content =
  span >>> cl "md3-tooltip-anchor" $ wrap do
    liftEffect $ ensureStyle "md3-tooltip" tooltipCss
    tipId <- liftEffect uniqueId
    w <- unwrap ("aria-describedby" := tipId $ content)
    _ <- unwrap (staticHTML ("<div id=\"" <> tipId <> "\" class=\"md3-tooltip\" role=\"tooltip\">" <> escaped tipText <> "</div>"))
    pure w


-- a plain tooltip is transient: it shows while the pointer rests on the anchor
-- or the anchor has KEYBOARD focus (`:focus-visible`), never for the focus a
-- click leaves behind — `:focus-within` kept it up after every tap
tooltipCss :: String
tooltipCss = """
.md3-tooltip-anchor { position: relative; display: inline-block; }
.md3-tooltip { position: absolute; top: 100%; left: 50%; transform: translateX(-50%); margin-top: 4px; background: var(--md-sys-color-inverse-surface, #322f35); color: var(--md-sys-color-inverse-on-surface, #f5eff7); font: 400 12px/16px Roboto, sans-serif; padding: 4px 8px; border-radius: 4px; white-space: nowrap; visibility: hidden; opacity: 0; transition: opacity .15s; z-index: 1000; pointer-events: none; }
.md3-tooltip-anchor:hover .md3-tooltip, .md3-tooltip-anchor:has(:focus-visible) .md3-tooltip { visibility: visible; opacity: 1; }
"""

-- announcing statics (`{}`-output chrome with a face, at any row)

-- | A **divider**: the hairline rule between list rows or card sections,
-- | for separating groups that belong to the same surface. Fixed
-- | decoration, carrying no data.
dividerExact :: PUI Web {} {}
dividerExact = staticHTML "<md-divider style=\"width: 100%;\"></md-divider>"

divider :: Ocular (PUI Web)
divider = separating dividerExact

-- | One picture in an `imageList`, with `label` shown as its caption and
-- | used as its alternative text.
imageListItem :: forall r. { src :: String, alt :: String } -> PUI Web { | r } { | r }
imageListItem config = wrap do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  unwrap $ staticHTML $
    "<li class=\"md3-image-list__item\">"
      <> "<img class=\"md3-image-list__image\" src=\"" <> escaped config.src <> "\" alt=\"" <> escaped config.alt <> "\">"
      <> "<span class=\"md3-image-list__label\">" <> escaped config.alt <> "</span>"
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
imagePaneFace picture = wrap do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  unwrap $ el "li" >>> cl "md3-image-list__item" $ Semigroupoid.do
    img >>> cl "md3-image-list__image" >>> attrWith "src" (_.src <<< picture) >>> attrWith "alt" (_.alt <<< picture) $ identity
    span >>> cl "md3-image-list__label" $ text (_.alt <<< picture)

-- the element adapter for the index-keyed internal collection: reads the
-- item out of the reconciler's { ix, item } row at the wiring level (the
-- closed-singleton adopters deliberately do not read from wider rows)

-- Private

foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import setIntProp :: String -> Node -> Int -> Effect Unit
foreign import getIntProp :: String -> Node -> Effect Int
foreign import setBoolProp :: String -> Node -> Boolean -> Effect Unit
foreign import getBoolProp :: String -> Node -> Effect Boolean
foreign import setClassIf :: Node -> String -> Boolean -> Effect Unit
foreign import showDialog :: Node -> Effect Unit
foreign import closeDialog :: Node -> Effect Unit
foreign import openMenuAnchoredTo :: Node -> Node -> Effect Unit

-- Entry point

-- | Mount the app in the page's `<body>`, dressed for Material 3: the MD3
-- | typescale stylesheet — what gives the `md-typescale-*` classes the
-- | typography oculars wear their meaning — is adopted by the document
-- | here, once, and the app then mounts exactly as `PUI.Web.HTML.body`
-- | does — same signature, same closed-app demand (`PUI Web {} o`). The
-- | entry is a word of the vocabulary like any other, so a screen changes
-- | design system at its first line by changing the import, and nothing
-- | runs at import time.
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptTypescale
  HTML.body ui

foreign import adoptTypescale :: Effect Unit
