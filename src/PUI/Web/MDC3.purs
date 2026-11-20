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
-- | and sets the page's base typeface to the type scale's plain face before
-- | mounting; nothing happens at import time. Every entry hand-rolled here
-- | sets its text in a typescale role (`.md-typescale-*`), never a font of its own.
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
-- |     `fab`; `iconButton`; `menu`/`menuItem`; `tile` (and `doubleClickTile`), one clickable cell
-- |     of a grid
-- |   * **be told something** — `snackbar` (passing),
-- |     `dialog`/`confirmed`/`modal` (must be answered), `linearProgress`,
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
  , rowHeader
  , dataTable
  , dataTable'
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
  , filledNumberField
  , filledIntegerField
  , filledTextField
  , filledTextField'
  , filterChip
  , group
  , headlineLarge
  , headlineMedium
  , headlineSmall
  , iconButton
  , iconButton'
  , icon
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
  , outlinedNumberField
  , outlinedIntegerField
  , outlinedTextField
  , outlinedTextField'
  , radioButton
  , radioButtonUnpicked
  , radioButtonOptional
  , segmentedButton
  , segmentedButtonUnpicked
  , segmentedButtonOptional
  , rangeSlider
  , rangeSlider'
  , rangeSliderLive
  , rangeSliderLive'
  , select
  , select'
  , selectUnpicked
  , selectOptional
  , confirmed
  , confirmed'
  , modal
  , slider
  , slider'
  , sliderLive
  , sliderLive'
  , snackbar
  , tabBar
  , radio
  , segment
  , option
  , tab
  , tab'
  , textButton
  , tile
  , doubleClickTile
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
import ConvertableOptions (class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number.Format (toString)
import Data.Profunctor.Row.RecordToRecord (focusField)
import Data.Profunctor.Row.VariantToVariant (focusCase)
import Data.Variant (inj, match, prj) as Variant
import Data.Variant.Case (caseText)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import PUI (integral, static, fromHandles, handles, gatheredOf, numeral, Ocular, PUI, recordToRecordWiring, variantToRecordWiring, variantToVariantWiring, replayed, occurrencesOn, Gesture(..), optionWiring, statusOver, confirmedOver, confirmedOver', modalOver, eventLeaf)
import PUI.Web.HTML (aside, div, h1, h2, h3, img, label, p, span, table, tbody, td, th, thead, tr)
import PUI.Web.HTML (body) as HTML
import PUI.Web (chrome, fieldLeaf, Node, OptCaption(..), OptLabelIcon(..), OptSelected(..), Web, attrWith, attribute, autoDismiss, cl, clWhen, el, element, ensureStyle, getChecked, getValue, init, isFocused, listenNode, listenNodeDeferred, onInputDebounced, removeAttribute, separating, setAttribute, setChecked, setValue, staticHTML, staticText, text, statusText, uniqueId, (:=), hasClass, listenCapture, grouping, member, unpickAll, clearedByRepress)
import QualifiedDo.Semigroupoid as Semigroupoid
import Prim.Row (class Cons)
import Data.Structural (withStructuralOrd)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Record as Record
import Type.Proxy (Proxy(..))

-- Implementation notes — the reference above is the contract.
--
-- Built on the `@material/web` custom elements (`<md-filled-button>`,
-- `<md-checkbox>`, …): importing the FFI module registers the tags, so a
-- component leaf is `element "md-..."` plus property/event wiring — no
-- foundation classes, no hand-fused ripple or label chrome. The sorts,
-- shapes and naming follow `PUI.Web.MDC2`'s implementation notes
-- (guardrails L3, L20); `divider` is the `<md-divider>`. The entries
-- `@material/web` does not implement (segmented button, snackbar, card,
-- top app bar, navigation drawer, data table, image list, tooltip) are
-- hand-rolled as minimal chrome over the `--md-sys-*` design tokens, each
-- injecting its stylesheet once via `ensureStyle`, so a page that themes
-- the tokens themes them too.
--
-- **The `dimap` round-trip contract for editors** holds as in
-- `PUI.Web.MDC2`.

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
-- | cannot be, and `disabled:` takes a predicate on the row — while it
-- | holds for the row last fed, the button is drawn disabled and reports
-- | no click (`button' @"Add" { icon: "add", disabled: isSoldOut }`).
-- | A decoration: `disabled: const false` is the button itself.
button'
  :: forall @l provided r v
   . IsSymbol l
  => Cons l { | r } () v
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String, disabled :: { | r } -> Boolean } { | provided } { label :: Maybe String, icon :: Maybe String, disabled :: { | r } -> Boolean }
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
  => ConvertOptionsWithDefaults OptLabelIcon { label :: Maybe String, icon :: Maybe String, disabled :: { | r } -> Boolean } { | provided } { label :: Maybe String, icon :: Maybe String, disabled :: { | r } -> Boolean }
  => String
  -> { | provided }
  -> PUI Web { | r } [ | v ]
buttonOf tag provided = disabledWhen config.disabled $ eventLeaf @l $ el tag $ Semigroupoid.do
  case config.icon of
    Just icon' -> el "md-icon" >>> "slot" := "icon" $ staticText icon'
    Nothing -> identity
  case config.label of
    Just label' -> staticText label'
    Nothing -> identity
  where
  config = convertOptionsWithDefaults OptLabelIcon { label: Just (reflectSymbol (Proxy @l)), icon: Nothing, disabled: const false } provided :: { label :: Maybe String, icon :: Maybe String, disabled :: { | r } -> Boolean }


-- The `disabled` attribute of the element just built, set while `holds`
-- is true of the row fed: a disabled control reports no click.
disabledWhen :: forall r a. ({ | r } -> Boolean) -> PUI Web { | r } a -> PUI Web { | r } a
disabledWhen holds w = fromHandles do
  w' <- handles w
  node <- gets _.sibling
  pure
    { toUser: \row -> do
        if holds row then setAttribute node "disabled" "" else removeAttribute node "disabled"
        w'.toUser row
    , fromUser: w'.fromUser
    }

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
fab glyph = eventLeaf @l $
  el "md-fab" >>> "aria-label" := fromMaybe glyph config.label >>> extended $
    el "md-icon" >>> "slot" := "icon" $ staticText glyph
  where
  config = { label: Just (reflectSymbol (Proxy @l)) } :: { label :: Maybe String }
  extended = case config.label of
    Just label' -> "label" := label'
    Nothing -> identity

-- | A **Material icon** as chrome: `icon "check_circle"` draws the named
-- | glyph inline, spaced from the copy after it, and reads and changes
-- | nothing — decoration a pane can show by case (`icon "check_circle" #
-- | inCaseAt @"plated" @"state"`, diner's kitchen). Hidden from assistive
-- | technology: what it marks is said by the copy beside it or by the
-- | row's own state.
icon :: forall r. String -> PUI Web { | r } { | r }
icon name = el "md-icon" >>> "aria-hidden" := "true" >>> "style" := "vertical-align: middle; margin-inline-end: 8px;" $ staticText name

-- | A compact **icon-only action**, for toolbars, list rows and card
-- | corners where a labelled button would not fit. `label` is not drawn —
-- | it is what assistive technology announces, defaulting to
-- | the case label verbatim. For an icon that stays pressed
-- | (favourite, mute), use `iconToggle` instead.
iconButton :: forall @l r v. IsSymbol l => Cons l { | r } () v => String -> PUI Web { | r } [ | v ]
iconButton glyph = eventLeaf @l $
  el "md-icon-button" >>> "aria-label" := config.label $
    el "md-icon" $ staticText glyph
  where
  config = { label: reflectSymbol (Proxy @l) } :: { label :: String }

-- | `iconButton` with options: `disabled:` takes a predicate on the row,
-- | as `button'`'s does — while it holds for the row last fed, the button
-- | is drawn disabled and reports no click (`iconButton' @"Add a portion"
-- | "add" { disabled: noneLeft }`, diner). `disabled: const false` is the
-- | button itself.
iconButton'
  :: forall @l provided r v
   . IsSymbol l
  => Cons l { | r } () v
  => ConvertOptionsWithDefaults OptLabelIcon { disabled :: { | r } -> Boolean } { | provided } { disabled :: { | r } -> Boolean }
  => String
  -> { | provided }
  -> PUI Web { | r } [ | v ]
iconButton' glyph provided = disabledWhen config.disabled (iconButton @l glyph)
  where
  config = convertOptionsWithDefaults OptLabelIcon { disabled: const false } provided :: { disabled :: { | r } -> Boolean }

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
filledTextField = let config = { floatingLabel: reflectSymbol (Proxy @l) } in fieldLeaf @l $ (textFieldLeaf "md-filled-text-field" Nothing config.floatingLabel "")

-- | `filledTextField` with a `hint` under it — a format or an example the
-- | label is not (`filledTextField' @"Start date" { hint: "DD.MM.YYYY" }`),
-- | so the label stays the field's name. Shown as MD3's supporting text.
filledTextField' :: forall @l r b. IsSymbol l => Cons l String b r => { hint :: String } -> PUI Web { | r } { | r }
filledTextField' { hint } = fieldLeaf @l $ (textFieldLeaf "md-filled-text-field" Nothing (reflectSymbol (Proxy @l)) hint)

-- | `filledTextField` in Material's outlined variant — a border instead of
-- | a fill. Same behaviour; pick one variant and keep to it across a form.
outlinedTextField :: forall @l r b. IsSymbol l => Cons l String b r => PUI Web { | r } { | r }
outlinedTextField = let config = { floatingLabel: reflectSymbol (Proxy @l) } in fieldLeaf @l $ (textFieldLeaf "md-outlined-text-field" Nothing config.floatingLabel "")

-- | `outlinedTextField` with a `hint` under it, as `filledTextField'`.
outlinedTextField' :: forall @l r b. IsSymbol l => Cons l String b r => { hint :: String } -> PUI Web { | r } { | r }
outlinedTextField' { hint } = fieldLeaf @l $ (textFieldLeaf "md-outlined-text-field" Nothing (reflectSymbol (Proxy @l)) hint)

-- | `filledTextField` read as a **number**: `filledNumberField @"Amount"
-- | @"number" @"invalid"` edits a field `[ number :: Number, invalid ::
-- | String ]` — the case names the view's, as `checkbox`'s are — holding
-- | the number when the text parses and the text itself when it does not,
-- | so an invalid entry is model state a pane shows (`# inCaseAt
-- | @"invalid" @"Amount"`) and a `provided` withholds the submit on, never
-- | a fact the DOM alone knows. `PUI.numeral` states the parse and what a
-- | parsed number keeps of its text.
filledNumberField :: forall @l @n @i r b v nr ir. IsSymbol l => IsSymbol n => IsSymbol i => Cons l [ | v ] b r => Cons n Number nr v => Cons i String ir v => PUI Web { | r } { | r }
filledNumberField = fieldLeaf @l $ numeral @n @i (textFieldLeaf "md-filled-text-field" Nothing (reflectSymbol (Proxy @l)) "")

-- | `outlinedTextField` read as a number, as `filledNumberField`.
outlinedNumberField :: forall @l @n @i r b v nr ir. IsSymbol l => IsSymbol n => IsSymbol i => Cons l [ | v ] b r => Cons n Number nr v => Cons i String ir v => PUI Web { | r } { | r }
outlinedNumberField = fieldLeaf @l $ numeral @n @i (textFieldLeaf "md-outlined-text-field" Nothing (reflectSymbol (Proxy @l)) "")

-- | `filledTextField` read as a **whole number**: `filledIntegerField
-- | @"Portions" @"count" @"invalid"` edits a field `[ count :: Int, invalid
-- | :: String ]`, as `filledNumberField` does a `Number`; a fraction is
-- | invalid text. `PUI.integral` states the parse.
filledIntegerField :: forall @l @n @i r b v nr ir. IsSymbol l => IsSymbol n => IsSymbol i => Cons l [ | v ] b r => Cons n Int nr v => Cons i String ir v => PUI Web { | r } { | r }
filledIntegerField = fieldLeaf @l $ integral @n @i (textFieldLeaf "md-filled-text-field" Nothing (reflectSymbol (Proxy @l)) "")

-- | `outlinedTextField` read as a whole number, as `filledIntegerField`.
outlinedIntegerField :: forall @l @n @i r b v nr ir. IsSymbol l => IsSymbol n => IsSymbol i => Cons l [ | v ] b r => Cons n Int nr v => Cons i String ir v => PUI Web { | r } { | r }
outlinedIntegerField = fieldLeaf @l $ integral @n @i (textFieldLeaf "md-outlined-text-field" Nothing (reflectSymbol (Proxy @l)) "")

-- | `filledTextField` that waits `ms` after the last keystroke before
-- | reporting — for a field that drives expensive work (a search, a
-- | recomputed preview) and should not fire once per character.
debouncedTextField :: forall @l r b. IsSymbol l => Cons l String b r => { ms :: Number } -> PUI Web { | r } { | r }
debouncedTextField settleTime = let config = { floatingLabel: reflectSymbol (Proxy @l) } in fieldLeaf @l $ (textFieldLeaf "md-filled-text-field" (Just settleTime.ms) config.floatingLabel "")

-- the raw MD3 text field — scalar, so private; the custom element carries
-- its own label/ripple chrome, so the leaf is property/event wiring only.
-- Focus-guarded like `Web.input`: model updates never clobber the field
-- being typed in (the element delegates focus, so the host is the
-- activeElement), but still echo so the channel stays live. Debouncing
-- sits at the DOM boundary (`Web.onInputDebounced`), in front of the wire
-- rather than on it, so the field stays loop-safe.
textFieldLeaf :: String -> Maybe Number -> String -> String -> PUI Web String String
textFieldLeaf tag mDebounce floatingLabel hint = fromHandles do
  element tag (pure unit)
  attribute "label" floatingLabel
  when (hint /= "") $ attribute "supporting-text" hint
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
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
filledTextArea provided = let config = convertOptionsWithDefaults OptCaption { floatingLabel: reflectSymbol (Proxy @l), columns: 40, rows: 4 } provided in fieldLeaf @l $ fromHandles do
  element "md-filled-text-field" (pure unit)
  attribute "type" "textarea"
  attribute "label" config.floatingLabel
  attribute "rows" (show config.rows)
  -- `columns` is the preferred width; the card (or any container) wins on a
  -- narrow viewport — a leaf never escapes its surface
  attribute "style" ("width: " <> show config.columns <> "ch; max-width: 100%;")
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
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
-- | `checkbox @"Terms" @"accepted" @"declined"` is ticked exactly while
-- | `"Terms"` sits at `accepted`; ticking reports `.accepted {}`, clearing
-- | reports `.declined {}`. Use a checkbox for a fact the user states as
-- | part of a form; use `toggleSwitch` for a setting that takes effect at
-- | once.
checkbox :: forall @l @c @n r b v cr nr. IsSymbol l => IsSymbol c => IsSymbol n => Cons l [ | v ] b r => Cons c {} cr v => Cons n {} nr v => PUI Web { | r } { | r }
checkbox = fieldLeaf @l $
  label >>> "style" := "display: inline-flex; align-items: center; gap: 12px;" $ fromHandles do
    aRef <- liftEffect $ Ref.new {}
    element "md-checkbox" (pure unit)
    node <- gets _.sibling
    lbl <- handles (staticText (reflectSymbol (Proxy @l)))
    -- the wrapping <label> names the host but not the input inside its shadow
    -- root, so the caption is stamped as the box's accessible name
    liftEffect $ setAttribute node "aria-label" (reflectSymbol (Proxy @l))
    liftEffect $ recordToRecordWiring
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
-- | The options are leaves, one per case of the field, merged
-- | `VariantToVariant.do` in the order they are shown:
-- | `radioButton @"Roast" $ VariantToVariant.do radio @"Light"; radio @"Dark"`.
-- | The block closes the field's cases; the group lifts it onto field `l`.
-- A selector group in three forms, over one clearing of its options:
-- every feed first shows nothing picked, then the one option owning the
-- fed case shows itself, so a case no option owns (an unpicked one) shows
-- nothing. The plain form's field holds the options' case; the `…Unpicked`
-- form's is a variant whose case `c` holds it, every other case unpicked;
-- the `…Optional` form adds a named none case `n` that the user's clearing
-- gesture stores.

-- | The radio group over its options: field `l` holds one of their cases.
radioButton :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioButton options = fieldLeaf @l $ radioGroupOf options

-- | `radioButton` for a choice owed but not yet made.
radioButtonUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioButtonUnpicked options = fieldLeaf @l $ radioGroupOf (focusCase @c options)

-- | `radioButton` for a choice the user may leave unmade: pressing the
-- | checked radio again clears it, storing case `n`.
radioButtonOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
radioButtonOptional options = fieldLeaf @l $ radioGroupOf (clearedByRepress @n radioBeforeClick (focusCase @c options))

radioGroupOf :: forall a. PUI Web a a -> PUI Web a a
radioGroupOf options =
  div >>> "style" := "display: flex; flex-direction: column; align-items: flex-start; gap: 8px;" >>> "role" := "radiogroup" $ fromHandles do
    { result: inner, members } <- grouping (handles options)
    liftEffect do
      groupName <- uniqueId
      members >>= traverse_ \m -> setAttribute m.node "name" groupName
    pure
      { toUser: \a -> unpickAll members *> inner.toUser a
      , fromUser: inner.fromUser
      }

radioBeforeClick :: Node -> Effect Unit -> Effect Unit
radioBeforeClick container snapshot = do
  listenNode container "pointerdown" snapshot
  listenNode container "keydown" snapshot

-- | One option of a `radioButton` group: an `md-radio` captioned by its case.
radio :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
radio = fromHandles do
  node <- element "label" do
    element "md-radio" (pure unit)
    radioNode <- gets _.sibling
    liftEffect $ setAttribute radioNode "aria-label" (reflectSymbol (Proxy @c))
    chrome (staticText (reflectSymbol (Proxy @c)))
    pure radioNode
  attribute "style" "display: inline-flex; align-items: center; gap: 12px;"
  labelNode <- gets _.sibling
  _ <- member { node, picked: getChecked node, unpick: setChecked node false }
  liftEffect do
    -- md-radio reports a change on a click on a radio already checked; the
    -- press says whether it was, so only a new pick leaves
    wasCheckedRef <- Ref.new false
    let snapshot = getChecked node >>= flip Ref.write wasCheckedRef
    listenNode labelNode "pointerdown" snapshot
    listenNode labelNode "keydown" snapshot
    optionWiring @c
      { show: setChecked node true
      , reports: \k -> listenNode node "change" do
          wasChecked <- Ref.read wasCheckedRef
          Ref.write false wasCheckedRef
          unless wasChecked k
      , picked: getChecked node
      }

-- | The Material **switch**: a setting that takes effect the moment it is
-- | flipped — notifications on, dark mode on. (A `checkbox` states a fact
-- | to be submitted with the rest of a form; a switch acts immediately.)
toggleSwitch :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
toggleSwitch = let config = { label: reflectSymbol (Proxy @l) } in fieldLeaf @l $ (switchLeaf config.label)

switchLeaf :: String -> PUI Web Boolean Boolean
switchLeaf lbl =
  label >>> "style" := "display: inline-flex; align-items: center; gap: 12px;" $ fromHandles do
    element "md-switch" (pure unit)
    node <- gets _.sibling
    liftEffect $ setAttribute node "aria-label" lbl
    chrome (staticText lbl)
    liftEffect $ recordToRecordWiring
      { show: setBoolProp "selected" node
      , held: pure false
      , reports: listenNode node "change"
      , read: \_ -> getBoolProp "selected" node
      }

-- | The **slider**: a quantity chosen by feel, where the range matters more
-- | than the exact number — a volume, a tip, a budget.
-- |
-- | The range is part of the quantity: `{ current, min, max, step }` is one
-- | business datum, so the limits come from the data and may change while
-- | the app runs, and a range nobody supplied is a compile error. `step` is
-- | `.discrete n` or `.continuous {}`.
-- |
-- | It reports on **release**, once per adjustment, so one drag is one
-- | entry in the history — one undo step, one audit line. For a readout
-- | that follows the thumb, use `sliderLive`.
slider :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
slider = let config = { label: reflectSymbol (Proxy @l) } in fieldLeaf @l $ (sliderLeaf false config.label "")

-- | `slider` whose value is read in a `unit` (`slider' @"Amount" { unit:
-- | "€" }`), shown after the number on the handle's value label, so the
-- | label stays the quantity's name.
slider' :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => { unit :: String } -> PUI Web { | r } { | r }
slider' config = fieldLeaf @l $ (sliderLeaf false (reflectSymbol (Proxy @l)) config.unit)

-- | `slider` reporting continuously while the thumb moves — for a live
-- | readout or preview that has to follow the drag. Whatever it drives
-- | should be cheap to redo; a drag that should land in the history as one
-- | change needs the plain `slider`, or a `debounced` stage downstream.
sliderLive :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
sliderLive = let config = { label: reflectSymbol (Proxy @l) } in fieldLeaf @l $ (sliderLeaf true config.label "")

-- | `sliderLive` whose value is read in a `unit`, as `slider'`.
sliderLive' :: forall @l r b. IsSymbol l => Cons l { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => { unit :: String } -> PUI Web { | r } { | r }
sliderLive' config = fieldLeaf @l $ (sliderLeaf true (reflectSymbol (Proxy @l)) config.unit)

-- `<md-slider>` ships no text label of its own (`labeled` is the handle's
-- value indicator), so a non-empty config label renders visibly above the
-- slider, like a text field's floating label.
sliderLeaf :: Boolean -> String -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
sliderLeaf live label measure
  | label == "" = bareSliderLeaf live label measure
  | otherwise =
      div >>> "style" := "display: inline-flex; flex-direction: column; align-items: flex-start;" $ fromHandles do
        chrome (span >>> cl "md-typescale-label-medium" >>> "style" := "color: var(--md-sys-color-on-surface-variant, #49454f); margin-left: 8px;" $ staticText label)
        handles (bareSliderLeaf live label measure)

bareSliderLeaf :: Boolean -> String -> String -> PUI Web { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
bareSliderLeaf live label measure = fromHandles do
  element "md-slider" (pure unit)
  attribute "labeled" ""
  attribute "aria-label" label
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  -- the handle's value label reads the number in its unit; `valueLabel`
  -- is a plain property, so it follows the thumb by listening to it
  let measured = when (measure /= "") do
        v <- getNumberProp "value" node
        setStringProp "valueLabel" node (toString v <> " " <> measure)
  liftEffect $ when (measure /= "") $ listenNode node "input" measured
  liftEffect $ recordToRecordWiring
    { show: \q -> do
        setNumberProp "min" node q.min
        setNumberProp "max" node q.max
        Variant.match { discrete: \s -> setNumberProp "step" node s, continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "value" node q.current
        measured
    , held: pure false
    , reports: \report -> do
        when live $ listenNode node "input" report
        listenNode node "change" report
    , read: \q -> getNumberProp "value" node <#> \v -> q { current = v }
    }

-- | The **range slider**: an interval chosen by feel between two thumbs — a
-- | price band, a time window, the hours a shop is open. Like `slider`'s
-- | quantity the interval is one business datum, `{ start, end, min, max,
-- | step }`, its bounds from the data; `start ≤ end` is the model's
-- | invariant, which the thumbs cannot cross. Reports on **release**, once
-- | per adjustment, like `slider`; `rangeSliderLive` follows the drag.
-- | (HTML's `rangeInput` is the native single-thumb input, a quantity.)
rangeSlider :: forall @l r b. IsSymbol l => Cons l { start :: Number, end :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
rangeSlider = fieldLeaf @l $ rangeSliderLeaf false (reflectSymbol (Proxy @l)) ""

-- | `rangeSlider` whose values are read in a `unit`, as `slider'`.
rangeSlider' :: forall @l r b. IsSymbol l => Cons l { start :: Number, end :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => { unit :: String } -> PUI Web { | r } { | r }
rangeSlider' config = fieldLeaf @l $ rangeSliderLeaf false (reflectSymbol (Proxy @l)) config.unit

-- | `rangeSlider` reporting continuously while a thumb moves, as
-- | `sliderLive` does.
rangeSliderLive :: forall @l r b. IsSymbol l => Cons l { start :: Number, end :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => PUI Web { | r } { | r }
rangeSliderLive = fieldLeaf @l $ rangeSliderLeaf true (reflectSymbol (Proxy @l)) ""

-- | `rangeSliderLive` whose values are read in a `unit`, as `slider'`.
rangeSliderLive' :: forall @l r b. IsSymbol l => Cons l { start :: Number, end :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } b r => { unit :: String } -> PUI Web { | r } { | r }
rangeSliderLive' config = fieldLeaf @l $ rangeSliderLeaf true (reflectSymbol (Proxy @l)) config.unit

rangeSliderLeaf :: Boolean -> String -> String -> PUI Web { start :: Number, end :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { start :: Number, end :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
rangeSliderLeaf live label measure
  | label == "" = bareRangeSliderLeaf live label measure
  | otherwise =
      div >>> "style" := "display: inline-flex; flex-direction: column; align-items: flex-start;" $ fromHandles do
        chrome (span >>> cl "md-typescale-label-medium" >>> "style" := "color: var(--md-sys-color-on-surface-variant, #49454f); margin-left: 8px;" $ staticText label)
        handles (bareRangeSliderLeaf live label measure)

-- `<md-slider range>`: `valueStart`/`valueEnd` are its two thumbs, each
-- with its own value label and accessible name
bareRangeSliderLeaf :: Boolean -> String -> String -> PUI Web { start :: Number, end :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] } { start :: Number, end :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
bareRangeSliderLeaf live label measure = fromHandles do
  element "md-slider" (pure unit)
  attribute "range" ""
  attribute "labeled" ""
  attribute "aria-label" label
  attribute "aria-label-start" (label <> " start")
  attribute "aria-label-end" (label <> " end")
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  let measured = when (measure /= "") do
        start <- getNumberProp "valueStart" node
        end <- getNumberProp "valueEnd" node
        setStringProp "valueLabelStart" node (toString start <> " " <> measure)
        setStringProp "valueLabelEnd" node (toString end <> " " <> measure)
  liftEffect $ when (measure /= "") $ listenNode node "input" measured
  liftEffect $ recordToRecordWiring
    { show: \q -> do
        setNumberProp "min" node q.min
        setNumberProp "max" node q.max
        Variant.match { discrete: \s -> setNumberProp "step" node s, continuous: \_ -> removeAttribute node "step" } q.step
        setNumberProp "valueStart" node q.start
        setNumberProp "valueEnd" node q.end
        measured
    , held: pure false
    , reports: \report -> do
        when live $ listenNode node "input" report
        listenNode node "change" report
    , read: \q -> do
        start <- getNumberProp "valueStart" node
        end <- getNumberProp "valueEnd" node
        pure q { start = start, end = end }
    }

-- | The Material **filled select** (exposed dropdown): one choice out of a
-- | list too long to lay out in the open; the field's label names it and
-- | stays visible above the choice once one is made. For a handful of
-- | options worth comparing side by side, prefer `radioButton` or
-- | `segmentedButton`. Its options are `option` leaves.
-- | One option of a `select`: an `md-select-option` headed by its case.
option :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
option = fromHandles do
  selectNode <- gets _.parent
  element "md-select-option" do
    chrome (div >>> "slot" := "headline" $ staticText (reflectSymbol (Proxy @c)))
  attribute "value" (reflectSymbol (Proxy @c))
  optionNode <- gets _.sibling
  liftEffect $ optionWiring @c
    { show: setStringProp "value" selectNode (reflectSymbol (Proxy @c))
    , reports: listenNode selectNode "change"
    , picked: getBoolProp "selected" optionNode
    }

select :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
select options = fieldLeaf @l $ selectGroupOf @l false options

-- | `select` whose options the model may rule out: `select' @"Table"
-- | { unavailable: busyTables }` draws every option and disables the cases
-- | `unavailable` returns for the row it is fed, re-read on every feed — a
-- | disabled option is shown, never pickable. A decoration of `select`,
-- | so its editor laws are `select`'s, and `select' { unavailable: const
-- | [] } ≈ select`; a disabled case already stored stays shown, the
-- | model's to change.
select' :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => { unavailable :: { | r } -> Array [ | w ] } -> PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
select' { unavailable } options = fromHandles do
  inner <- handles (select @l options)
  node <- gets _.sibling
  pure
    { toUser: \row -> do
        disableOptions node (map caseText (unavailable row))
        inner.toUser row
    , fromUser: inner.fromUser
    }

selectUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectUnpicked options = fieldLeaf @l $ selectGroupOf @l false (focusCase @c options)

-- | `select` for a choice the user may leave unmade: an empty first
-- | option, which picked stores case `n`.
selectOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
selectOptional options = fieldLeaf @l $ selectGroupOf @l true (blankEmits @n (focusCase @c options))

selectGroupOf :: forall @l a. IsSymbol l => Boolean -> PUI Web a a -> PUI Web a a
selectGroupOf blank options = el "md-filled-select" >>> "label" := reflectSymbol (Proxy @l) >>> "style" := "min-width: 200px;" $ fromHandles do
  selectNode <- gets _.parent
  when blank $ chrome (staticHTML "<md-select-option value=\"\"></md-select-option>")
  inner <- handles options
  pure
    { toUser: \a -> setStringProp "value" selectNode "" *> inner.toUser a
    , fromUser: inner.fromUser
    }

-- The clearing of an optional select: picking the empty option emits `n`.
blankEmits :: forall @n b v. IsSymbol n => Cons n {} b v => PUI Web [ | v ] [ | v ] -> PUI Web [ | v ] [ | v ]
blankEmits options = fromHandles do
  selectNode <- gets _.parent
  inner <- handles options
  pure
    { toUser: inner.toUser
    , fromUser: \prop -> do
        inner.fromUser prop
        listenNode selectNode "change" do
          value <- getStringProp "value" selectNode
          when (value == "") $ prop (Variant.inj (Proxy @n) {})
    }

-- | The Material **segmented button**: two to five options joined in one
-- | control, all visible, one selected — a filter row, a view switch, a
-- | size. Compact where a radio group would be airy and a dropdown would
-- | hide the alternatives. Its options are `segment` leaves.
-- | One segment of a `segmentedButton`, captioned by its case. Pressing
-- | the segment already selected picks nothing new.
segment :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
segment = fromHandles do
  chrome $ el "button" >>> cl "md3-segmented-button__segment" >>> cl "md-typescale-label-large" >>> "role" := "radio" >>> "aria-checked" := "false" $ Semigroupoid.do
    static (span >>> cl "md3-segmented-button__check" >>> "aria-hidden" := "true")
    span $ staticText (reflectSymbol (Proxy @c))
  node <- gets _.sibling
  members <- member
    { node
    , picked: hasClass segmentSelected node
    , unpick: do
        setClassIf node segmentSelected false
        setAttribute node "aria-checked" "false"
    }
  let
    selectSelf = do
      unpickAll members
      setClassIf node segmentSelected true
      setAttribute node "aria-checked" "true"
  liftEffect $ optionWiring @c
    { show: selectSelf
    , reports: \k -> listenNode node "click" do
        already <- hasClass segmentSelected node
        unless already (selectSelf *> k)
    , picked: pure true
    }

segmentSelected :: String
segmentSelected = "md3-segmented-button__segment--selected"

segmentBeforeClick :: Node -> Effect Unit -> Effect Unit
segmentBeforeClick container snapshot = listenCapture container "click" snapshot

segmentedButton :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
segmentedButton options = fieldLeaf @l $ segmentGroupOf options

segmentedButtonUnpicked :: forall @l @c w b1 v b r. IsSymbol l => IsSymbol c => Cons c [ | w ] b1 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
segmentedButtonUnpicked options = fieldLeaf @l $ segmentGroupOf (focusCase @c options)

-- | `segmentedButton` for a choice the user may leave unmade: pressing the
-- | selected segment again clears it, storing case `n`.
segmentedButtonOptional :: forall @l @c @n w b1 b2 v b r. IsSymbol l => IsSymbol c => IsSymbol n => Cons c [ | w ] b1 v => Cons n {} b2 v => Cons l [ | v ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
segmentedButtonOptional options = fieldLeaf @l $ segmentGroupOf (clearedByRepress @n segmentBeforeClick (focusCase @c options))

segmentGroupOf :: forall a. PUI Web a a -> PUI Web a a
segmentGroupOf options = div >>> cl "md3-segmented-button" >>> "role" := "radiogroup" $ fromHandles do
  liftEffect $ ensureStyle "md3-segmented-button" segmentedButtonCss
  { result: inner, members } <- grouping (handles options)
  pure
    { toUser: \a -> unpickAll members *> inner.toUser a
    , fromUser: inner.fromUser
    }

segmentedButtonCss :: String
segmentedButtonCss = """
.md3-segmented-button { display: inline-flex; border: 1px solid var(--md-sys-color-outline, #79747e); border-radius: 100px; overflow: hidden; }
.md3-segmented-button__segment { display: inline-flex; align-items: center; justify-content: center; gap: 8px; height: 40px; padding: 0 16px; border: none; border-right: 1px solid var(--md-sys-color-outline, #79747e); background: transparent; color: var(--md-sys-color-on-surface, #1d1b20); cursor: pointer; }
.md3-segmented-button__segment:last-child { border-right: none; }
.md3-segmented-button__segment--selected { background: var(--md-sys-color-secondary-container, #e8def8); color: var(--md-sys-color-on-secondary-container, #1d192b); }
.md3-segmented-button__segment--selected .md3-segmented-button__check::before { content: "✓"; }
"""

-- | The Material **filter chip**: a small tag the user switches on or off,
-- | showing a checkmark while on. Chips come in sets where any number may
-- | be active at once — dietary tags, categories, facets. Put them in a
-- | `chipSet`.
filterChip :: forall @l r b. IsSymbol l => Cons l Boolean b r => PUI Web { | r } { | r }
filterChip = let config = { label: reflectSymbol (Proxy @l) } in fieldLeaf @l $ (chipLeaf config.label)

chipLeaf :: String -> PUI Web Boolean Boolean
chipLeaf lbl = fromHandles do
  element "md-filter-chip" (pure unit)
  attribute "label" lbl
  node <- gets _.sibling
  -- the chip toggles `selected` as the click's default action; read it a
  -- microtask later, once the toggle has settled
  liftEffect $ recordToRecordWiring
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
iconToggle provided = let config = convertOptionsWithDefaults OptCaption { label: reflectSymbol (Proxy @l) } provided in fieldLeaf @l $ (iconToggleLeaf config)

iconToggleLeaf :: { onIcon :: String, offIcon :: String, label :: String } -> PUI Web Boolean Boolean
iconToggleLeaf config = fromHandles do
  -- the selected icon renders filled (the MD3 selected-state convention),
  -- so a same-glyph pair still reads as off/on
  chrome $ el "md-icon-button" >>> "toggle" := "" >>> "aria-label" := config.label $ Semigroupoid.do
    el "md-icon" $ staticText config.offIcon
    el "md-icon" >>> "slot" := "selected" >>> "style" := "font-variation-settings: 'FILL' 1;" $ staticText config.onIcon
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring
    { show: setBoolProp "selected" node
    , held: pure false
    , reports: listenNode node "change"
    , read: \_ -> getBoolProp "selected" node
    }

-- | The Material **tab bar**: the top-level sections of one screen, one
-- | open at a time. Keyboard navigation and the sliding indicator come with
-- | it. Its options are `tab` leaves (`tab'` with an icon).
-- |
-- | Unlike `segmentedButton` a tab bar is never in a "nothing picked"
-- | state — some section is always open — which is what makes it the
-- | selector to build a sectioned editor around: the tab bar beside one
-- | `inCase @l` editor pane per section, each pane editing its own part of
-- | the model.
-- | One tab of a `tabBar`, captioned by its case.
tab :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => PUI Web [ | v ] [ | o ]
tab = tabWith @c Nothing

-- | `tab` with an icon beside its caption.
tab' :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => { icon :: String } -> PUI Web [ | v ] [ | o ]
tab' { icon: glyph } = tabWith @c (Just glyph)

tabWith :: forall @c v b o. IsSymbol c => Cons c {} () v => Cons c {} b o => Maybe String -> PUI Web [ | v ] [ | o ]
tabWith glyph = fromHandles do
  tabs <- gets _.parent
  chrome $ el "md-primary-tab" $ Semigroupoid.do
    maybe identity (\i -> el "md-icon" >>> "slot" := "icon" $ staticText i) glyph
    staticText (reflectSymbol (Proxy @c))
  node <- gets _.sibling
  members <- member { node, picked: getBoolProp "active" node, unpick: setBoolProp "active" node false }
  liftEffect $ optionWiring @c
    { show: do
        unpickAll members
        setBoolProp "active" node true
    , reports: listenNode tabs "change"
    , picked: getBoolProp "active" node
    }

tabBar :: forall @l w b r. IsSymbol l => Cons l [ | w ] b r => PUI Web [ | w ] [ | w ] -> PUI Web { | r } { | r }
tabBar options = fieldLeaf @l $ el "md-tabs" >>> "style" := "align-self: stretch;" $ fromHandles (_.result <$> grouping (handles options))

-- | The **indeterminate progress bar** — see
-- | `PUI.Web.MDC2.indeterminateLinearProgress`: a status fed the run's
-- | `started` and `ended`, shown between them, `PUI.action`'s slot
-- | (`indeterminateLinearProgress # action submit`); no label, `{}` out.
-- | Over `md-linear-progress`, hidden rather than removed between runs.
indeterminateLinearProgress :: PUI Web [ started :: {}, ended :: {} ] {}
indeterminateLinearProgress = fromHandles do
  element "md-linear-progress" (pure unit)
  attribute "indeterminate" ""
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ variantToRecordWiring $ Variant.match
    { started: \_ -> setAttribute node "style" visibleStyle
    , ended: \_ -> setAttribute node "style" hiddenStyle }
  where
  visibleStyle = "min-width: 200px;"
  hiddenStyle = "min-width: 200px; visibility: hidden;"

-- | The **determinate progress bar**: how far along something is, `value`
-- | running 0 to 1. As much a gauge as a progress indicator — a quiz's
-- | position, a budget's use, a quota — written as `linearProgress
-- | progressFraction`: the value is a **read function** like every
-- | display's (guardrails L17).
linearProgress
  :: forall r
   . ({ | r } -> Number) -> PUI Web { | r } { | r }
linearProgress f = fromHandles do
  element "md-linear-progress" (pure unit)
  attribute "style" "min-width: 200px;"
  node <- gets _.sibling
  liftEffect $ recordToRecordWiring { show: \r -> setNumberProp "value" node (f r), held: pure false, reports: \_ -> pure unit, read: pure }

-- | The **spinner** — `indeterminateLinearProgress` in circular form, for
-- | inline and compact places (a button, a card corner) where a bar across
-- | the width would be too much.
indeterminateCircularProgress :: PUI Web [ started :: {}, ended :: {} ] {}
indeterminateCircularProgress = fromHandles do
  element "md-circular-progress" (pure unit)
  attribute "indeterminate" ""
  attribute "style" hiddenStyle
  node <- gets _.sibling
  pure $ variantToRecordWiring $ Variant.match
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
elevationOf klass w = fromHandles do
  liftEffect $ ensureStyle "md3-elevation" elevationCss
  handles (div w # cl klass)

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
-- | A plain ocular with no config: MD3 defines a card as a *surface* with
-- | no title (`@material/web`'s card element is a bare `<slot>`), so a
-- | card's heading is typography in its content (`titleMedium`,
-- | `headlineSmall`). A card whose content is one model sub-record is the
-- | labelled group, `group @l`.
card :: Ocular (PUI Web)
card content = fromHandles do
  liftEffect $ ensureStyle "md3-card" cardCss
  handles $ div >>> cl "md3-card" $ content

-- a flex column with a gap: MD3 custom elements carry no margins of their
-- own, so the card supplies the vertical rhythm between its children
cardCss :: String
cardCss = """
.md3-card { background: var(--md-sys-color-surface-container-low, #f7f2fa); color: var(--md-sys-color-on-surface, #1d1b20); border-radius: 12px; box-shadow: 0 1px 2px rgba(0,0,0,.3), 0 1px 3px 1px rgba(0,0,0,.15); padding: 16px; margin: 15px 0; display: flex; flex-direction: column; align-items: flex-start; gap: 16px; }
.md3-card > :is(p, h1, h2, h3, h4, h5, h6) { margin: 0; }
.md3-card > md-list { align-self: stretch; }
"""

-- | The card's **action row**: the buttons belonging to the card, side by
-- | side at their natural width instead of stretched down its column.
cardActions :: Ocular (PUI Web)
cardActions = div >>> "style" := "display: flex; gap: 8px; align-items: center;"

-- | A **labelled model group** — see `PUI.Web.MDC2.group` for the
-- | derivation and its law: `group @l w` renders `w` on a `card` headed by
-- | the label verbatim (`titleMedium`) and nests `w`'s model under field
-- | `l`, stamping the label as the accessible group name (`role="group"`,
-- | `aria-labelledby` the heading). It draws the surface, so it leads its
-- | lines (`group @"Customer" $ …`).
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
  el "md-dialog" >>> init pure showDialog closeDialog $ fromHandles do
    chrome (div >>> "slot" := "headline" $ staticText title)
    handles (div >>> "slot" := "content" $ content)

-- | The witness rung — see `PUI.Web.MDC2.confirmed`: `focusCase @l` over
-- | the modal, the `+→+` stage of case `l`, its confirm button captioned
-- | by the case and re-emitting it with the row its content answered, a
-- | cancel consuming it, every other case passing.
confirmed
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
confirmed = confirmedOver @l modalFace

-- | `confirmed` with options: `cancel:` captions its Cancel with the copy
-- | the screen speaks (`confirmed' @"Usuń danie" { cancel: "Nie, nie
-- | usuwaj dania" } $ …`, diner); the Cancel still consumes the case.
confirmed'
  :: forall @l r b v
   . IsSymbol l
  => Cons l { | r } b v
  => { cancel :: String }
  -> PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
confirmed' { cancel } = confirmedOver' @l cancel modalFace

-- | The **modal** — see `PUI.Web.MDC2.modal`: the leaf owning case `l`,
-- | telling it as its title and showing its payload until the user
-- | closes it with its "OK", which releases the case again; followed on
-- | an event path by `# acknowledged`. Over `md-dialog`.
modal
  :: forall @l r v
   . IsSymbol l
  => Cons l { | r } () v
  => PUI Web { | r } { | r }
  -> PUI Web [ | v ] [ | v ]
modal = modalOver @l modalFace

-- the modal the dialog leaves are built over: `variantToVariantWiring`
-- over `md-dialog`, an optional headline, the content stage, an optional
-- cancel button and the deciding button, named by the headline or else
-- by the decision; Escape closes the element by itself, releasing nothing
modalFace
  :: forall r
   . { title :: Maybe String, decide :: String, cancel :: Maybe String }
  -> PUI Web { | r } { | r }
  -> PUI Web { | r } { | r }
modalFace { title, decide, cancel } content = fromHandles do
  decideRef <- liftEffect $ Ref.new Nothing
  cancelRef <- liftEffect $ Ref.new Nothing
  shown <- handles $ el "md-dialog" >>> "aria-label" := fromMaybe decide title $ fromHandles do
    for_ title \t -> handles (div >>> "slot" := "headline" $ staticText t)
    w <- handles (div >>> "slot" := "content" $ content)
    chrome $ div >>> "slot" := "actions" $ fromHandles do
      for_ cancel \c -> do
        chrome (el "md-text-button" $ staticText c)
        node <- gets _.sibling
        liftEffect $ Ref.write (Just node) cancelRef
      decision <- handles (el "md-text-button" $ staticText decide)
      node <- gets _.sibling
      liftEffect $ Ref.write (Just node) decideRef
      pure decision
    pure w
  dialogNode <- gets _.sibling
  decideNode <- liftEffect $ Ref.read decideRef
  cancelNode <- liftEffect $ Ref.read cancelRef
  liftEffect $ variantToVariantWiring
    { open: showDialog dialogNode
    , close: closeDialog dialogNode
    , content: shown
    , reports: \decided -> for_ decideNode \node -> listenNode node "click" decided
    , cancels: \cancelled -> for_ cancelNode \node -> listenNode node "click" cancelled
    }

-- | The **snackbar**: a brief message at the bottom of the screen that
-- | dismisses itself after a few seconds, for something that has just
-- | happened and needs no reply ("Order placed"). It never interrupts — for
-- | something the user must acknowledge, use a `dialog`.
-- |
-- | A status that **tells its case** and takes its fold — see
-- | `PUI.Web.MDC2.snackbar` (`snackbar @"Order placed" (const
-- | usualOrder)`). Hand-rolled over the tokens, since `@material/web` has
-- | no snackbar: it dismisses itself after 5 s.
snackbar
  :: forall @l a v i o
   . IsSymbol l
  => Cons l a () v
  => ({ event :: a, model :: i } -> o)
  -> PUI Web [ | v ] (i -> o)
snackbar = statusOver @l snackbarFace

snackbarFace :: forall a r. String -> PUI Web a { | r }
snackbarFace caseLabel = fromHandles do
  liftEffect $ ensureStyle "md3-snackbar" snackbarCss
  w <- handles $ div >>> cl "md3-snackbar" >>> cl "md-typescale-body-medium" >>> "role" := "status" $
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
.md3-snackbar { position: fixed; bottom: 16px; left: 50%; transform: translateX(-50%); background: var(--md-sys-color-inverse-surface, #322f35); color: var(--md-sys-color-inverse-on-surface, #f5eff7); padding: 14px 16px; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,.3), 0 4px 8px 3px rgba(0,0,0,.15); visibility: hidden; opacity: 0; transition: opacity .15s; z-index: 1000; }
.md3-snackbar--open { visibility: visible; opacity: 1; }
"""

-- | A **menu**: a labelled button that opens a short list of `menuItem`
-- | actions and closes again when one is picked. For actions; for choosing
-- | a value the model keeps, use `select`.
menu :: String -> Ocular (PUI Web)
menu anchorCaption content =
  span >>> "style" := "position: relative; display: inline-block;" $ fromHandles do
    chrome $ el "md-outlined-button" >>> "trailing-icon" := "" $ Semigroupoid.do
      staticText anchorCaption
      el "md-icon" >>> "slot" := "icon" $ staticText "arrow_drop_down"
    anchorNode <- gets _.sibling
    w <- handles (el "md-menu" $ content)
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
-- | than computed, and must be unique: matched keys are updated in place
-- | and a row's node moves with its key, so the list refreshes under the
-- | user without flicker. The row each item is drawn from is the
-- | projection's signature, in the view model module.
listOf
  :: forall @l @k r i b2 o key v
   . IsSymbol l
  => IsSymbol k
  => Cons l key () v
  => Cons k key b2 r
  => ({ | i } -> Array { | r })
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOf = listOf' @l @k {}


-- | `listOf` with options: `selected` takes a predicate on a row, and the
-- | rows it holds take the catalogue's selected styling (`listOf'
-- | @"Person picked" @"key" { selected: isSelected } entries item`).
listOf'
  :: forall @l @k r provided i b2 o key v
   . IsSymbol l
  => IsSymbol k
  => Cons l key () v
  => Cons k key b2 r
  => ConvertOptionsWithDefaults OptSelected { selected :: { | r } -> Boolean } { | provided } { selected :: { | r } -> Boolean }
  => { | provided }
  -> ({ | i } -> Array { | r })
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOf' provided f item = withStructuralOrd @key $ fromHandles do
  liftEffect $ ensureStyle "md3-list" listCss
  handles $ el "md-list" >>> "style" := "overflow-y: auto;" $
    ( replayed @l (Record.get (Proxy @k)) $ occurrencesOn Click $ clWhen config.selected "md3-list-item--selected"
        $ el "md-list-item" >>> "type" := "button" $ item
    ) # gatheredOf @k f
  where
  config = convertOptionsWithDefaults OptSelected { selected: const false } provided

-- | A **tile**: one clickable cell of a grid of them — a sheet's cell, a
-- | keypad's key, a palette's swatch — showing the given display and
-- | reporting, as case `l`, field `k` of the row it was shown (`tile @"Key
-- | entered" @"key" $ textAt @"key"`, calculator; `tile @"Cell claimed"
-- | @"key"`, tic-tac-toe), its face styled outside like any
-- | leaf (`attrWith "style" keyFace $ tile …`). `listOf`'s pick at a
-- | single cell, built the same way — replay of the row last shown,
-- | `× → +` by shape as by behaviour — over a real `<button>`, so it has
-- | its role, its tab stop and Enter/Space without ARIA, and its content
-- | is its accessible name (a wordless swatch names itself with
-- | `attrAt @"name" "aria-label"`).
tile
  :: forall @l @k r o b1 key v
   . IsSymbol l
  => IsSymbol k
  => Cons k key b1 r
  => Cons l key () v
  => PUI Web { | r } o
  -> PUI Web { | r } [ | v ]
tile = tileOn @l @k Click

-- | `tile` reporting a **double click** — the cell a sheet edits in place
-- | (`doubleClickTile @"Cell double-clicked" @"key" $ textAt @"text"`,
-- | cells), a single click left to selection and focus.
doubleClickTile
  :: forall @l @k r o b1 key v
   . IsSymbol l
  => IsSymbol k
  => Cons k key b1 r
  => Cons l key () v
  => PUI Web { | r } o
  -> PUI Web { | r } [ | v ]
doubleClickTile = tileOn @l @k DoubleClick

tileOn
  :: forall @l @k r o b1 key v
   . IsSymbol l
  => IsSymbol k
  => Cons k key b1 r
  => Cons l key () v
  => Gesture
  -> PUI Web { | r } o
  -> PUI Web { | r } [ | v ]
tileOn gesture content = fromHandles do
  liftEffect $ ensureStyle "md3-tile" tileCss
  handles $ replayed @l (Record.get (Proxy @k)) $ occurrencesOn gesture $ el "button" >>> "type" := "button" >>> cl "md3-tile" $ content

tileCss :: String
tileCss = ".md3-tile { display: block; width: 100%; border: 0; padding: 0; margin: 0; background: none; font: inherit; color: inherit; text-align: inherit; cursor: pointer; box-sizing: border-box; }"

-- | `listOf` over an array the model stores: the rows are field `t` of the
-- | fed row, named by its label after the key
-- | (`listOfAt @"Message opened" @"id" @"messages" item`). Rows derived
-- | from the row are a projection, `listOf`.
listOfAt
  :: forall @l @k @t i r b2 b3 o key v
   . IsSymbol l
  => IsSymbol k
  => IsSymbol t
  => Cons l key () v
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
  :: forall @l @k @t provided i r b2 b3 o key v
   . IsSymbol l
  => IsSymbol k
  => IsSymbol t
  => Cons l key () v
  => Cons k key b2 r
  => Cons t (Array { | r }) b3 i
  => ConvertOptionsWithDefaults OptSelected { selected :: { | r } -> Boolean } { | provided } { selected :: { | r } -> Boolean }
  => { | provided }
  -> PUI Web { | r } o
  -> PUI Web { | i } [ | v ]
listOfAt' provided = listOf' @l @k provided (Record.get (Proxy @t))

listCss :: String
listCss = """
md-list-item.md3-list-item--selected { background: var(--md-sys-color-secondary-container, #e8def8); }
"""

-- | A **data table**: values in rows and columns, where the column a value
-- | sits in is what says what it means. Its header slot holds the fixed
-- | headings, one `columnHeader "…"` each, and its first argument is what
-- | assistive technology announces the table as (`dataTable "Cart"`); the
-- | body is `dataRow`s of `dataCell`s, usually one row per element of a
-- | collection.
dataTable :: String -> PUI Web {} {} -> Ocular (PUI Web)
dataTable tableName = dataTable' tableName { fullWidth: false }

-- | `dataTable` with options: `fullWidth: true` stretches the table across
-- | the width its container gives it, its columns spread out, instead of
-- | hugging its content (`dataTable' "Statystyki" { fullWidth: true }`,
-- | diner), and given less than 600px it tightens its cells, as
-- | `layoutGrid` folds its columns. `fullWidth: false` is `dataTable`.
dataTable' :: String -> { fullWidth :: Boolean } -> PUI Web {} {} -> Ocular (PUI Web)
dataTable' tableName { fullWidth } headerCells content = fromHandles do
  liftEffect $ ensureStyle "md3-data-table" dataTableCss
  handles $ div >>> cl "md3-data-table" >>> (if fullWidth then cl "md3-data-table--full" else identity) $
    table >>> "aria-label" := tableName $ fromHandles do
      chrome (thead $ tr $ headerCells)
      handles (tbody $ content)

-- | One **column header** of a `dataTable` — static copy naming the column,
-- | its text a value like every static: `columnHeader "Qty"`.
columnHeader :: forall r. String -> PUI Web { | r } { | r }
columnHeader heading = th >>> cl "md-typescale-title-small" >>> "role" := "columnheader" >>> "scope" := "col" $ staticText heading

-- | The **row header** of a `dataRow`, in its first cell — static copy
-- | naming the row, as `columnHeader` names a column: `rowHeader "Total"`
-- | heads a totals row, its values in `dataCell`s beside it (diner's
-- | `rowHeader "Łącznie"`).
rowHeader :: forall r. String -> PUI Web { | r } { | r }
rowHeader heading = th >>> cl "md-typescale-title-small" >>> "role" := "rowheader" >>> "scope" := "row" $ staticText heading

dataTableCss :: String
dataTableCss = """
.md3-data-table { border: 1px solid var(--md-sys-color-outline-variant, #cac4d0); border-radius: 12px; overflow-x: auto; display: inline-block; max-width: 100%; box-sizing: border-box; }
.md3-data-table--full { display: block; align-self: stretch; container-type: inline-size; }
.md3-data-table table { border-collapse: collapse; width: 100%; color: var(--md-sys-color-on-surface, #1d1b20); }
.md3-data-table th { text-align: left; height: 56px; padding: 0 16px; }
.md3-data-table td, .md3-data-table tbody th { height: 52px; padding: 6px 16px; box-sizing: border-box; border-top: 1px solid var(--md-sys-color-outline-variant, #cac4d0); }
.md3-data-table :is(td, th) > :is(p, h1, h2, h3, h4, h5, h6) { margin: 0; }
@container (max-width: 599px) { .md3-data-table :is(td, th), .md3-data-table tbody th { padding-inline: 8px; } }
"""

-- | One row of a `dataTable` — a single record's line across the columns.
dataRow :: Ocular (PUI Web)
dataRow = tr

-- | One cell of a `dataRow`: the value under one column heading. Cells are
-- | written in the same order as the table's `columns`.
dataCell :: Ocular (PUI Web)
dataCell = td >>> cl "md-typescale-body-medium"

-- | An **image list**: pictures laid out in `columns` masonry columns, each
-- | one an `imageListItem` — a gallery, where the pictures are the content
-- | rather than an illustration of it.
imageList :: Int -> Ocular (PUI Web)
imageList columns content = fromHandles do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  handles $ el "ul" >>> cl "md3-image-list" >>> "style" := ("column-count: " <> show columns <> "; column-gap: 16px; margin: 0;") $ content

imageListCss :: String
imageListCss = """
.md3-image-list { list-style: none; padding: 0; }
.md3-image-list__item { margin-bottom: 16px; break-inside: avoid; }
.md3-image-list__image { width: 100%; border-radius: 12px; display: block; }
.md3-image-list__label { color: var(--md-sys-color-on-surface, #1d1b20); }
"""

-- | The twelve-column **layout grid** a screen's regions are placed on,
-- | holding `layoutCell`s. It spans the width its container gives it and
-- | adds no margin of its own — the surface or the page it sits on owns
-- | the margins — so its cells line up with whatever stands above and
-- | below it. In a grid narrower than 600 px (MD3's compact window)
-- | every cell takes the whole row.
layoutGrid :: Ocular (PUI Web)
layoutGrid content = fromHandles do
  liftEffect $ ensureStyle "md3-layout-grid" layoutGridCss
  handles $ div >>> cl "md3-layout-grid" $ content

-- | One region of a `layoutGrid`, `span` columns wide out of twelve. A
-- | `card` in a cell fills it, the grid's gap spacing the cards.
layoutCell :: Int -> Ocular (PUI Web)
layoutCell cellSpan = div >>> cl "md3-layout-cell" >>> "style" := ("grid-column: span " <> show cellSpan <> ";")

layoutGridCss :: String
layoutGridCss = """
.md3-layout-grid { display: grid; grid-template-columns: repeat(12, 1fr); gap: 16px; align-self: stretch; container-type: inline-size; }
.md3-layout-cell { display: flex; flex-direction: column; min-width: 0; }
.md3-layout-cell > .md3-card { margin: 0; flex: 1; }
@container (max-width: 599px) { .md3-layout-cell { grid-column: 1 / -1 !important; } }
"""

-- | The **top app bar**: the band carrying the screen's title, with the
-- | content laid out beneath it.
topAppBar :: String -> Ocular (PUI Web)
topAppBar title content = fromHandles do
  liftEffect $ ensureStyle "md3-top-app-bar" topAppBarCss
  chrome $ el "header" >>> cl "md3-top-app-bar" >>> cl "md-typescale-title-large" $ span >>> cl "md3-top-app-bar__title" $ staticText title
  handles (div >>> cl "md3-top-app-bar-content" $ content)

topAppBarCss :: String
topAppBarCss = """
.md3-top-app-bar { display: flex; align-items: center; height: 64px; padding: 0 16px; background: var(--md-sys-color-surface-container, #f3edf7); color: var(--md-sys-color-on-surface, #1d1b20); }
.md3-top-app-bar-content { padding: 16px; }
"""

-- | The permanent **navigation drawer**: a titled nav panel pinned beside
-- | the content, both fed what the drawer is fed. The nav is live, not a
-- | static menu — it is the stage, so what it emits is the drawer's
-- | output: a selectable nav (a `listOf` of sections with its fold, `#
-- | stepped`) lands its pick on the row, the app's knot feeds the drawer
-- | again, and the content shows that model beside it
-- | (photo-gallery).
drawer :: forall r o. { title :: String, subtitle :: String } -> PUI Web { | r } o -> PUI Web { | r } { | r } -> PUI Web { | r } o
drawer config nav content = div >>> "style" := "display: flex;" $ fromHandles do
  liftEffect $ ensureStyle "md3-drawer" drawerCss
  nav' <- handles $ aside >>> cl "md3-drawer" $ fromHandles do
    chrome $ div >>> cl "md3-drawer__header" $ Semigroupoid.do
      h3 >>> cl "md3-drawer__title" >>> cl "md-typescale-title-medium" $ staticText config.title
      el "h6" >>> cl "md3-drawer__subtitle" >>> cl "md-typescale-body-medium" $ staticText config.subtitle
    handles (div $ nav)
  content' <- handles (div >>> "style" := "flex: 1; padding: 16px;" $ content)
  -- the content is a display: its echoes are the row the nav already has
  liftEffect $ content'.fromUser \_ -> pure unit
  pure { toUser: \r -> nav'.toUser r *> content'.toUser r, fromUser: nav'.fromUser }

drawerCss :: String
drawerCss = """
.md3-drawer { width: 256px; flex: none; box-sizing: border-box; background: var(--md-sys-color-surface, #fef7ff); border-right: 1px solid var(--md-sys-color-outline-variant, #cac4d0); padding: 12px; }
.md3-drawer__header { padding: 16px 12px; }
.md3-drawer__title { margin: 0; color: var(--md-sys-color-on-surface, #1d1b20); }
.md3-drawer__subtitle { margin: 0; color: var(--md-sys-color-on-surface-variant, #49454f); }
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
  span >>> cl "md3-tooltip-anchor" $ fromHandles do
    liftEffect $ ensureStyle "md3-tooltip" tooltipCss
    tipId <- liftEffect uniqueId
    w <- handles ("aria-describedby" := tipId $ content)
    chrome $ div >>> "id" := tipId >>> cl "md3-tooltip" >>> cl "md-typescale-body-small" >>> "role" := "tooltip" $ staticText tipText
    pure w


-- a plain tooltip is transient: it shows while the pointer rests on the anchor
-- or the anchor has KEYBOARD focus (`:focus-visible`), never for the focus a
-- click leaves behind — `:focus-within` kept it up after every tap
tooltipCss :: String
tooltipCss = """
.md3-tooltip-anchor { position: relative; display: inline-block; }
.md3-tooltip { position: absolute; top: 100%; left: 50%; transform: translateX(-50%); margin-top: 4px; background: var(--md-sys-color-inverse-surface, #322f35); color: var(--md-sys-color-inverse-on-surface, #f5eff7); padding: 4px 8px; border-radius: 4px; white-space: nowrap; visibility: hidden; opacity: 0; transition: opacity .15s; z-index: 1000; pointer-events: none; }
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
imageListItem config = fromHandles do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  handles $ el "li" >>> cl "md3-image-list__item" $ Semigroupoid.do
    static (img >>> cl "md3-image-list__image" >>> "src" := config.src >>> "alt" := config.alt)
    span >>> cl "md3-image-list__label" >>> cl "md-typescale-title-small" $ staticText config.alt


-- | One picture in an `imageList`, **fed through the channel**: like any
-- | display it takes its read function, the row in and the picture's
-- | `{ src, alt }` out, so a gallery is `actedOf` over the pictures
-- | (`imagePane developedShot # actedOf @"shot" albumShots`), each item
-- | built once and updated in place. `imageListItem` is its static
-- | sibling.
imagePane :: forall r. ({ | r } -> { src :: String, alt :: String }) -> PUI Web { | r } { | r }
imagePane picture = imagePaneFace picture

imagePaneFace :: forall r. ({ | r } -> { src :: String, alt :: String }) -> PUI Web { | r } { | r }
imagePaneFace picture = fromHandles do
  liftEffect $ ensureStyle "md3-image-list" imageListCss
  handles $ el "li" >>> cl "md3-image-list__item" $ Semigroupoid.do
    img >>> cl "md3-image-list__image" >>> attrWith "src" (_.src <<< picture) >>> attrWith "alt" (_.alt <<< picture) $ identity
    span >>> cl "md3-image-list__label" >>> cl "md-typescale-title-small" $ text (_.alt <<< picture)

-- Private

foreign import disableOptions :: Node -> Array String -> Effect Unit
foreign import setNumberProp :: String -> Node -> Number -> Effect Unit
foreign import setStringProp :: String -> Node -> String -> Effect Unit
foreign import getNumberProp :: String -> Node -> Effect Number
foreign import setIntProp :: String -> Node -> Int -> Effect Unit
foreign import getIntProp :: String -> Node -> Effect Int
foreign import setBoolProp :: String -> Node -> Boolean -> Effect Unit
foreign import getBoolProp :: String -> Node -> Effect Boolean
foreign import setClassIf :: Node -> String -> Boolean -> Effect Unit
foreign import getStringProp :: String -> Node -> Effect String
foreign import showDialog :: Node -> Effect Unit
foreign import closeDialog :: Node -> Effect Unit
foreign import openMenuAnchoredTo :: Node -> Node -> Effect Unit

-- Entry point

-- | Mount the app in the page's `<body>`, dressed for Material 3: the MD3
-- | typescale stylesheet (what gives the `md-typescale-*` classes their
-- | meaning) is adopted by the document here, once, the page's base
-- | typeface is set to the type scale's plain face, and the app then
-- | mounts as `PUI.Web.HTML.body` does — same signature, same closed-app
-- | demand (`PUI Web {} o`). Nothing runs at import time (guardrails L11).
body :: forall o. PUI Web {} o -> Effect Unit
body ui = do
  adoptTypescale
  ensureStyle "md3-body" bodyCss
  HTML.body ui

-- the page's base typeface: text outside every typography word still
-- reads in the type scale's plain face
bodyCss :: String
bodyCss = "body { font-family: var(--md-ref-typeface-plain, Roboto), sans-serif; }"

foreign import adoptTypescale :: Effect Unit
