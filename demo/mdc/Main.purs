-- | The full-catalog MDC (Material Design 2, https://m2.material.io)
-- | showcase — every component the `MDC` module offers, exercised in one
-- | genuine four-direction row pipeline:
-- |
-- |   load (action) → `×→×` settings form → `×→+` event buttons →
-- |   `+→+` backend dispatch → `+→×` status snackbars/banner
-- |
-- | The page chrome is oculars all the way down: `topAppBar` over a
-- | permanent `drawer` (whose nav is a `list` of `listItem`s — pure
-- | `{} → {}` chrome), cards per catalog section, a `layoutGrid` for the
-- | form. Editors cover every `×→×` citizen (`filledTextField`,
-- | `filledTextArea`, `checkbox`, `radioButton`, `toggleSwitch`, `slider`,
-- | `select`, `segmentedButton`, `filterChip`, `iconToggle`); the shipping
-- | variant is edited with `tab @l`s in a `tabBar` plus `casePane`s, all
-- | `synced`; the data table shows live `reading`s; the image list and
-- | dividers are announcing statics. Events cover `button`, `fab`,
-- | `iconButton` and a `menu` of `menuItem`s; dispatch shows both
-- | progress displays (`indeterminateLinearProgress` and
-- | `indeterminateCircularProgress`); statuses cover `snackbar`s and a
-- | `banner`.
-- |
-- | Type-changing editors make the form's input and output rows differ:
-- | `radioButton`/`select`/`segmentedButton` consume `Maybe`-selection
-- | fields (`SettingsIn`) and produce bare selections (`SettingsOut`).
-- | Those editors echo only a `Just` (there is no bare selection to
-- | announce otherwise), so `loadSettings` seeds every selection —
-- | otherwise the record-merge gates would hold until the user picks.
-- |
-- | (The MD2 dialog is the one omission: its open/close protocol needs a
-- | flow of its own — see `MDC.dialog`/`MDC.simpleDialog`.)
module Main (main) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Symbol (class IsSymbol)
import Data.Variant (case_, inj, on, prj) as Variant
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import MDC as MDC
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import Record (get)
import Type.Proxy (Proxy(..))
import UI (UI, action, debounced, silence, synced)
import Web (Web, attr, body, staticText, text, variant)
import Web (div) as Web

-- The named types — the aggregate the whole pipeline revolves around,
-- direction-split because the type-changing editors (radio, select,
-- segmented button) consume selection state and produce bare selections.
type SettingsIn =
  { name :: String
  , notes :: String
  , volume :: Number
  , wifi :: Boolean
  , dark :: Boolean
  , favorite :: Boolean
  , archived :: Boolean
  , subscribed :: Maybe Unit
  , plan :: Maybe String
  , theme :: Maybe String
  , size :: Maybe String
  , shipping ::
      [ standard :: { days :: String }
      , express :: { price :: String }
      ]
  }

type SettingsOut =
  { name :: String
  , notes :: String
  , volume :: Number
  , wifi :: Boolean
  , dark :: Boolean
  , favorite :: Boolean
  , archived :: Boolean
  , subscribed :: Maybe Unit
  , plan :: String
  , theme :: String
  , size :: String
  , shipping ::
      [ standard :: { days :: String }
      , express :: { price :: String }
      ]
  }

main :: Effect Unit
main = body @Unit $ MDC.topAppBar { title: "Bambik · MDC2 showcase" } $ MDC.drawer { title: "MDC2", subtitle: "the full catalog" } nav Semigroupoid.do
  action loadSettings MDC.indeterminateLinearProgress
  MDC.layoutGrid RecordToRecord.do
    MDC.layoutCell { span: 12 } $ MDC.headline6 $ reading @"name" ("Settings — " <> _)
    MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Text fields" } RecordToRecord.do
      MDC.filledTextField @"name" { floatingLabel: "Name" }
      MDC.filledTextArea @"notes" { columns: 60, rows: 3 }
    MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Selection controls" } RecordToRecord.do
      MDC.checkbox @"subscribed" $ staticText "Subscribe to the newsletter"
      MDC.radioButton @"plan"
        [ { value: "free", label: "Free plan" }
        , { value: "pro", label: "Pro plan" }
        , { value: "team", label: "Team plan" }
        ]
      MDC.tooltip { text: "Toggles connectivity" } $ MDC.toggleSwitch @"wifi" { label: "Wi-Fi" }
      MDC.iconToggle @"dark" { onIcon: "dark_mode", offIcon: "light_mode", label: "Dark mode" }
      staticText "Dark mode"
    MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Chips" } $ MDC.chipSet RecordToRecord.do
      MDC.filterChip @"favorite" { label: "Favorite" }
      MDC.filterChip @"archived" { label: "Archived" }
    MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Segmented buttons" } $
      MDC.segmentedButton @"size"
        [ { value: "S", label: "S" }
        , { value: "M", label: "M" }
        , { value: "L", label: "L" }
        ]
    MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Menus: exposed dropdown" } $
      MDC.select @"theme" { floatingLabel: "Theme" }
        [ { value: "light", label: "Light" }
        , { value: "dark", label: "Dark" }
        , { value: "system", label: "System" }
        ]
    -- the slider and its readout are `synced` siblings of the one field, so
    -- the readout follows every emission (a plain record-merge sibling would
    -- update on load only); `>>> silence` makes the display an `a → a`
    -- member that never emits
    MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Sliders" } $ synced
      [ MDC.slider @"volume" { label: "Volume", min: 0.0, max: 100.0, step: Nothing }
      , MDC.body2 (reading @"volume" (\v -> "Volume " <> show v)) >>> silence
      ]
    MDC.layoutCell { span: 12 } $ MDC.card { caption: Just "Tabs" } $ field @"shipping" $ synced
      [ MDC.tabBar $ synced
          [ MDC.tab @"standard" { label: Just "Standard", icon: Just "local_shipping" } { days: "3" }
          , MDC.tab @"express" { label: Just "Express", icon: Just "bolt" } { price: "9.99" }
          ]
      , casePane @"standard" $ MDC.filledTextField @"days" { floatingLabel: "Delivery days" }
      , casePane @"express" $ MDC.filledTextField @"price" { floatingLabel: "Express fee" }
      ]
    MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Data tables" } $
      MDC.dataTable { label: "Live summary", columns: [ "Setting", "Value" ] } RecordToRecord.do
        MDC.dataRow RecordToRecord.do
          MDC.dataCell $ staticText "Name"
          MDC.dataCell $ reading @"name" identity
        MDC.dataRow RecordToRecord.do
          MDC.dataCell $ staticText "Volume"
          MDC.dataCell $ reading @"volume" show
        MDC.dataRow RecordToRecord.do
          MDC.dataCell $ staticText "Theme"
          MDC.dataCell $ reading @"theme" (fromMaybe "—")
    MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Image lists" } $ MDC.imageList { columns: 3 } RecordToRecord.do
      MDC.imageListItem { src: swatch "845ec2" 140, label: "Iris" }
      MDC.imageListItem { src: swatch "ff9671" 100, label: "Coral" }
      MDC.imageListItem { src: swatch "00c9a7" 120, label: "Mint" }
      MDC.imageListItem { src: swatch "0081cf" 110, label: "Sea" }
      MDC.imageListItem { src: swatch "c34a36" 130, label: "Clay" }
      MDC.imageListItem { src: swatch "936c00" 90, label: "Ochre" }
    MDC.layoutCell { span: 12 } divider
    MDC.layoutCell { span: 12 } $ debounced $ MDC.body1 $ lcmap summarize text
  MDC.card { caption: Just "Buttons, FAB, icon buttons, menus" } $ Web.div >>> attr "style" "display: flex; align-items: center; gap: 16px; flex-wrap: wrap;" $ RecordToVariant.do
    MDC.button @"save" { label: Just "Save", icon: Just "save" }
    MDC.fab @"like" { icon: "favorite", label: Just "Like" }
    MDC.iconButton @"share" { icon: "share", label: "Share" }
    MDC.menu { label: "More" } RecordToVariant.do
      MDC.menuItem @"export" { label: "Export settings" }
      MDC.menuItem @"reset" { label: "Reset to defaults" }
  VariantToVariant.do
    action (Variant.on (Proxy @"save") saveSettings Variant.case_) MDC.indeterminateLinearProgress
    action (Variant.on (Proxy @"like") like Variant.case_) MDC.indeterminateCircularProgress
    action (Variant.on (Proxy @"share") share Variant.case_) MDC.indeterminateCircularProgress
    action (Variant.on (Proxy @"export") exportSettings Variant.case_) MDC.indeterminateLinearProgress
    action (Variant.on (Proxy @"reset") reset Variant.case_) MDC.indeterminateCircularProgress
  VariantToRecord.do
    MDC.snackbar @"saved"
    MDC.snackbar @"liked"
    MDC.snackbar @"shared"
    MDC.banner @"exported"
    MDC.snackbar @"resetDone"
  silence
  where
  nav = MDC.list RecordToRecord.do
    MDC.listItem $ staticText "Text fields"
    MDC.listItem $ staticText "Selection controls"
    MDC.listItem $ staticText "Chips"
    MDC.listItem $ staticText "Segmented buttons"
    MDC.listItem $ staticText "Menus"
    MDC.listItem $ staticText "Sliders"
    MDC.listItem $ staticText "Tabs"
    MDC.listItem $ staticText "Data tables"
    MDC.listItem $ staticText "Image lists"
    divider
    MDC.listItem $ staticText "Buttons & FAB"
    MDC.listItem $ staticText "Progress indicators"
    MDC.listItem $ staticText "Banner & snackbars"
  divider = MDC.divider

-- model functions

-- a self-contained placeholder image: a colored SVG swatch data URI
swatch :: String -> Int -> String
swatch color height =
  "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='300' height='" <> show height
    <> "'><rect width='100%25' height='100%25' fill='%23" <> color <> "'/></svg>"

summarize :: SettingsIn -> String
summarize s =
  "Summary: " <> s.name
    <> ", volume " <> show s.volume
    <> ", Wi-Fi " <> (if s.wifi then "on" else "off")
    <> ", plan " <> fromMaybe "unset" s.plan
    <> ", theme " <> fromMaybe "unset" s.theme
    <> ", size " <> fromMaybe "unset" s.size
    <> ", shipping " <> shippingText s.shipping
    <> "."

shippingText ::
  [ standard :: { days :: String }
  , express :: { price :: String }
  ]
  -> String
shippingText = Variant.case_
  # Variant.on (Proxy @"standard") (\r -> "standard (" <> r.days <> " days)")
  # Variant.on (Proxy @"express") (\r -> "express (" <> r.price <> " fee)")

-- asynchronous actions

loadSettings :: Unit -> Aff SettingsIn
loadSettings _ = do
  liftEffect $ log "loading settings"
  delay (Milliseconds 700.0)
  pure
    { name: "Ada Lovelace"
    , notes: "First programmer."
    , volume: 40.0
    , wifi: true
    , dark: false
    , favorite: true
    , archived: false
    , subscribed: Just unit
    -- selections are seeded: type-changing editors echo only a `Just`,
    -- so an unseeded selection would hold the record-merge gate shut
    , plan: Just "free"
    , theme: Just "light"
    , size: Just "M"
    , shipping: .standard { days: "3" }
    }

saveSettings :: SettingsOut -> Aff [ saved :: String ]
saveSettings s = do
  liftEffect $ log ("saving settings for " <> s.name)
  delay (Milliseconds 1000.0)
  pure $ .saved ("Saved settings for " <> s.name)

like :: SettingsOut -> Aff [ liked :: String ]
like s = do
  delay (Milliseconds 600.0)
  pure $ .liked (s.name <> " added to favorites")

share :: SettingsOut -> Aff [ shared :: String ]
share s = do
  delay (Milliseconds 600.0)
  pure $ .shared ("Shared the profile of " <> s.name)

exportSettings :: SettingsOut -> Aff [ exported :: String ]
exportSettings s = do
  delay (Milliseconds 1200.0)
  pure $ .exported ("Exported: " <> s.name <> ", plan " <> s.plan <> ", theme " <> s.theme <> ", size " <> s.size <> ", shipping " <> shippingText s.shipping)

reset :: SettingsOut -> Aff [ resetDone :: String ]
reset _ = do
  delay (Milliseconds 600.0)
  pure $ .resetDone "Settings reset to defaults"

-- row-generic helpers (candidates for the library once proven here)

-- | A single-field display as a record-merge operand: reads one field,
-- | contributes nothing.
reading :: forall @l a r. IsSymbol l => Cons l a () r => (a -> String) -> UI Web { | r } {}
reading render = lcmap (\r -> render (get (Proxy @l) r)) text

-- | A case *pane*: the sub-form for one case, attached to the DOM only while
-- | that case is selected (`Web.variant` hides on the other cases), emitting
-- | back into the same case.
casePane :: forall @l f b s. IsSymbol l => Cons l f b s => UI Web f f -> UI Web [ | s ] [ | s ]
casePane w = dimap (Variant.prj (Proxy @l)) (Variant.inj (Proxy @l)) (variant w)
