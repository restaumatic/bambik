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
-- | `select`, `segmentedButton`, `tabBar`, `filterChip`, `iconToggle`).
-- | The shipping variant is edited through **record-shaped editor state**:
-- | `dimap` brackets the variant into `ShippingState` (seeding absent
-- | payloads) and back out (projecting the selection), and `looped` — the
-- | `×`-diagonal self-trace — re-broadcasts every emission so the tab bar
-- | and its `shownWhen` panes stay mutually consistent, with payload
-- | retention falling out of the merge gates. The slider readout and the
-- | data-table/summary live views are `tapped` stages (display every
-- | emission flowing through, pass it on). The image list and dividers
-- | are announcing statics. Events cover `button`, `fab`, `iconButton`
-- | and a `menu` of `menuItem`s — plus a two-step publish **wizard as a
-- | `folding @"next"` stage**: the step state loops silently as the
-- | `next` case (an `announce` operand primes the fold with its initial
-- | state), and `published` exits into the dispatch like any other event.
-- | Dispatch shows both progress displays (`indeterminateLinearProgress`
-- | and `indeterminateCircularProgress`); a shape-agnostic `tapped` line
-- | logs the status *variant* flowing past; statuses cover `snackbar`s
-- | and a `banner`.
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

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap, lcmap)
import Data.Profunctor.Row.RecordToRecord (feedback)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToRecord (retain, unfolding)
import Data.Profunctor.Row.VariantToVariant (iterate)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (case_, inj, match, on, prj) as Variant
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import PUI (PUI, action, announce, asCase, asField, debounced, field, forCase, forField, forValue, looped, projection, seeded, silence, tapped, with)
import PUI.HTML (attr, body, div, shownWhen, staticText, text) as HTML
import PUI.MDC (banner, body1, body2, button, card, checkbox, chipSet, dataCell, dataRow, dataTable, divider, drawer, fab, filledTextArea, filledTextField, filterChip, headline6, iconButton, iconToggle, imageList, imageListItem, indeterminateCircularProgress, indeterminateLinearProgress, layoutCell, layoutGrid, list, listItem, menu, menuItem, radioButton, segmentedButton, select, slider, snackbar, tabBar, toggleSwitch, tooltip, topAppBar) as MDC
import PUI.Web (Web)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import Record (get)
import Type.Proxy (Proxy(..))

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
main =
  HTML.body $ ( MDC.topAppBar { title: "Bambik · MDC2 showcase" } $ MDC.drawer { title: "MDC2", subtitle: "the full catalog" }
    ( MDC.list RecordToRecord.do
        MDC.listItem $ HTML.staticText "Text fields"
        MDC.listItem $ HTML.staticText "Selection controls"
        MDC.listItem $ HTML.staticText "Chips"
        MDC.listItem $ HTML.staticText "Segmented buttons"
        MDC.listItem $ HTML.staticText "Menus"
        MDC.listItem $ HTML.staticText "Sliders"
        MDC.listItem $ HTML.staticText "Tabs"
        MDC.listItem $ HTML.staticText "Data tables"
        MDC.listItem $ HTML.staticText "Image lists"
        MDC.divider
        MDC.listItem $ HTML.staticText "Buttons & FAB"
        MDC.listItem $ HTML.staticText "Wizard"
        MDC.listItem $ HTML.staticText "Progress indicators"
        MDC.listItem $ HTML.staticText "Banner & snackbars"
    ) Semigroupoid.do
      -- the pipeline: stages composed with `Semigroupoid` (`>>>` under the do)
      MDC.indeterminateLinearProgress # action loadSettings
      -- the form: the ×→× merge (direction class `RecordToRecord`) — operands
      -- own disjoint output fields, inputs may overlap; label-indexed MDC
      -- components are `field @l`-shaped inside (bare `Profunctor`)
      MDC.layoutGrid RecordToRecord.do
        MDC.layoutCell { span: 12 } $ MDC.headline6 (HTML.text # projection ("Settings — " <> _) # forField @"name")
        MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Text fields" } RecordToRecord.do
          MDC.filledTextField { floatingLabel: "Name" } # asField @"name"
          MDC.filledTextArea { columns: 60, rows: 3 } # asField @"notes"
        MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Selection controls" } RecordToRecord.do
          MDC.checkbox (HTML.staticText "Subscribe to the newsletter") # asField @"subscribed"
          MDC.radioButton
            [ { value: "free", label: "Free plan" }
            , { value: "pro", label: "Pro plan" }
            , { value: "team", label: "Team plan" }
            ]
            # asField @"plan"
          MDC.tooltip { text: "Toggles connectivity" } $ MDC.toggleSwitch { label: "Wi-Fi" } # asField @"wifi"
          MDC.iconToggle { onIcon: "dark_mode", offIcon: "light_mode", label: "Dark mode" } # asField @"dark"
          HTML.staticText "Dark mode"
        MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Chips" } $ MDC.chipSet RecordToRecord.do
          MDC.filterChip { label: "Favorite" } # asField @"favorite"
          MDC.filterChip { label: "Archived" } # asField @"archived"
        MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Segmented buttons" } $
          MDC.segmentedButton
            [ { value: "S", label: "S" }
            , { value: "M", label: "M" }
            , { value: "L", label: "L" }
            ]
            # asField @"size"
        MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Menus: exposed dropdown" } $
          MDC.select { floatingLabel: "Theme" }
            [ { value: "light", label: "Light" }
            , { value: "dark", label: "Dark" }
            , { value: "system", label: "System" }
            ]
            # asField @"theme"
        -- the readout is a `tapped` stage after the slider (`Strong`: `second`
        -- retains the value, the display's echo forwards it): it displays every
        -- value the slider emits and passes it on (a plain record-merge sibling
        -- would update on load only)
        MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Sliders" } $ Semigroupoid.do
          MDC.slider { label: "Volume", min: 0.0, max: 100.0, step: Nothing } # asField @"volume"
          -- `feedback` (co-strength `Costrong`, dual of `Strong`): the `peak`
          -- field loops from this stage's output back to its input, invisible
          -- in the stage's outer `{volume} → {volume}` type; `seeded` primes
          -- the loop at registration
          ( Semigroupoid.do
              seeded { volume: 0.0, peak: 0.0 }
              lcmap stepPeak identity
              MDC.body2 (HTML.text # projection peakLine # forValue) # tapped
          ) # feedback
          MDC.body2 (HTML.text # projection (\v -> "Volume " <> show v) # forField @"volume") # tapped
        -- the variant model is edited through record-shaped editor state
        -- (`ShippingState` — all payloads persist, the merge gates retain them):
        -- `dimap` (bare `Profunctor`) brackets the variant in (seeding absent
        -- payloads) and out (projecting the selected case); `looped` — the
        -- ×-diagonal self-trace, `Costrong`'s self-feeding special case —
        -- re-broadcasts every emission so the tab bar and panes stay mutually
        -- consistent; panes stay attached (`shownWhen` only hides them — the
        -- gates need their echoes), their inputs narrowed by `lcmap`
        -- (`Profunctor`, contravariant side)
        MDC.layoutCell { span: 12 } $ MDC.card { caption: Just "Tabs" }
          ( ( RecordToRecord.do
                MDC.tabBar
                  [ { value: "standard", label: "Standard", icon: Just "local_shipping" }
                  , { value: "express", label: "Express", icon: Just "bolt" }
                  ]
                  # asField @"selected"
                HTML.shownWhen (\r -> r.selected == "standard") (MDC.filledTextField { floatingLabel: "Delivery days" } # asField @"days" # lcmap daysOf)
                HTML.shownWhen (\r -> r.selected == "express") (MDC.filledTextField { floatingLabel: "Express fee" } # asField @"price" # lcmap priceOf)
            ) # looped # dimap shippingState shippingCase
          ) # field @"shipping"
        MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Image lists" } $ MDC.imageList { columns: 3 } RecordToRecord.do
          MDC.imageListItem { src: swatch "845ec2" 140, label: "Iris" }
          MDC.imageListItem { src: swatch "ff9671" 100, label: "Coral" }
          MDC.imageListItem { src: swatch "00c9a7" 120, label: "Mint" }
          MDC.imageListItem { src: swatch "0081cf" 110, label: "Sea" }
          MDC.imageListItem { src: swatch "c34a36" 130, label: "Clay" }
          MDC.imageListItem { src: swatch "936c00" 90, label: "Ochre" }
      -- live views of the form's *output*: within the form merge, siblings never
      -- see each other's emissions (`recordToRecord` has no cross-feed), so
      -- whole-record displays go in a `tapped` stage (`Strong`) after it —
      -- every form emission is displayed and passed on
      ( MDC.layoutGrid RecordToRecord.do
        MDC.layoutCell { span: 6 } $ MDC.card { caption: Just "Data tables" } $
          MDC.dataTable { label: "Live summary", columns: [ "Setting", "Value" ] } RecordToRecord.do
            MDC.dataRow RecordToRecord.do
              MDC.dataCell $ HTML.staticText "Name"
              MDC.dataCell (HTML.text # forField @"name")
            MDC.dataRow RecordToRecord.do
              MDC.dataCell $ HTML.staticText "Volume"
              MDC.dataCell (HTML.text # projection show # forField @"volume")
            MDC.dataRow RecordToRecord.do
              MDC.dataCell $ HTML.staticText "Theme"
              MDC.dataCell (HTML.text # forField @"theme")
        MDC.layoutCell { span: 12 } MDC.divider
        MDC.layoutCell { span: 12 } (MDC.body1 (HTML.text # projection summarize # forValue) # debounced)
      ) # tapped
      -- the events: the ×→+ merge (direction class `RecordToVariant`, ungated
      -- broadcast) — every operand reads the settings record, each emits its
      -- own event cases (`recordToCase` inside the button components)
      RecordToVariant.do
        MDC.card { caption: Just "Buttons, FAB, icon buttons, menus" } $ HTML.div >>> HTML.attr "style" "display: flex; align-items: center; gap: 16px; flex-wrap: wrap;" $ RecordToVariant.do
          MDC.button { label: Just "Save", icon: Just "save" } # asCase @"save"
          MDC.fab { icon: "favorite", label: Just "Like" } # asCase @"like"
          MDC.iconButton { icon: "share", label: "Share" } # asCase @"share"
          MDC.menu { label: "More" } RecordToVariant.do
            MDC.menuItem { label: "Export settings" } # asCase @"export"
            MDC.menuItem { label: "Reset to defaults" } # asCase @"reset"
        -- the wizard: `folding` (co-strength `Coresolving`, the retraction of
        -- `Resolving` — a terminating fold) makes this ×→+ operand loop: the
        -- "next" case carries the step state and re-enters silently
        -- (re-rendering the step — the nullary `announce` primes the fold with
        -- its initial state, the way units announce their `{}`), while the
        -- "published" case exits into the dispatch like any other event
        MDC.card { caption: Just "Wizard (folding)" }
          ( ( Semigroupoid.do
                ( RecordToRecord.do
                    HTML.shownWhen (\r -> r.step == "review") $ MDC.body2 (HTML.text # projection reviewLine # forValue)
                    HTML.shownWhen (\r -> r.step == "confirm") $ MDC.body2 (HTML.text # projection confirmLine # forValue)
                ) # tapped
                HTML.div >>> HTML.attr "style" "display: flex; align-items: center; gap: 16px;" $ RecordToVariant.do
                  announce initialStep
                  HTML.shownWhen (\r -> r.step == "review") (MDC.button { label: Just "Next", icon: Nothing } # asCase @"next" # lcmap (toStep "confirm"))
                  HTML.shownWhen (\r -> r.step == "confirm") (MDC.button { label: Just "Back", icon: Nothing } # asCase @"next" # lcmap (toStep "review"))
                  HTML.shownWhen (\r -> r.step == "confirm") (MDC.button { label: Just "Publish", icon: Just "publish" } # asCase @"publish" # lcmap essentials)
            ) # folding @"next"
          )
      -- the dispatch: the +→+ merge (direction class `VariantToVariant`) —
      -- exclusive inputs, one action handler per event case
      VariantToVariant.do
        MDC.indeterminateLinearProgress # action (Variant.on (Proxy @"save") saveSettings Variant.case_)
        MDC.indeterminateCircularProgress # action (Variant.on (Proxy @"like") like Variant.case_)
        MDC.indeterminateCircularProgress # action (Variant.on (Proxy @"share") share Variant.case_)
        MDC.indeterminateLinearProgress # action (Variant.on (Proxy @"export") exportSettings Variant.case_)
        MDC.indeterminateCircularProgress # action (Variant.on (Proxy @"reset") reset Variant.case_)
        -- retry: `iterate` (co-strength `Cochoice`, dual of `Choice`) loops
        -- the flaky publish — a failed attempt re-emits the `publish` case
        -- (attempt incremented), which re-enters this handler; success exits
        -- as the `published` status like any other case
        MDC.indeterminateCircularProgress # action (Variant.on (Proxy @"publish") publishFlaky Variant.case_) # iterate
      -- the activity meter: `tapped` (`Strong`, shape-agnostic) duplicates the
      -- status stream into a display arm, and the arm is an `unfolding`
      -- (co-strength `Coretaining`, dual of `Retaining`): each status case
      -- joins the running count via `retain` — the count re-enters as the
      -- `resume` case, `countUp` does the event⋈state join, `seeded` primes
      -- the state at registration
      ( Semigroupoid.do
          seeded resumeZero
          retain identity # dimap splitStatus countUp
          MDC.body2 (HTML.text # projection activityLine # forValue) # tapped
      ) # unfolding @"resume" # tapped
      -- the statuses: the +→× merge (direction class `VariantToRecord`) —
      -- one receiver per message case
      VariantToRecord.do
        MDC.snackbar # forCase @"saved"
        MDC.snackbar # forCase @"liked"
        MDC.snackbar # forCase @"shared"
        MDC.banner # forCase @"exported"
        MDC.snackbar # forCase @"resetDone"
        MDC.snackbar # forCase @"published"
      -- the sink: `silence`, the merges' variant-output unit (`pempty`)
      silence
  ) # with unit

-- model functions

-- a self-contained placeholder image: a colored SVG swatch data URI
swatch :: String -> Int -> String
swatch color height =
  "data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' width='300' height='" <> show height
    <> "'><rect width='100%25' height='100%25' fill='%23" <> color <> "'/></svg>"

summarize :: SettingsOut -> String
summarize s =
  "Summary: " <> s.name
    <> ", volume " <> show s.volume
    <> ", Wi-Fi " <> (if s.wifi then "on" else "off")
    <> ", plan " <> s.plan
    <> ", theme " <> s.theme
    <> ", size " <> s.size
    <> ", shipping " <> shippingText s.shipping
    <> "."

shippingText ::
  [ standard :: { days :: String }
  , express :: { price :: String }
  ]
  -> String
shippingText = Variant.match
  { standard: \r -> "standard (" <> r.days <> " days)"
  , express: \r -> "express (" <> r.price <> " fee)"
  }

-- the shipping ensemble's editor state: the model holds one case at a
-- time, the editor keeps every payload (retained by the merge gates while
-- the `looped` ensemble runs); `shippingState`/`shippingCase` bracket the
-- variant in (seeding absent payloads) and out (projecting the selection)
type ShippingState = { selected :: String, days :: String, price :: String }

shippingState ::
  [ standard :: { days :: String }
  , express :: { price :: String }
  ]
  -> ShippingState
shippingState = Variant.match
  { standard: \r -> { selected: "standard", days: r.days, price: "9.99" }
  , express: \r -> { selected: "express", days: "3", price: r.price }
  }

shippingCase :: ShippingState ->
  [ standard :: { days :: String }
  , express :: { price :: String }
  ]
shippingCase s = if s.selected == "standard" then .standard { days: s.days } else .express { price: s.price }

daysOf :: ShippingState -> { days :: String }
daysOf s = { days: s.days }

priceOf :: ShippingState -> { price :: String }
priceOf s = { price: s.price }

-- wizard view/model functions (their signatures close the wizard's rows:
-- inputs name/plan joined with the fold state step)
initialStep ::
  [ publish :: { name :: String, plan :: String, attempt :: Int }
  , next :: { step :: String }
  ]
initialStep = .next { step: "review" }

reviewLine :: { name :: String, plan :: String, step :: String } -> String
reviewLine r = "Step 1 of 2 — review: publish " <> r.name <> " (" <> r.plan <> " plan)?"

confirmLine :: { name :: String, plan :: String, step :: String } -> String
confirmLine r = "Step 2 of 2 — confirm publishing " <> r.name <> "."

toStep :: String -> { name :: String, plan :: String, step :: String } -> { step :: String }
toStep s _ = { step: s }

essentials :: { name :: String, plan :: String, step :: String } -> { name :: String, plan :: String, attempt :: Int }
essentials r = { name: r.name, plan: r.plan, attempt: 0 }

stepPeak :: { volume :: Number, peak :: Number } -> { volume :: Number, peak :: Number }
stepPeak r = { volume: r.volume, peak: max r.volume r.peak }

peakLine :: { volume :: Number, peak :: Number } -> String
peakLine r = "Session peak " <> show r.peak

resumeZero ::
  [ saved :: String
  , liked :: String
  , shared :: String
  , exported :: String
  , resetDone :: String
  , published :: String
  , resume :: { count :: Int }
  ]
resumeZero = .resume { count: 0 }

splitStatus ::
  [ saved :: String
  , liked :: String
  , shared :: String
  , exported :: String
  , resetDone :: String
  , published :: String
  , resume :: { count :: Int }
  ]
  -> Either String { count :: Int }
splitStatus = Variant.match
  { saved: Left
  , liked: Left
  , shared: Left
  , exported: Left
  , resetDone: Left
  , published: Left
  , resume: Right
  }

countUp :: Tuple String { count :: Int } -> { last :: String, count :: Int }
countUp (Tuple message st) = { last: message, count: st.count + 1 }

activityLine :: { last :: String, count :: Int } -> String
activityLine r = show r.count <> (if r.count == 1 then " action" else " actions") <> " — last: " <> r.last

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

publishFlaky :: { name :: String, plan :: String, attempt :: Int } -> Aff
  [ published :: String
  , publish :: { name :: String, plan :: String, attempt :: Int }
  ]
publishFlaky r = do
  delay (Milliseconds 800.0)
  if r.attempt < 1
    then do
      liftEffect $ log "publish failed, retrying"
      pure $ .publish r { attempt = r.attempt + 1 }
    else pure $ .published ("Published " <> r.name <> " on the " <> r.plan <> " plan on attempt " <> show (r.attempt + 1))

