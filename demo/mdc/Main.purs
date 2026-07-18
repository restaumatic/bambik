module Main (main) where

import Prelude ((#), ($), (+), (<), (<>), (==), (>>>), Unit, discard, identity, max, pure, show, unit)

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
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Console (log)
import PUI (action, announce, asCase, asField, completed, debounced, displayed, field, forCase, forField, forValue, looped, muted, onCase, projection, seeded, silence, tapped, updates, with)
import PUI.HTML (attr, body, div, provided, staticText, text)
import PUI.MDC (banner, body1, body2, button, card, checkbox, chipSet, dataCell, dataRow, dataTable, divider, drawer, fab, filledTextArea, filledTextField, filterChip, headline6, iconButton, iconToggle, imageList, imageListItem, indeterminateCircularProgress, indeterminateLinearProgress, layoutCell, layoutGrid, list, listItem, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

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
  body $ ( topAppBar { title: "Bambik · MDC2 showcase" } $ drawer { title: "MDC2", subtitle: "the full catalog" }
    ( muted $ list RecordToRecord.do
        listItem $ staticText "Text fields"
        listItem $ staticText "Selection controls"
        listItem $ staticText "Chips"
        listItem $ staticText "Segmented buttons"
        listItem $ staticText "Menus"
        listItem $ staticText "Sliders"
        listItem $ staticText "Tabs"
        listItem $ staticText "Data tables"
        listItem $ staticText "Image lists"
        divider
        listItem $ staticText "Buttons & FAB"
        listItem $ staticText "Wizard"
        listItem $ staticText "Progress indicators"
        listItem $ staticText "Banner & snackbars"
    ) Semigroupoid.do
      indeterminateLinearProgress # action loadSettings
      layoutGrid RecordToRecord.do
        layoutCell { span: 12 } $ headline6 (text # projection ("Settings — " <> _) # forField @"name")
        layoutCell { span: 6 } $ card { caption: "Text fields" } RecordToRecord.do
          filledTextField { floatingLabel: "Name" } # asField @"name"
          filledTextArea { columns: 60, rows: 3 } # asField @"notes"
        layoutCell { span: 6 } $ card { caption: "Selection controls" } RecordToRecord.do
          checkbox (staticText "Subscribe to the newsletter") # asField @"subscribed"
          radioButton
            [ { value: "free", label: "Free plan" }
            , { value: "pro", label: "Pro plan" }
            , { value: "team", label: "Team plan" }
            ]
            # asField @"plan"
          tooltip { text: "Toggles connectivity" } $ toggleSwitch { label: "Wi-Fi" } # asField @"wifi"
          iconToggle { onIcon: "dark_mode", offIcon: "light_mode", label: "Dark mode" } # asField @"dark"
          staticText "Dark mode"
        layoutCell { span: 6 } $ card { caption: "Chips" } $ chipSet RecordToRecord.do
          filterChip { label: "Favorite" } # asField @"favorite"
          filterChip { label: "Archived" } # asField @"archived"
        layoutCell { span: 6 } $ card { caption: "Segmented buttons" } $
          segmentedButton
            [ { value: "S", label: "S" }
            , { value: "M", label: "M" }
            , { value: "L", label: "L" }
            ]
            # asField @"size"
        layoutCell { span: 6 } $ card { caption: "Menus: exposed dropdown" } $
          select { floatingLabel: "Theme" }
            [ { value: "light", label: "Light" }
            , { value: "dark", label: "Dark" }
            , { value: "system", label: "System" }
            ]
            # asField @"theme"
        layoutCell { span: 6 } $ card { caption: "Sliders" } $ Semigroupoid.do
          sliderLive { label: "Volume", min: minVolume, max: maxVolume } # asField @"volume"
          ( Semigroupoid.do
              seeded zeroVolume
              lcmap stepPeak identity
              body2 (text # projection peakLine # forValue) # tapped
          ) # feedback
          body2 (text # projection volumeLine # forField @"volume") # tapped
        layoutCell { span: 12 } $ card { caption: "Tabs" }
          ( ( Semigroupoid.do
                tabBar
                  [ { value: "standard", label: "Standard", icon: "local_shipping" }
                  , { value: "express", label: "Express", icon: "bolt" }
                  ]
                  # asField @"selected" # completed
                filledTextField { floatingLabel: "Delivery days" } # asField @"days"
                  # provided # lcmap standardPane # updates setDays
                filledTextField { floatingLabel: "Express fee" } # asField @"price"
                  # provided # lcmap expressPane # updates setPrice
            ) # looped # dimap shippingState shippingCase
          ) # field @"shipping"
        layoutCell { span: 6 } $ card { caption: "Image lists" } $ imageList { columns: 3 } RecordToRecord.do
          imageListItem { src: swatch "845ec2" 140, label: "Iris" }
          imageListItem { src: swatch "ff9671" 100, label: "Coral" }
          imageListItem { src: swatch "00c9a7" 120, label: "Mint" }
          imageListItem { src: swatch "0081cf" 110, label: "Sea" }
          imageListItem { src: swatch "c34a36" 130, label: "Clay" }
          imageListItem { src: swatch "936c00" 90, label: "Ochre" }
      ( layoutGrid RecordToRecord.do
        layoutCell { span: 6 } $ card { caption: "Data tables" } $
          dataTable { label: "Live summary", columns: [ "Setting", "Value" ] } RecordToRecord.do
            dataRow RecordToRecord.do
              dataCell $ staticText "Name"
              dataCell (text # forField @"name")
            dataRow RecordToRecord.do
              dataCell $ staticText "Volume"
              dataCell (text # projection show # forField @"volume")
            dataRow RecordToRecord.do
              dataCell $ staticText "Theme"
              dataCell (text # forField @"theme")
        layoutCell { span: 12 } divider
        layoutCell { span: 12 } (body1 (text # projection summarize # forValue) # debounced)
      ) # tapped
      RecordToVariant.do
        card { caption: "Buttons, FAB, icon buttons, menus" } $ div >>> attr "style" "display: flex; align-items: center; gap: 16px; flex-wrap: wrap;" $ RecordToVariant.do
          button { label: "Save", icon: "save" } # asCase @"save"
          fab { icon: "favorite", label: "Like" } # asCase @"like"
          iconButton { icon: "share", label: "Share" } # asCase @"share"
          menu { label: "More" } RecordToVariant.do
            menuItem { label: "Export settings" } # asCase @"export"
            menuItem { label: "Reset to defaults" } # asCase @"reset"
        card { caption: "Wizard (folding)" }
          ( ( Semigroupoid.do
                body2 (text # projection reviewLine # forValue) # provided # lcmap atReview # displayed
                body2 (text # projection confirmLine # forValue) # provided # lcmap atConfirm # displayed
                div >>> attr "style" "display: flex; align-items: center; gap: 16px;" $ RecordToVariant.do
                  announce reviewStep
                  button { label: "Next" } # asCase @"next" # provided # lcmap nextAtReview
                  button { label: "Back" } # asCase @"next" # provided # lcmap backAtConfirm
                  button { label: "Publish", icon: "publish" } # asCase @"publish" # provided # lcmap publishAtConfirm
            ) # folding @"next"
          )
      VariantToVariant.do
        indeterminateLinearProgress # action saveSettings # onCase @"save"
        indeterminateCircularProgress # action like # onCase @"like"
        indeterminateCircularProgress # action share # onCase @"share"
        indeterminateLinearProgress # action exportSettings # onCase @"export"
        indeterminateCircularProgress # action reset # onCase @"reset"
        indeterminateCircularProgress # action publishFlaky # onCase @"publish" # iterate
      ( Semigroupoid.do
          seeded resumeZero
          retain identity # dimap splitStatus countUp
          body2 (text # projection activityLine # forValue) # tapped
      ) # unfolding @"resume" # tapped
      VariantToRecord.do
        snackbar # forCase @"saved"
        snackbar # forCase @"liked"
        snackbar # forCase @"shared"
        banner # forCase @"exported"
        snackbar # forCase @"resetDone"
        snackbar # forCase @"published"
      silence
  ) # with unit

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
shippingText = match
  { standard: \r -> "standard (" <> r.days <> " days)"
  , express: \r -> "express (" <> r.price <> " fee)"
  }

type ShippingState = { selected :: String, days :: String, price :: String }

shippingState ::
  [ standard :: { days :: String }
  , express :: { price :: String }
  ]
  -> ShippingState
shippingState = match
  { standard: \r -> { selected: "standard", days: r.days, price: "9.99" }
  , express: \r -> { selected: "express", days: "3", price: r.price }
  }

shippingCase :: ShippingState ->
  [ standard :: { days :: String }
  , express :: { price :: String }
  ]
shippingCase s = if s.selected == "standard" then .standard { days: s.days } else .express { price: s.price }

standardPane :: ShippingState -> Maybe { days :: String }
standardPane s = if s.selected == "standard" then Just { days: s.days } else Nothing

expressPane :: ShippingState -> Maybe { price :: String }
expressPane s = if s.selected == "express" then Just { price: s.price } else Nothing

setDays :: { days :: String } -> ShippingState -> ShippingState
setDays { days } s = s { days = days }

setPrice :: { price :: String } -> ShippingState -> ShippingState
setPrice { price } s = s { price = price }

reviewStep ::
  [ publish :: { name :: String, plan :: String, attempt :: Int }
  , next :: { step :: String }
  ]
reviewStep = .next { step: "review" }

reviewLine :: { name :: String, plan :: String, step :: String } -> String
reviewLine r = "Step 1 of 2 — review: publish " <> r.name <> " (" <> r.plan <> " plan)?"

confirmLine :: { name :: String, plan :: String, step :: String } -> String
confirmLine r = "Step 2 of 2 — confirm publishing " <> r.name <> "."

atReview :: { name :: String, plan :: String, step :: String } -> Maybe { name :: String, plan :: String, step :: String }
atReview r = if r.step == "review" then Just r else Nothing

atConfirm :: { name :: String, plan :: String, step :: String } -> Maybe { name :: String, plan :: String, step :: String }
atConfirm r = if r.step == "confirm" then Just r else Nothing

nextAtReview :: { name :: String, plan :: String, step :: String } -> Maybe { step :: String }
nextAtReview r = if r.step == "review" then Just { step: "confirm" } else Nothing

backAtConfirm :: { name :: String, plan :: String, step :: String } -> Maybe { step :: String }
backAtConfirm r = if r.step == "confirm" then Just { step: "review" } else Nothing

publishAtConfirm :: { name :: String, plan :: String, step :: String } -> Maybe { name :: String, plan :: String, attempt :: Int }
publishAtConfirm r = if r.step == "confirm" then Just { name: r.name, plan: r.plan, attempt: 0 } else Nothing

stepPeak :: { volume :: Number, peak :: Number } -> { volume :: Number, peak :: Number }
stepPeak r = { volume: r.volume, peak: max r.volume r.peak }

peakLine :: { volume :: Number, peak :: Number } -> String
peakLine r = "Session peak " <> show r.peak

volumeLine :: Number -> String
volumeLine v = "Volume " <> show v

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
splitStatus = match
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

minVolume :: Number
minVolume = 0.0

maxVolume :: Number
maxVolume = 100.0

zeroVolume :: { volume :: Number, peak :: Number }
zeroVolume = { volume: 0.0, peak: 0.0 }
