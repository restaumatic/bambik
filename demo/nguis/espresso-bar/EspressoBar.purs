module EspressoBar (espressoBar) where

import Prelude (Unit, min, otherwise, (#), ($), (*), (+), (<>), (==))

import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor (rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (toLower, trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, asField, forCase, mvu, projection, required, tapped, updates)
import PUI.HTML (body, staticText, text)
import PUI.MDC3 (bodyMedium, button, card, checkbox, chipSet, divider, elevation5, filledTextField, filterChip, iconToggle, labelMedium, labeled, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBar :: Effect Unit
espressoBar =
  body $
    elevation5 $
      topAppBar { title: "Espresso Bar" } $
        card { caption: "Your order" } Semigroupoid.do
          ( Semigroupoid.do
              RecordToRecord.do
                tabBar (labeled drinkChoices) # asField @"drink"
                filledTextField { floatingLabel: "Your name" } # asField @"customer"
                segmentedButton (labeled sizeChoices) # required # asField @"size"
                select { floatingLabel: "Milk" } (labeled milkChoices) # required # asField @"milk"
                radioButton (labeled roastChoices) # required # asField @"roast"
                sliderLive { label: "Sugar", min: noSugar, max: maxSugar, step: sugarStep } # asField @"sugar"
                chipSet RecordToRecord.do
                  filterChip { label: "Extra shot" } # asField @"extraShot"
                  filterChip { label: "Decaf" } # asField @"decaf"
                toggleSwitch { label: "Takeaway cup" } # asField @"takeaway"
                iconToggle { onIcon: "favorite", offIcon: "heart_plus", label: "Mark as favorite" } # asField @"favorite"
                checkbox (staticText "Loyalty member") # tooltip { text: "Members get 10% off" } # asField @"loyalty"
                divider
              menu { label: "Presets" } ( RecordToVariant.do
                  menuItem { label: "The usual" } # asCase @"theUsual"
                  menuItem { label: "Espresso, no frills" } # asCase @"espressoNoFrills" )
                # updates (match { theUsual: \m _ -> theUsual m, espressoNoFrills: \m _ -> espressoNoFrills m })
          ) # mvu usualOrder
          bodyMedium ( RecordToRecord.do
              staticText "Your cup: "
              text # projection summaryText ) # tapped
          ( RecordToRecord.do
              labelMedium $ staticText "Caffeine"
              linearProgress ) # projection caffeineFraction # tapped
          button { label: "Place order", icon: "local_cafe" } # asCase @"placeOrder" # rmap (match { placeOrder: brew })
          snackbar # forCase @"brewed"

usualOrder :: { customer :: String, drink :: String, size :: String, milk :: String, roast :: String, sugar :: Number, extraShot :: Boolean, decaf :: Boolean, takeaway :: Boolean, favorite :: Boolean, loyalty :: Maybe Unit }
usualOrder =
  { customer: ""
  , drink: "Cappuccino"
  , size: "Medium"
  , milk: "Whole"
  , roast: "Medium roast"
  , sugar: 1.0
  , extraShot: false
  , decaf: false
  , takeaway: false
  , favorite: false
  , loyalty: Nothing
  }

theUsual :: { drink :: String, size :: String, milk :: String, roast :: String, sugar :: Number, extraShot :: Boolean, decaf :: Boolean } -> { drink :: String, size :: String, milk :: String, roast :: String, sugar :: Number, extraShot :: Boolean, decaf :: Boolean }
theUsual order = order { drink = "Cappuccino", size = "Medium", milk = "Whole", roast = "Medium roast", sugar = 1.0, extraShot = false, decaf = false }

espressoNoFrills :: { drink :: String, size :: String, milk :: String, roast :: String, sugar :: Number, extraShot :: Boolean, decaf :: Boolean } -> { drink :: String, size :: String, milk :: String, roast :: String, sugar :: Number, extraShot :: Boolean, decaf :: Boolean }
espressoNoFrills order = order { drink = "Espresso", size = "Small", milk = "None", sugar = 0.0, extraShot = false, decaf = false }

brew :: { customer :: String, drink :: String, size :: String, milk :: String, roast :: String, sugar :: Number, extraShot :: Boolean, decaf :: Boolean, takeaway :: Boolean, favorite :: Boolean, loyalty :: Maybe Unit } -> [ brewed :: String ]
brew { customer, drink, size, milk, roast, sugar, extraShot, decaf, takeaway, favorite, loyalty } =
  .brewed
    ( "Coming right up" <> forCustomer { customer }
        <> ": " <> summaryText { drink, size, milk, roast, sugar, extraShot, decaf, takeaway, loyalty }
        <> (if favorite then " ★" else "")
    )

forCustomer :: { customer :: String } -> String
forCustomer { customer } = case trim customer of
  "" -> ""
  name -> ", " <> name

summaryText :: { drink :: String, size :: String, milk :: String, roast :: String, sugar :: Number, extraShot :: Boolean, decaf :: Boolean, takeaway :: Boolean, loyalty :: Maybe Unit } -> String
summaryText { drink, size, milk, roast, sugar, extraShot, decaf, takeaway, loyalty } =
  size <> " " <> toLower drink
    <> (if milk == "None" then "" else " with " <> toLower milk <> " milk")
    <> ", " <> toLower roast
    <> (if extraShot then ", extra shot" else "")
    <> (if decaf then ", decaf" else "")
    <> sugarsText sugar
    <> (if takeaway then ", to go" else "")
    <> " — " <> money (price { size, milk, extraShot, loyalty })

sugarsText :: Number -> String
sugarsText n
  | n == 0.0 = ""
  | n == 1.0 = ", 1 sugar"
  | otherwise = ", " <> toStringWith (fixed 0) n <> " sugars"

price :: { size :: String, milk :: String, extraShot :: Boolean, loyalty :: Maybe Unit } -> Number
price { size, milk, extraShot, loyalty } = discounted (sizePrice size + milkPrice milk + (if extraShot then 0.5 else 0.0))
  where
  discounted p = case loyalty of
    Just _ -> p * 0.9
    Nothing -> p

sizePrice :: String -> Number
sizePrice size = case size of
  "Small" -> 3.0
  "Large" -> 4.0
  _ -> 3.5

milkPrice :: String -> Number
milkPrice milk = case milk of
  "Oat" -> 0.4
  "Almond" -> 0.4
  _ -> 0.0

money :: Number -> String
money n = "€" <> toStringWith (fixed 2) n

caffeineFraction :: { drink :: String, extraShot :: Boolean, decaf :: Boolean } -> Number
caffeineFraction { drink, extraShot, decaf }
  | decaf = 0.05
  | otherwise = min 1.0 (drinkShots drink + (if extraShot then 0.35 else 0.0))

drinkShots :: String -> Number
drinkShots drink = case drink of
  "Espresso" -> 0.6
  "Cappuccino" -> 0.45
  _ -> 0.3

drinkChoices :: Array String
drinkChoices = [ "Espresso", "Cappuccino", "Latte" ]

sizeChoices :: Array String
sizeChoices = [ "Small", "Medium", "Large" ]

milkChoices :: Array String
milkChoices = [ "Whole", "Oat", "Almond", "None" ]

roastChoices :: Array String
roastChoices = [ "Light roast", "Medium roast", "Dark roast" ]

noSugar :: Number
noSugar = 0.0

maxSugar :: Number
maxSugar = 4.0

sugarStep :: Number
sugarStep = 1.0
