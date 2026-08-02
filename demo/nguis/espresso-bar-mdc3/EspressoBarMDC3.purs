module EspressoBarMDC3 (espressoBarMDC3) where

import Prelude (Unit, const, min, otherwise, (#), ($), (*), (+), (<>), (==))

import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (trim)
import Data.Variant (match)
import Effect (Effect)
import PUI (PUI, asCase, asField, forCase, mvu, projected, required, tapped, updated, with)
import PUI.HTML (body, div, staticText, text)
import PUI.Web (Web)
import PUI.MDC3 (bodyMedium, button, card, checkbox, chipSet, divider, elevation5, filledTextField, filterChip, iconToggle, labelMedium, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBarMDC3 :: Effect Unit
espressoBarMDC3 =
  body $
    elevation5 $
      topAppBar { title: "Espresso Bar" } $
        card { caption: "Your order" } Semigroupoid.do
          ( Semigroupoid.do
              RecordToRecord.do
                tabBar
                  [ { value: .espresso {}, label: "Espresso" }
                  , { value: .cappuccino {}, label: "Cappuccino" }
                  , { value: .latte {}, label: "Latte" }
                  ] # asField @"drink"
                filledTextField { floatingLabel: "Your name" } # asField @"customer"
                segmentedButton
                  [ { value: .small {}, label: "Small" }
                  , { value: .medium {}, label: "Medium" }
                  , { value: .large {}, label: "Large" }
                  ] # required # asField @"size"
                select { floatingLabel: "Milk" }
                  [ { value: .whole {}, label: "Whole" }
                  , { value: .oat {}, label: "Oat" }
                  , { value: .almond {}, label: "Almond" }
                  , { value: .none {}, label: "None" }
                  ] # required # asField @"milk"
                radioButton
                  [ { value: .light {}, label: "Light roast" }
                  , { value: .medium {}, label: "Medium roast" }
                  , { value: .dark {}, label: "Dark roast" }
                  ] # required # asField @"roast"
                sliderLive { label: "Sugar" } # asField @"sugar"
                chipSet RecordToRecord.do
                  filterChip { label: "Extra shot" } # asField @"extraShot"
                  filterChip { label: "Decaf" } # asField @"decaf"
                toggleSwitch { label: "Takeaway cup" } # asField @"takeaway"
                iconToggle { onIcon: "favorite", offIcon: "heart_plus", label: "Mark as favorite" } # asField @"favorite"
                checkbox (staticText "Loyalty member") # tooltip { text: "Members get 10% off" } # asField @"loyalty"
                divider
              menu { label: "Presets" } ( RecordToVariant.do
                  menuItem { label: "The usual" } # asCase @"theUsual" # with theUsual
                  menuItem { label: "Espresso, no frills" } # asCase @"espressoNoFrills" )
                # updated (match { theUsual: const, espressoNoFrills: \m _ -> espressoNoFrills m })
          ) # mvu usualOrder
          bodyMedium ( RecordToRecord.do
              staticText "Your cup: "
              text # projected summaryText ) # tapped
          ( div $ RecordToRecord.do
              labelMedium $ staticText "Caffeine"
              linearProgress ) # projected caffeineFraction # tapped
          button { label: "Place order", icon: "local_cafe" } # asCase @"brewed"
          brewedToast

usualOrder :: { customer :: String, drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], roast :: [ light :: {}, medium :: {}, dark :: {} ], sugar :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, extraShot :: Boolean, decaf :: Boolean, takeaway :: Boolean, favorite :: Boolean, loyalty :: Maybe {} }
usualOrder =
  { customer: ""
  , drink: .cappuccino {}
  , size: .medium {}
  , milk: .whole {}
  , roast: .medium {}
  , sugar: sugars 1.0
  , extraShot: false
  , decaf: false
  , takeaway: false
  , favorite: false
  , loyalty: Nothing
  }

theUsual :: { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], roast :: [ light :: {}, medium :: {}, dark :: {} ], sugar :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, extraShot :: Boolean, decaf :: Boolean }
theUsual = { drink: .cappuccino {}, size: .medium {}, milk: .whole {}, roast: .medium {}, sugar: sugars 1.0, extraShot: false, decaf: false }

espressoNoFrills :: { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], roast :: [ light :: {}, medium :: {}, dark :: {} ], sugar :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, extraShot :: Boolean, decaf :: Boolean } -> { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], roast :: [ light :: {}, medium :: {}, dark :: {} ], sugar :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, extraShot :: Boolean, decaf :: Boolean }
espressoNoFrills order = order { drink = .espresso {}, size = .small {}, milk = .none {}, sugar = sugars 0.0, extraShot = false, decaf = false }

brewedToast :: PUI Web [ brewed :: { customer :: String, drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], roast :: [ light :: {}, medium :: {}, dark :: {} ], sugar :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, extraShot :: Boolean, decaf :: Boolean, takeaway :: Boolean, favorite :: Boolean, loyalty :: Maybe {} } ] {}
brewedToast = snackbar # forCase @"brewed" brewedLine

brewedLine :: { customer :: String, drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], roast :: [ light :: {}, medium :: {}, dark :: {} ], sugar :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, extraShot :: Boolean, decaf :: Boolean, takeaway :: Boolean, favorite :: Boolean, loyalty :: Maybe {} } -> String
brewedLine { customer, drink, size, milk, roast, sugar, extraShot, decaf, takeaway, favorite, loyalty } =
  "Coming right up" <> forCustomer { customer }
    <> ": " <> summaryText { drink, size, milk, roast, sugar, extraShot, decaf, takeaway, loyalty }
    <> (if favorite then " ★" else "")

forCustomer :: { customer :: String } -> String
forCustomer { customer } = case trim customer of
  "" -> ""
  name -> ", " <> name

summaryText :: { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], roast :: [ light :: {}, medium :: {}, dark :: {} ], sugar :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, extraShot :: Boolean, decaf :: Boolean, takeaway :: Boolean, loyalty :: Maybe {} } -> String
summaryText { drink, size, milk, roast, sugar, extraShot, decaf, takeaway, loyalty } =
  sizeText size <> " " <> drinkText drink
    <> milkText milk
    <> ", " <> roastText roast
    <> (if extraShot then ", extra shot" else "")
    <> (if decaf then ", decaf" else "")
    <> sugarsText sugar.current
    <> (if takeaway then ", to go" else "")
    <> " — " <> money (price { size, milk, extraShot, loyalty })

drinkText :: [ espresso :: {}, cappuccino :: {}, latte :: {} ] -> String
drinkText = match { espresso: \_ -> "espresso", cappuccino: \_ -> "cappuccino", latte: \_ -> "latte" }

sizeText :: [ small :: {}, medium :: {}, large :: {} ] -> String
sizeText = match { small: \_ -> "Small", medium: \_ -> "Medium", large: \_ -> "Large" }

milkText :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ] -> String
milkText = match { whole: \_ -> " with whole milk", oat: \_ -> " with oat milk", almond: \_ -> " with almond milk", none: \_ -> "" }

roastText :: [ light :: {}, medium :: {}, dark :: {} ] -> String
roastText = match { light: \_ -> "light roast", medium: \_ -> "medium roast", dark: \_ -> "dark roast" }

sugarsText :: Number -> String
sugarsText n
  | n == 0.0 = ""
  | n == 1.0 = ", 1 sugar"
  | otherwise = ", " <> toStringWith (fixed 0) n <> " sugars"

price :: { size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], extraShot :: Boolean, loyalty :: Maybe {} } -> Number
price { size, milk, extraShot, loyalty } = discounted (sizePrice size + milkPrice milk + (if extraShot then 0.5 else 0.0))
  where
  discounted p = case loyalty of
    Just _ -> p * 0.9
    Nothing -> p

sizePrice :: [ small :: {}, medium :: {}, large :: {} ] -> Number
sizePrice = match { small: \_ -> 3.0, medium: \_ -> 3.5, large: \_ -> 4.0 }

milkPrice :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ] -> Number
milkPrice = match { whole: \_ -> 0.0, oat: \_ -> 0.4, almond: \_ -> 0.4, none: \_ -> 0.0 }

money :: Number -> String
money n = "€" <> toStringWith (fixed 2) n

caffeineFraction :: { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], extraShot :: Boolean, decaf :: Boolean } -> Number
caffeineFraction { drink, extraShot, decaf }
  | decaf = 0.05
  | otherwise = min 1.0 (drinkShots drink + (if extraShot then 0.35 else 0.0))

drinkShots :: [ espresso :: {}, cappuccino :: {}, latte :: {} ] -> Number
drinkShots = match { espresso: \_ -> 0.6, cappuccino: \_ -> 0.45, latte: \_ -> 0.3 }

sugars :: Number -> { current :: Number, min :: Number, max :: Number, step :: Maybe Number }
sugars n = { current: n, min: noSugar, max: maxSugar, step: Just 1.0 }

noSugar :: Number
noSugar = 0.0

maxSugar :: Number
maxSugar = 4.0
