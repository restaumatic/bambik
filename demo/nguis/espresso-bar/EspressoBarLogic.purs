module EspressoBarLogic (brewedLine, caffeineFraction, espressoNoFrills, summaryText, theUsual, usualOrder) where

import Prelude (min, otherwise, (*), (+), (<>), (==))

import Data.Maybe (Maybe(..))
import Data.Number.Format (fixed, toStringWith)
import Data.String (trim)
import Data.Variant (match)

usualOrder :: { customer :: String, drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], "Size" :: [ small :: {}, medium :: {}, large :: {} ], "Milk" :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], "Roast" :: [ light :: {}, medium :: {}, dark :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Extra shot" :: Boolean, "Decaf" :: Boolean, takeaway :: Boolean, favorite :: Boolean, "Loyalty" :: Maybe {} }
usualOrder =
  { customer: ""
  , drink: .cappuccino {}
  , "Size": .medium {}
  , "Milk": .whole {}
  , "Roast": .medium {}
  , "Sugar": sugars 1.0
  , "Extra shot": false
  , "Decaf": false
  , takeaway: false
  , favorite: false
  , "Loyalty": Nothing
  }

theUsual :: { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], "Size" :: [ small :: {}, medium :: {}, large :: {} ], "Milk" :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], "Roast" :: [ light :: {}, medium :: {}, dark :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Extra shot" :: Boolean, "Decaf" :: Boolean }
theUsual = { drink: .cappuccino {}, "Size": .medium {}, "Milk": .whole {}, "Roast": .medium {}, "Sugar": sugars 1.0, "Extra shot": false, "Decaf": false }

espressoNoFrills :: { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], "Size" :: [ small :: {}, medium :: {}, large :: {} ], "Milk" :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], "Roast" :: [ light :: {}, medium :: {}, dark :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Extra shot" :: Boolean, "Decaf" :: Boolean } -> { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], "Size" :: [ small :: {}, medium :: {}, large :: {} ], "Milk" :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], "Roast" :: [ light :: {}, medium :: {}, dark :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Extra shot" :: Boolean, "Decaf" :: Boolean }
espressoNoFrills order = order { drink = .espresso {}, "Size" = .small {}, "Milk" = .none {}, "Sugar" = sugars 0.0, "Extra shot" = false, "Decaf" = false }

brewedLine :: { customer :: String, drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], "Size" :: [ small :: {}, medium :: {}, large :: {} ], "Milk" :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], "Roast" :: [ light :: {}, medium :: {}, dark :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Extra shot" :: Boolean, "Decaf" :: Boolean, takeaway :: Boolean, favorite :: Boolean, "Loyalty" :: Maybe {} } -> String
brewedLine { customer, drink, "Size": size, "Milk": milk, "Roast": roast, "Sugar": sugar, "Extra shot": extraShot, "Decaf": decaf, takeaway, favorite, "Loyalty": loyalty } =
  "Coming right up" <> forCustomer { customer }
    <> ": " <> summaryText { drink, "Size": size, "Milk": milk, "Roast": roast, "Sugar": sugar, "Extra shot": extraShot, "Decaf": decaf, takeaway, "Loyalty": loyalty }
    <> (if favorite then " ★" else "")

forCustomer :: { customer :: String } -> String
forCustomer { customer } = case trim customer of
  "" -> ""
  name -> ", " <> name

summaryText :: { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], "Size" :: [ small :: {}, medium :: {}, large :: {} ], "Milk" :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], "Roast" :: [ light :: {}, medium :: {}, dark :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, "Extra shot" :: Boolean, "Decaf" :: Boolean, takeaway :: Boolean, "Loyalty" :: Maybe {} } -> String
summaryText { drink, "Size": size, "Milk": milk, "Roast": roast, "Sugar": sugar, "Extra shot": extraShot, "Decaf": decaf, takeaway, "Loyalty": loyalty } =
  sizeText size <> " " <> drinkText drink
    <> milkText milk
    <> ", " <> roastText roast
    <> (if extraShot then ", extra shot" else "")
    <> (if decaf then ", decaf" else "")
    <> sugarsText sugar.current
    <> (if takeaway then ", to go" else "")
    <> " — " <> money (price { "Size": size, "Milk": milk, "Extra shot": extraShot, "Loyalty": loyalty })

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

price :: { "Size" :: [ small :: {}, medium :: {}, large :: {} ], "Milk" :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], "Extra shot" :: Boolean, "Loyalty" :: Maybe {} } -> Number
price { "Size": size, "Milk": milk, "Extra shot": extraShot, "Loyalty": loyalty } = discounted (sizePrice size + milkPrice milk + (if extraShot then 0.5 else 0.0))
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

caffeineFraction :: { drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], "Extra shot" :: Boolean, "Decaf" :: Boolean } -> Number
caffeineFraction { drink, "Extra shot": extraShot, "Decaf": decaf }
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
