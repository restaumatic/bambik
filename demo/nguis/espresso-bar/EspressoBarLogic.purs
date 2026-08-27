module EspressoBarLogic (brewedLine, caffeineFraction, espressoNoFrills, summaryText, theUsual, usualOrder) where

import Prelude (min, otherwise, (*), (+), (<>), (==))

import Data.Number.Format (fixed, toStringWith)
import Data.String (trim)
import Data.Variant (match)
import Data.Variant.Case (caseText)

usualOrder :: { "Your name" :: String, drink :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ], "Size" :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ], "Milk" :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ], "Roast" :: [ "Light" :: {}, "Medium" :: {}, "Dark" :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Extra shot" :: Boolean, "Decaf" :: Boolean, "Takeaway cup" :: Boolean, "Mark as favorite" :: Boolean, "Loyalty" :: [ member :: {}, guest :: {} ] }
usualOrder =
  { "Your name": ""
  , drink: ."Cappuccino" {}
  , "Size": ."Medium" {}
  , "Milk": ."with whole milk" {}
  , "Roast": ."Medium" {}
  , "Sugar": sugars 1.0
  , "Extra shot": false
  , "Decaf": false
  , "Takeaway cup": false
  , "Mark as favorite": false
  , "Loyalty": .guest {}
  }

theUsual :: { drink :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ], "Size" :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ], "Milk" :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ], "Roast" :: [ "Light" :: {}, "Medium" :: {}, "Dark" :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Extra shot" :: Boolean, "Decaf" :: Boolean }
theUsual = { drink: ."Cappuccino" {}, "Size": ."Medium" {}, "Milk": ."with whole milk" {}, "Roast": ."Medium" {}, "Sugar": sugars 1.0, "Extra shot": false, "Decaf": false }

espressoNoFrills :: { drink :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ], "Size" :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ], "Milk" :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ], "Roast" :: [ "Light" :: {}, "Medium" :: {}, "Dark" :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Extra shot" :: Boolean, "Decaf" :: Boolean } -> { drink :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ], "Size" :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ], "Milk" :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ], "Roast" :: [ "Light" :: {}, "Medium" :: {}, "Dark" :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Extra shot" :: Boolean, "Decaf" :: Boolean }
espressoNoFrills order = order { drink = ."Espresso" {}, "Size" = ."Small" {}, "Milk" = ."no milk" {}, "Sugar" = sugars 0.0, "Extra shot" = false, "Decaf" = false }

brewedLine :: { "Your name" :: String, drink :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ], "Size" :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ], "Milk" :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ], "Roast" :: [ "Light" :: {}, "Medium" :: {}, "Dark" :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Extra shot" :: Boolean, "Decaf" :: Boolean, "Takeaway cup" :: Boolean, "Mark as favorite" :: Boolean, "Loyalty" :: [ member :: {}, guest :: {} ] } -> String
brewedLine { "Your name": customer, drink, "Size": size, "Milk": milk, "Roast": roast, "Sugar": sugar, "Extra shot": extraShot, "Decaf": decaf, "Takeaway cup": takeaway, "Mark as favorite": favorite, "Loyalty": loyalty } =
  "Coming right up" <> forCustomer { "Your name": customer }
    <> ": " <> summaryText { drink, "Size": size, "Milk": milk, "Roast": roast, "Sugar": sugar, "Extra shot": extraShot, "Decaf": decaf, "Takeaway cup": takeaway, "Loyalty": loyalty }
    <> (if favorite then " ★" else "")

forCustomer :: { "Your name" :: String } -> String
forCustomer { "Your name": customer } = case trim customer of
  "" -> ""
  name -> ", " <> name

summaryText :: { drink :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ], "Size" :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ], "Milk" :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ], "Roast" :: [ "Light" :: {}, "Medium" :: {}, "Dark" :: {} ], "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }, "Extra shot" :: Boolean, "Decaf" :: Boolean, "Takeaway cup" :: Boolean, "Loyalty" :: [ member :: {}, guest :: {} ] } -> String
summaryText { drink, "Size": size, "Milk": milk, "Roast": roast, "Sugar": sugar, "Extra shot": extraShot, "Decaf": decaf, "Takeaway cup": takeaway, "Loyalty": loyalty } =
  caseText size <> " " <> caseText drink
    <> " " <> caseText milk
    <> ", " <> caseText roast <> " roast"
    <> (if extraShot then ", extra shot" else "")
    <> (if decaf then ", decaf" else "")
    <> sugarsText sugar.current
    <> (if takeaway then ", to go" else "")
    <> " — " <> money (price { "Size": size, "Milk": milk, "Extra shot": extraShot, "Loyalty": loyalty })

sugarsText :: Number -> String
sugarsText n
  | n == 0.0 = ""
  | n == 1.0 = ", 1 sugar"
  | otherwise = ", " <> toStringWith (fixed 0) n <> " sugars"

price :: { "Size" :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ], "Milk" :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ], "Extra shot" :: Boolean, "Loyalty" :: [ member :: {}, guest :: {} ] } -> Number
price { "Size": size, "Milk": milk, "Extra shot": extraShot, "Loyalty": loyalty } = discounted (sizePrice size + milkPrice milk + (if extraShot then 0.5 else 0.0))
  where
  discounted p = match { member: \_ -> p * 0.9, guest: \_ -> p } loyalty

sizePrice :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ] -> Number
sizePrice = match { "Small": \_ -> 3.0, "Medium": \_ -> 3.5, "Large": \_ -> 4.0 }

milkPrice :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ] -> Number
milkPrice = match { "with whole milk": \_ -> 0.0, "with oat milk": \_ -> 0.4, "with almond milk": \_ -> 0.4, "no milk": \_ -> 0.0 }

money :: Number -> String
money n = "€" <> toStringWith (fixed 2) n

caffeineFraction :: { drink :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ], "Extra shot" :: Boolean, "Decaf" :: Boolean } -> Number
caffeineFraction { drink, "Extra shot": extraShot, "Decaf": decaf }
  | decaf = 0.05
  | otherwise = min 1.0 (drinkShots drink + (if extraShot then 0.35 else 0.0))

drinkShots :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ] -> Number
drinkShots = match { "Espresso": \_ -> 0.6, "Cappuccino": \_ -> 0.45, "Latte": \_ -> 0.3 }

sugars :: Number -> { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
sugars n = { current: n, min: noSugar, max: maxSugar, step: .discrete 1.0 }

noSugar :: Number
noSugar = 0.0

maxSugar :: Number
maxSugar = 4.0
