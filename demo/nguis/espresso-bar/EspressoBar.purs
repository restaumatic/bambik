module EspressoBar (Drink, Loyalty, Milk, Order, Roast, Size, Sugar, caffeineFraction, cupLine, espressoNoFrills, placeOrder, theUsual, usualOrder) where

import Prelude (discard, min, otherwise, pure, ($), (*), (+), (-), (<>), (==))

import Data.Number.Format (fixed, toStringWith)
import Data.String (toLower)
import Data.Variant (match, on)
import Data.Variant.Case (caseText)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Type.Proxy (Proxy(..))

type Order =
  { "Decaf" :: Boolean
  , "Drink" :: Drink
  , "Extra shot" :: Boolean
  , "Loyalty member" :: Loyalty
  , "Mark as favorite" :: Boolean
  , "Milk" :: Milk
  , "Roast" :: Roast
  , "Size" :: Size
  , "Sugar" :: Sugar
  , "Takeaway cup" :: Boolean
  , "Name" :: String
  }

type Drink = [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ]

type Size = [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ]

type Milk = [ "Whole milk" :: {}, "Oat milk" :: {}, "Almond milk" :: {}, "No milk" :: {} ]

type Roast = [ "Light" :: {}, "Medium" :: {}, "Dark" :: {} ]

type Sugar =
  { current :: Number
  , min :: Number
  , max :: Number
  , step :: [ discrete :: Number, continuous :: {} ]
  }

type Loyalty = [ member :: {}, guest :: {} ]

usualOrder :: Order
usualOrder =
  { "Name": ""
  , "Drink": ."Cappuccino" {}
  , "Size": ."Medium" {}
  , "Milk": ."Whole milk" {}
  , "Roast": ."Medium" {}
  , "Sugar": sugars 1.0
  , "Extra shot": false
  , "Decaf": false
  , "Takeaway cup": false
  , "Mark as favorite": false
  , "Loyalty member": .guest {}
  }

theUsual :: Order -> Order
theUsual order = order { "Drink" = ."Cappuccino" {}, "Size" = ."Medium" {}, "Milk" = ."Whole milk" {}, "Roast" = ."Medium" {}, "Sugar" = sugars 1.0, "Extra shot" = false, "Decaf" = false }

espressoNoFrills :: Order -> Order
espressoNoFrills order = order { "Drink" = ."Espresso" {}, "Size" = ."Small" {}, "Milk" = ."No milk" {}, "Sugar" = sugars 0.0, "Extra shot" = false, "Decaf" = false }

cupLine :: Order -> String
cupLine order = "Your cup: " <> summaryText order

summaryText
  :: forall r1.
     { "Drink" :: Drink
     , "Size" :: Size
     , "Milk" :: Milk
     , "Roast" :: Roast
     , "Sugar" :: Sugar
     , "Extra shot" :: Boolean
     , "Decaf" :: Boolean
     , "Takeaway cup" :: Boolean
     , "Loyalty member" :: Loyalty
     | r1
     }
  -> String
summaryText { "Drink": drink, "Size": size, "Milk": milk, "Roast": roast, "Sugar": sugar, "Extra shot": extraShot, "Decaf": decaf, "Takeaway cup": takeaway, "Loyalty member": loyalty } =
  caseText size <> " " <> caseText drink
    <> milkText milk
    <> ", " <> caseText roast <> " roast"
    <> (if extraShot then ", extra shot" else "")
    <> (if decaf then ", decaf" else "")
    <> sugarsText sugar.current
    <> (if takeaway then ", to go" else "")
    <> " — " <> money (price { "Size": size, "Milk": milk, "Extra shot": extraShot, "Loyalty member": loyalty })

milkText :: Milk -> String
milkText = on (Proxy @"No milk") (\_ -> " no milk") (\milk -> " with " <> toLower (caseText milk))

sugarsText :: Number -> String
sugarsText n
  | n == 0.0 = ""
  | n == 1.0 = ", 1 sugar"
  | otherwise = ", " <> toStringWith (fixed 0) n <> " sugars"

price
  :: forall r1.
     { "Size" :: Size, "Milk" :: Milk, "Extra shot" :: Boolean, "Loyalty member" :: Loyalty | r1 }
  -> Number
price { "Size": size, "Milk": milk, "Extra shot": extraShot, "Loyalty member": loyalty } = discounted (sizePrice size + milkPrice milk + (if extraShot then 0.5 else 0.0))
  where
  discounted p = match { member: \_ -> p * (1.0 - memberDiscount), guest: \_ -> p } loyalty

memberDiscount :: Number
memberDiscount = 0.1


sizePrice :: Size -> Number
sizePrice = match { "Small": \_ -> 3.0, "Medium": \_ -> 3.5, "Large": \_ -> 4.0 }

milkPrice :: Milk -> Number
milkPrice = match { "Whole milk": \_ -> 0.0, "Oat milk": \_ -> 0.4, "Almond milk": \_ -> 0.4, "No milk": \_ -> 0.0 }

money :: Number -> String
money n = "€" <> toStringWith (fixed 2) n

caffeineFraction :: Order -> Number
caffeineFraction { "Drink": drink, "Extra shot": extraShot, "Decaf": decaf }
  | decaf = 0.05
  | otherwise = min 1.0 (drinkShots drink + (if extraShot then 0.35 else 0.0))

drinkShots :: Drink -> Number
drinkShots = match { "Espresso": \_ -> 0.6, "Cappuccino": \_ -> 0.45, "Latte": \_ -> 0.3 }

sugars :: Number -> Sugar
sugars n = { current: n, min: noSugar, max: maxSugar, step: .discrete 1.0 }

noSugar :: Number
noSugar = 0.0

maxSugar :: Number
maxSugar = 4.0

placeOrder :: Order -> Aff [ "Order placed" :: {} ]
placeOrder _ = do
  delay (Milliseconds 800.0)
  pure $ ."Order placed" {}
