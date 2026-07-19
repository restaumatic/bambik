module Checkout (checkout) where

import Prelude ((#), ($), (<>), (==), Unit)

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, asCase, displayed, forValue, mvu, projection, tapped, updates)
import PUI.HTML (body, provided, text)
import PUI.MDC (body2, button, card, elevation20)
import QualifiedDo.Semigroupoid as Semigroupoid

type Order =
  { item :: String
  , address :: String
  , card :: String
  , confirmation :: String
  }

type Step =
  { item :: String
  , address :: String
  , card :: String
  , confirmation :: String
  , step :: String
  }

checkout :: Effect Unit
checkout =
  body $
    elevation20 $
      card { caption: "Checkout" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              body2 (text # projection cartLine # forValue) # provided # lcmap atCart # displayed
              body2 (text # projection shippingLine # forValue) # provided # lcmap atShipping # displayed
              body2 (text # projection paymentLine # forValue) # provided # lcmap atPayment # displayed
              RecordToVariant.do
                announce cartStep
                button { label: "Next" } # asCase @"next" # provided # lcmap nextAtCart
                button { label: "Next" } # asCase @"next" # provided # lcmap nextAtShipping
                button { label: "Back" } # asCase @"next" # provided # lcmap backAtShipping
                button { label: "Back" } # asCase @"next" # provided # lcmap backAtPayment
                button { label: "Place order", icon: "shopping_cart_checkout" } # asCase @"placed" # provided # lcmap placeAtPayment
          ) # folding @"next" # updates (match { placed: recordPlaced })
          body2 (text # projection _.confirmation # forValue) # tapped
      ) # mvu freshOrder

cartStep ::
  [ placed :: { summary :: String }
  , next :: { step :: String }
  ]
cartStep = .next { step: "cart" }

cartLine :: Step -> String
cartLine r = "Step 1 of 3 — Cart: " <> r.item

shippingLine :: Step -> String
shippingLine r = "Step 2 of 3 — Shipping to " <> r.address

paymentLine :: Step -> String
paymentLine r = "Step 3 of 3 — Pay with card " <> r.card

atCart :: Step -> Maybe Step
atCart r = if r.step == "cart" then Just r else Nothing

atShipping :: Step -> Maybe Step
atShipping r = if r.step == "shipping" then Just r else Nothing

atPayment :: Step -> Maybe Step
atPayment r = if r.step == "payment" then Just r else Nothing

nextAtCart :: Step -> Maybe { step :: String }
nextAtCart r = if r.step == "cart" then Just { step: "shipping" } else Nothing

nextAtShipping :: Step -> Maybe { step :: String }
nextAtShipping r = if r.step == "shipping" then Just { step: "payment" } else Nothing

backAtShipping :: Step -> Maybe { step :: String }
backAtShipping r = if r.step == "shipping" then Just { step: "cart" } else Nothing

backAtPayment :: Step -> Maybe { step :: String }
backAtPayment r = if r.step == "payment" then Just { step: "shipping" } else Nothing

placeAtPayment :: Step -> Maybe { summary :: String }
placeAtPayment r =
  if r.step == "payment"
    then Just { summary: "Order placed: " <> r.item <> " → " <> r.address <> " (card " <> r.card <> ")" }
    else Nothing

recordPlaced :: { summary :: String } -> Order -> Order
recordPlaced r o = o { confirmation = r.summary }

freshOrder :: Order
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , confirmation: ""
  }
