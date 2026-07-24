module Checkout (checkout) where

import Prelude ((#), ($), (<>), (==), Unit)

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, asCase, displayed, mvu, projection, tapped, updates)
import PUI.HTML (body, provided, text)
import PUI.MDC (body2, button, card, elevation20)
import QualifiedDo.Semigroupoid as Semigroupoid

checkout :: Effect Unit
checkout =
  body $
    elevation20 $
      card { caption: "Checkout" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              body2 text # projection cartLine # provided # lcmap atCart # displayed
              body2 text # projection shippingLine # provided # lcmap atShipping # displayed
              body2 text # projection paymentLine # provided # lcmap atPayment # displayed
              RecordToVariant.do
                announce cartStep
                button { label: "Next" } # asCase @"next" # provided # lcmap nextAtCart
                button { label: "Next" } # asCase @"next" # provided # lcmap nextAtShipping
                button { label: "Back" } # asCase @"next" # provided # lcmap backAtShipping
                button { label: "Back" } # asCase @"next" # provided # lcmap backAtPayment
                button { label: "Place order", icon: "shopping_cart_checkout" } # asCase @"placed" # provided # lcmap placeAtPayment) # folding @"next" # updates (match { placed: recordPlaced })
          body2 text # projection _.confirmation # tapped
      ) # mvu freshOrder

cartStep ::
  [ placed :: { summary :: String }
  , next :: { step :: String }
  ]
cartStep = .next { step: "cart" }

cartLine :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> String
cartLine r = "Step 1 of 3 — Cart: " <> r.item

shippingLine :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> String
shippingLine r = "Step 2 of 3 — Shipping to " <> r.address

paymentLine :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> String
paymentLine r = "Step 3 of 3 — Pay with card " <> r.card

atCart :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> Maybe { item :: String, address :: String, card :: String, confirmation :: String, step :: String }
atCart r = if r.step == "cart" then Just r else Nothing

atShipping :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> Maybe { item :: String, address :: String, card :: String, confirmation :: String, step :: String }
atShipping r = if r.step == "shipping" then Just r else Nothing

atPayment :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> Maybe { item :: String, address :: String, card :: String, confirmation :: String, step :: String }
atPayment r = if r.step == "payment" then Just r else Nothing

nextAtCart :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> Maybe { step :: String }
nextAtCart r = if r.step == "cart" then Just { step: "shipping" } else Nothing

nextAtShipping :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> Maybe { step :: String }
nextAtShipping r = if r.step == "shipping" then Just { step: "payment" } else Nothing

backAtShipping :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> Maybe { step :: String }
backAtShipping r = if r.step == "shipping" then Just { step: "cart" } else Nothing

backAtPayment :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> Maybe { step :: String }
backAtPayment r = if r.step == "payment" then Just { step: "shipping" } else Nothing

placeAtPayment :: { item :: String, address :: String, card :: String, confirmation :: String, step :: String } -> Maybe { summary :: String }
placeAtPayment r =
  if r.step == "payment"
    then Just { summary: "Order placed: " <> r.item <> " → " <> r.address <> " (card " <> r.card <> ")" }
    else Nothing

recordPlaced :: { summary :: String } -> { item :: String, address :: String, card :: String, confirmation :: String } -> { item :: String, address :: String, card :: String, confirmation :: String }
recordPlaced r o = o { confirmation = r.summary }

freshOrder :: { item :: String, address :: String, card :: String, confirmation :: String }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , confirmation: ""
  }
