module Checkout (checkout) where

import Prelude ((#), ($), (<>), (==), Unit)

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (announce, asCase, displayed, mvu, projection, tapped, updatesOn, widenRecordInput)
import PUI.HTML (body, provided, text)
import PUI.MDC (body2, button, card, elevation20)
import QualifiedDo.Semigroupoid as Semigroupoid

checkout :: Effect Unit
checkout =
  body $
    elevation20 $
      card { caption: "Checkout" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              body2 text # projection cartLine # widenRecordInput # provided # lcmap atCart # displayed
              body2 text # projection shippingLine # widenRecordInput # provided # lcmap atShipping # displayed
              body2 text # projection paymentLine # widenRecordInput # provided # lcmap atPayment # displayed
              RecordToVariant.do
                announce cartStep
                button { label: "Next" } # asCase @"next" # provided # lcmap nextAtCart
                button { label: "Next" } # asCase @"next" # provided # lcmap nextAtShipping
                button { label: "Back" } # asCase @"next" # provided # lcmap backAtShipping
                button { label: "Back" } # asCase @"next" # provided # lcmap backAtPayment
                button { label: "Place order", icon: "shopping_cart_checkout" } # asCase @"placed" # provided # lcmap placeAtPayment) # folding @"next" # widenRecordInput # updatesOn (match { placed: recordPlaced })
          body2 text # projection _.confirmation # tapped
      ) # mvu freshOrder

cartStep ::
  [ placed :: { summary :: String }
  , next :: { step :: String }
  ]
cartStep = .next { step: "cart" }

cartLine :: { item :: String } -> String
cartLine r = "Step 1 of 3 — Cart: " <> r.item

shippingLine :: { address :: String } -> String
shippingLine r = "Step 2 of 3 — Shipping to " <> r.address

paymentLine :: { card :: String } -> String
paymentLine r = "Step 3 of 3 — Pay with card " <> r.card

atCart :: forall r. { step :: String | r } -> Maybe { step :: String | r }
atCart r = if r.step == "cart" then Just r else Nothing

atShipping :: forall r. { step :: String | r } -> Maybe { step :: String | r }
atShipping r = if r.step == "shipping" then Just r else Nothing

atPayment :: forall r. { step :: String | r } -> Maybe { step :: String | r }
atPayment r = if r.step == "payment" then Just r else Nothing

nextAtCart :: { step :: String } -> Maybe { step :: String }
nextAtCart r = if r.step == "cart" then Just { step: "shipping" } else Nothing

nextAtShipping :: { step :: String } -> Maybe { step :: String }
nextAtShipping r = if r.step == "shipping" then Just { step: "payment" } else Nothing

backAtShipping :: { step :: String } -> Maybe { step :: String }
backAtShipping r = if r.step == "shipping" then Just { step: "cart" } else Nothing

backAtPayment :: { step :: String } -> Maybe { step :: String }
backAtPayment r = if r.step == "payment" then Just { step: "shipping" } else Nothing

placeAtPayment :: { item :: String, address :: String, card :: String, step :: String } -> Maybe { summary :: String }
placeAtPayment r =
  if r.step == "payment"
    then Just { summary: "Order placed: " <> r.item <> " → " <> r.address <> " (card " <> r.card <> ")" }
    else Nothing

recordPlaced :: { summary :: String } -> { confirmation :: String } -> { confirmation :: String }
recordPlaced r o = o { confirmation = r.summary }

freshOrder :: { item :: String, address :: String, card :: String, confirmation :: String }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , confirmation: ""
  }
