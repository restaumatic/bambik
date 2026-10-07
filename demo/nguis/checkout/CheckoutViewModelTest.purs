module CheckoutViewModelTest (checkoutClaims) where

import Prelude ((==))

import CheckoutViewModel (cartLine, checkoutStep, freshOrder, onwardFrom, orderStatus, previousOf, stepTo)

checkoutClaims :: Array { claim :: String, holds :: Boolean }
checkoutClaims =
  [ { claim: "a fresh order starts at the cart", holds: checkoutStep freshOrder == .cart { item: "Wireless Headphones" } }
  , { claim: "from the cart, Next goes on to shipping", holds: onwardFrom freshOrder == .onward { step: .shipping {} } }
  , { claim: "the cart is the first step", holds: previousOf freshOrder == .first {} }
  , { claim: "stepping to shipping shows the address", holds: checkoutStep (stepTo { event: { step: .shipping {} }, model: freshOrder }) == .shipping { address: "221B Baker Street" } }
  , { claim: "a fresh order is pending", holds: orderStatus freshOrder == .pending {} }
  , { claim: "the cart line counts the steps", holds: cartLine { item: "Lamp" } == "Step 1 of 3 — Cart: Lamp" }
  ]
