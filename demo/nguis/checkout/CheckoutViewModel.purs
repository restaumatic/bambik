module CheckoutViewModel (cartLine, cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, paymentLine, placedLine, previousOf, shippingLine) where

import Prelude (identity, (<>))

import Data.Variant (match)

freshOrder :: { item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , status: .pending {}
  }

cartStep :: [ cart :: {}, shipping :: {}, payment :: {} ]
cartStep = .cart {}

checkoutStep :: forall r1. { item :: String, address :: String, card :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] | r1 } -> [ cart :: { item :: String }, shipping :: { address :: String }, payment :: { card :: String } ]
checkoutStep { item, address, card, step } = match
  { cart: \_ -> .cart { item }
  , shipping: \_ -> .shipping { address }
  , payment: \_ -> .payment { card }
  } step

cartLine :: forall r1. { item :: String | r1 } -> String
cartLine { item } = "Step 1 of 3 — Cart: " <> item

shippingLine :: forall r1. { address :: String | r1 } -> String
shippingLine { address } = "Step 2 of 3 — Shipping to " <> address

paymentLine :: forall r1. { card :: String | r1 } -> String
paymentLine { card } = "Step 3 of 3 — Pay with card " <> card

onwardFrom :: forall r1. { step :: [ cart :: {}, shipping :: {}, payment :: {} ] | r1 } -> [ onward :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, last :: {} ]
onwardFrom { step } = match
  { cart: \_ -> .onward { step: .shipping {} }
  , shipping: \_ -> .onward { step: .payment {} }
  , payment: \_ -> .last {}
  } step

previousOf :: forall r1. { step :: [ cart :: {}, shipping :: {}, payment :: {} ] | r1 } -> [ back :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, first :: {} ]
previousOf { step } = match
  { cart: \_ -> .first {}
  , shipping: \_ -> .back { step: .cart {} }
  , payment: \_ -> .back { step: .shipping {} }
  } step

goneOn :: [ "Next" :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } ] -> { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
goneOn = match { "Next": identity }

goneBack :: [ "Back" :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } ] -> { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
goneBack = match { "Back": identity }

orderPlaced :: forall r. { status :: [ pending :: {}, placed :: {} ] | r } -> { status :: [ pending :: {}, placed :: {} ] | r }
orderPlaced order = order { status = .placed {} }

orderStatus :: forall r1. { item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] | r1 } -> [ pending :: {}, placed :: { item :: String, address :: String, card :: String } ]
orderStatus { item, address, card, status } = match { pending: \_ -> .pending {}, placed: \_ -> .placed { item, address, card } } status

placedLine :: forall r1. { item :: String, address :: String, card :: String | r1 } -> String
placedLine { item, address, card } = "Order placed: " <> item <> " → " <> address <> " (card " <> card <> ")"
