module CheckoutViewModel (cartLine, checkoutStep, freshOrder, onwardFrom, orderPlaced, orderPlacedLine, orderStatus, paymentLine, placedLine, previousOf, shippingLine, stepTo, steppedBackLine, steppedOnLine) where

import Prelude ((<>))

import Data.Variant (match)

freshOrder :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , status: .pending {}
  , step: .cart {}
  }

checkoutStep :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } -> [ cart :: { item :: String }, payment :: { card :: String }, shipping :: { address :: String } ]
checkoutStep { item, address, card, step } = match
  { cart: \_ -> .cart { item }
  , shipping: \_ -> .shipping { address }
  , payment: \_ -> .payment { card }
  } step

cartLine :: { item :: String } -> String
cartLine { item } = "Step 1 of 3 — Cart: " <> item

shippingLine :: { address :: String } -> String
shippingLine { address } = "Step 2 of 3 — Shipping to " <> address

paymentLine :: { card :: String } -> String
paymentLine { card } = "Step 3 of 3 — Pay with card " <> card

onwardFrom :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } -> [ last :: {}, onward :: { step :: [ cart :: {}, payment :: {}, shipping :: {} ] } ]
onwardFrom { step } = match
  { cart: \_ -> .onward { step: .shipping {} }
  , shipping: \_ -> .onward { step: .payment {} }
  , payment: \_ -> .last {}
  } step

previousOf :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } -> [ back :: { step :: [ cart :: {}, payment :: {}, shipping :: {} ] }, first :: {} ]
previousOf { step } = match
  { cart: \_ -> .first {}
  , shipping: \_ -> .back { step: .cart {} }
  , payment: \_ -> .back { step: .shipping {} }
  } step

stepTo :: { event :: { step :: [ cart :: {}, payment :: {}, shipping :: {} ] }, model :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } } -> { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] }
stepTo { event: { step }, model } = model { step = step }

orderPlaced :: { event :: { card :: String }, model :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } } -> { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] }
orderPlaced { model } = model { status = .placed {} }

orderStatus :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } -> [ pending :: {}, placed :: { address :: String, card :: String, item :: String } ]
orderStatus { item, address, card, status } = match { pending: \_ -> .pending {}, placed: \_ -> .placed { item, address, card } } status

placedLine :: { address :: String, card :: String, item :: String } -> String
placedLine { item, address, card } = "Order placed: " <> item <> " → " <> address <> " (card " <> card <> ")"

steppedOnLine :: { event :: { step :: [ cart :: {}, payment :: {}, shipping :: {} ] }, model :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } } -> String
steppedOnLine { event: { step } } = "Went on to " <> stepName step

steppedBackLine :: { event :: { step :: [ cart :: {}, payment :: {}, shipping :: {} ] }, model :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } } -> String
steppedBackLine { event: { step } } = "Went back to " <> stepName step

orderPlacedLine :: { event :: { card :: String }, model :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, payment :: {}, shipping :: {} ] } } -> String
orderPlacedLine { event: { card }, model: { item } } = "Placed the order for " <> item <> ", paid with card " <> card

stepName :: [ cart :: {}, payment :: {}, shipping :: {} ] -> String
stepName = match { cart: \_ -> "the cart", shipping: \_ -> "shipping", payment: \_ -> "payment" }
