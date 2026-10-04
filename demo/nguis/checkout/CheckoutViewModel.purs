module CheckoutViewModel (cartLine, cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, paymentLine, placedLine, previousOf, shippingLine) where

import Prelude (identity, (<>))

import Data.Variant (match)

freshOrder :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ] }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , status: .pending {}
  }

cartStep :: [ cart :: {}, payment :: {}, shipping :: {} ]
cartStep = .cart {}

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

goneOn :: [ "Next" :: { step :: [ cart :: {}, payment :: {}, shipping :: {} ] } ] -> { step :: [ cart :: {}, payment :: {}, shipping :: {} ] }
goneOn = match { "Next": identity }

goneBack :: [ "Back" :: { step :: [ cart :: {}, payment :: {}, shipping :: {} ] } ] -> { step :: [ cart :: {}, payment :: {}, shipping :: {} ] }
goneBack = match { "Back": identity }

orderPlaced :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ] } -> { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ] }
orderPlaced order = order { status = .placed {} }

orderStatus :: { address :: String, card :: String, item :: String, status :: [ pending :: {}, placed :: {} ] } -> [ pending :: {}, placed :: { address :: String, card :: String, item :: String } ]
orderStatus { item, address, card, status } = match { pending: \_ -> .pending {}, placed: \_ -> .placed { item, address, card } } status

placedLine :: { address :: String, card :: String, item :: String } -> String
placedLine { item, address, card } = "Order placed: " <> item <> " → " <> address <> " (card " <> card <> ")"
