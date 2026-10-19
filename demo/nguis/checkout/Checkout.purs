module Checkout (Order, cartLine, checkoutStage, freshOrder, orderPlaced, paymentLine, placedLine, shippingLine) where

import Prelude ((<>))

import Data.Variant (match)

type Order =
  { address :: String
  , card :: String
  , item :: String
  , status :: [ pending :: {}, placed :: {} ]
  , step :: [ "Cart" :: {}, "Payment" :: {}, "Shipping" :: {} ]
  }

freshOrder :: Order
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "4242"
  , status: .pending {}
  , step: ."Cart" {}
  }

cartLine :: Order -> String
cartLine { item } = "In your cart: " <> item

shippingLine :: Order -> String
shippingLine { address } = "Shipping to " <> address

paymentLine :: Order -> String
paymentLine { card } = "Pay with card •••• " <> card

orderPlaced :: Order -> Order
orderPlaced order = order { status = .placed {} }

checkoutStage :: Order -> [ paying :: {}, placed :: {}, preparing :: {} ]
checkoutStage { status, step } = match
  { placed: \_ -> .placed {}
  , pending: \_ -> match { "Payment": \_ -> .paying {}, "Cart": \_ -> .preparing {}, "Shipping": \_ -> .preparing {} } step
  } status

placedLine :: Order -> String
placedLine { item, address, card } = "Order placed: " <> item <> ", shipping to " <> address <> ", paid with card •••• " <> card
