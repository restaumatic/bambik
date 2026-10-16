module Checkout (Order, cartLine, freshOrder, orderPlaced, paymentLine, placedLine, shippingLine) where

import Prelude ((<>))

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
  , card: "•••• 4242"
  , status: .pending {}
  , step: ."Cart" {}
  }

cartLine :: Order -> String
cartLine { item } = "In your cart: " <> item

shippingLine :: Order -> String
shippingLine { address } = "Shipping to " <> address

paymentLine :: Order -> String
paymentLine { card } = "Pay with card " <> card

orderPlaced :: Order -> Order
orderPlaced model = model { status = .placed {} }

placedLine :: Order -> String
placedLine { item, address, card } = "Order placed: " <> item <> " → " <> address <> " (card " <> card <> ")"
