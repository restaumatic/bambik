module Checkout (cartLine, freshOrder, onwardFrom, orderPlaced, paymentLine, placedLine, previousOf, shippingLine, stepBack, stepOnward) where

import Prelude ((<>))

import Data.Variant (match)

freshOrder
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , status: .pending {}
  , step: .cart {}
  }

cartLine
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> String
cartLine { item } = "Step 1 of 3 — Cart: " <> item

shippingLine
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> String
shippingLine { address } = "Step 2 of 3 — Shipping to " <> address

paymentLine
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> String
paymentLine { card } = "Step 3 of 3 — Pay with card " <> card

onwardFrom
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> [ last :: {}, onward :: {} ]
onwardFrom { step } = match
  { cart: \_ -> .onward {}
  , shipping: \_ -> .onward {}
  , payment: \_ -> .last {}
  } step

previousOf
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> [ back :: {}, first :: {} ]
previousOf { step } = match
  { cart: \_ -> .first {}
  , shipping: \_ -> .back {}
  , payment: \_ -> .back {}
  } step

stepOnward
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
stepOnward model@{ step } = model
  { step = match
    { cart: \_ -> .shipping {}
    , shipping: \_ -> .payment {}
    , payment: \_ -> .payment {}
    } step
  }

stepBack
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
stepBack model@{ step } = model
  { step = match
    { cart: \_ -> .cart {}
    , shipping: \_ -> .cart {}
    , payment: \_ -> .shipping {}
    } step
  }

orderPlaced
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
orderPlaced model = model { status = .placed {} }

placedLine
  :: { address :: String
     , card :: String
     , item :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, payment :: {}, shipping :: {} ]
     }
  -> String
placedLine { item, address, card } = "Order placed: " <> item <> " → " <> address <> " (card " <> card <> ")"
