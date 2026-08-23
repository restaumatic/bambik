module CheckoutLogic (atCart, atPayment, atShipping, cartLine, cartStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, placeAtPayment, placedLine, placedOrder, paymentLine, previousOf, shippingLine) where

import Prelude ((<>), (==))

import Data.Maybe (Maybe(..))
import Data.Variant (match)

freshOrder :: { item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , status: .pending {}
  }

cartStep :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
cartStep = { step: .cart {} }

atCart :: { item :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { item :: String }
atCart { item, step } = if step == .cart {} then Just { item } else Nothing

atShipping :: { address :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { address :: String }
atShipping { address, step } = if step == .shipping {} then Just { address } else Nothing

atPayment :: { card :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { card :: String }
atPayment { card, step } = if step == .payment {} then Just { card } else Nothing

-- the step a wizard button leads to: forward while there is one ahead,
-- backward while there is one behind — Nothing hides the button
onwardFrom :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
onwardFrom { step } = match
  { cart: \_ -> Just { step: .shipping {} }
  , shipping: \_ -> Just { step: .payment {} }
  , payment: \_ -> Nothing
  } step

previousOf :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
previousOf { step } = match
  { cart: \_ -> Nothing
  , shipping: \_ -> Just { step: .cart {} }
  , payment: \_ -> Just { step: .shipping {} }
  } step

-- each button's own case becomes the fold's loop case: the step it carries
-- is where the wizard resumes
goneOn :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> [ next :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } ]
goneOn resumed = .next resumed

goneBack :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> [ next :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } ]
goneBack resumed = .next resumed

placeAtPayment :: { item :: String, address :: String, card :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe {}
placeAtPayment { step } = if step == .payment {} then Just {} else Nothing

orderPlaced :: { status :: [ pending :: {}, placed :: {} ] }
orderPlaced = { status: .placed {} }

placedOrder :: { item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] } -> Maybe { item :: String, address :: String, card :: String }
placedOrder { item, address, card, status } =
  match { placed: \_ -> Just { item, address, card }, pending: \_ -> Nothing } status

cartLine :: { item :: String } -> String
cartLine { item } = "Step 1 of 3 \x2014 Cart: " <> item

shippingLine :: { address :: String } -> String
shippingLine { address } = "Step 2 of 3 \x2014 Shipping to " <> address

paymentLine :: { card :: String } -> String
paymentLine { card } = "Step 3 of 3 \x2014 Pay with card " <> card

placedLine :: { item :: String, address :: String, card :: String } -> String
placedLine { item, address, card } = "Order placed: " <> item <> " \x2192 " <> address <> " (card " <> card <> ")"
