module CheckoutLogic (cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, previousOf) where

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

-- the wizard's position, each step carrying what its pane reviews
checkoutStep :: { item :: String, address :: String, card :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> [ cart :: { item :: String }, shipping :: { address :: String }, payment :: { card :: String } ]
checkoutStep { item, address, card, step } = match
  { cart: \_ -> .cart { item }
  , shipping: \_ -> .shipping { address }
  , payment: \_ -> .payment { card }
  } step

-- the step a wizard button leads to: onward while there is one ahead,
-- back while there is one behind — the last and first steps have no button
onwardFrom :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> [ onward :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, last :: {} ]
onwardFrom { step } = match
  { cart: \_ -> .onward { step: .shipping {} }
  , shipping: \_ -> .onward { step: .payment {} }
  , payment: \_ -> .last {}
  } step

previousOf :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> [ back :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, first :: {} ]
previousOf { step } = match
  { cart: \_ -> .first {}
  , shipping: \_ -> .back { step: .cart {} }
  , payment: \_ -> .back { step: .shipping {} }
  } step

-- each button's own case becomes the fold's loop case: the step it carries
-- is where the wizard resumes
goneOn :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> [ next :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } ]
goneOn resumed = .next resumed

goneBack :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> [ next :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } ]
goneBack resumed = .next resumed

orderPlaced :: { status :: [ pending :: {}, placed :: {} ] }
orderPlaced = { status: .placed {} }

-- the order's status, a placed order carrying its receipt
orderStatus :: { item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] } -> [ pending :: {}, placed :: { item :: String, address :: String, card :: String } ]
orderStatus { item, address, card, status } = match { pending: \_ -> .pending {}, placed: \_ -> .placed { item, address, card } } status
