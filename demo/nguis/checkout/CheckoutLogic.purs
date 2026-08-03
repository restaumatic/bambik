module CheckoutLogic (atCart, atPayment, atShipping, backAtPayment, backAtShipping, cartStep, freshOrder, nextAtCart, nextAtShipping, orderPlaced, placeAtPayment, placedOrder) where

import Prelude ((==))

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

nextAtCart :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
nextAtCart { step } = if step == .cart {} then Just { step: .shipping {} } else Nothing

nextAtShipping :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
nextAtShipping { step } = if step == .shipping {} then Just { step: .payment {} } else Nothing

backAtShipping :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
backAtShipping { step } = if step == .shipping {} then Just { step: .cart {} } else Nothing

backAtPayment :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
backAtPayment { step } = if step == .payment {} then Just { step: .shipping {} } else Nothing

placeAtPayment :: { item :: String, address :: String, card :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe {}
placeAtPayment { step } = if step == .payment {} then Just {} else Nothing

orderPlaced :: { status :: [ pending :: {}, placed :: {} ] }
orderPlaced = { status: .placed {} }

placedOrder :: { item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] } -> Maybe { item :: String, address :: String, card :: String }
placedOrder { item, address, card, status } =
  match { placed: \_ -> Just { item, address, card }, pending: \_ -> Nothing } status
