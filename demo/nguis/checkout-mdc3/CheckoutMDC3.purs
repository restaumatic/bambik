module CheckoutMDC3 (checkoutMDC3) where

import Prelude (identity, (#), ($), (==), Unit)

import Data.Maybe (Maybe(..))
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, displayed, forField, informed, mvu, updated)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    elevation5 $
      card { caption: "Checkout" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              bodyMedium ( RecordToRecord.do
                  staticText "Step 1 of 3 — Cart: "
                  text # forField @"item" identity ) # provided atCart # displayed
              bodyMedium ( RecordToRecord.do
                  staticText "Step 2 of 3 — Shipping to "
                  text # forField @"address" identity ) # provided atShipping # displayed
              bodyMedium ( RecordToRecord.do
                  staticText "Step 3 of 3 — Pay with card "
                  text # forField @"card" identity ) # provided atPayment # displayed
              RecordToVariant.do
                button { label: "Next" } # asCase @"next" # provided nextAtCart
                button { label: "Next" } # asCase @"next" # provided nextAtShipping
                button { label: "Back" } # asCase @"next" # provided backAtShipping
                button { label: "Back" } # asCase @"next" # provided backAtPayment
                button { label: "Place order", icon: "shopping_cart_checkout" } # asCase @"placed" # provided placeAtPayment) # folding @"next" cartStep # updated (match { placed: informed recordPlaced })
          bodyMedium ( RecordToRecord.do
              staticText "Order placed: "
              text # forField @"item" identity
              staticText " → "
              text # forField @"address" identity
              staticText " (card "
              text # forField @"card" identity
              staticText ")" ) # provided placedOrder # displayed
      ) # mvu freshOrder

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

placeAtPayment :: { item :: String, address :: String, card :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { status :: [ pending :: {}, placed :: {} ] }
placeAtPayment { step } = if step == .payment {} then Just { status: .placed {} } else Nothing

recordPlaced :: { status :: [ pending :: {}, placed :: {} ] } -> { status :: [ pending :: {}, placed :: {} ] }
recordPlaced { status } = { status }

placedOrder :: { item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] } -> Maybe { item :: String, address :: String, card :: String }
placedOrder { item, address, card, status } =
  match { placed: \_ -> Just { item, address, card }, pending: \_ -> Nothing } status

freshOrder :: { item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , status: .pending {}
  }
