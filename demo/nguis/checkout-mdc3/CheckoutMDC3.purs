module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), (==), Unit)

import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, displayed, forField, forValue, mvu, updates)
import PUI.HTML (body, provided, staticText, text)
import PUI.MDC3 (bodyMedium, button, card, elevation5)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    elevation5 $
      card { caption: "Checkout" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              bodyMedium ( RecordToRecord.do
                  staticText "Step 1 of 3 — Cart: "
                  text # forValue # forField @"item" ) # provided # lcmap atCart # displayed
              bodyMedium ( RecordToRecord.do
                  staticText "Step 2 of 3 — Shipping to "
                  text # forValue # forField @"address" ) # provided # lcmap atShipping # displayed
              bodyMedium ( RecordToRecord.do
                  staticText "Step 3 of 3 — Pay with card "
                  text # forValue # forField @"card" ) # provided # lcmap atPayment # displayed
              RecordToVariant.do
                button { label: "Next" } # asCase @"next" # provided # lcmap nextAtCart
                button { label: "Next" } # asCase @"next" # provided # lcmap nextAtShipping
                button { label: "Back" } # asCase @"next" # provided # lcmap backAtShipping
                button { label: "Back" } # asCase @"next" # provided # lcmap backAtPayment
                button { label: "Place order", icon: "shopping_cart_checkout" } # asCase @"placed" # provided # lcmap placeAtPayment) # folding @"next" cartStep # updates (match { placed: recordPlaced })
          bodyMedium ( RecordToRecord.do
              staticText "Order placed: "
              text # forValue # forField @"item"
              staticText " → "
              text # forValue # forField @"address"
              staticText " (card "
              text # forValue # forField @"card"
              staticText ")" ) # provided # lcmap placedOrder # displayed
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

placeAtPayment :: { item :: String, address :: String, card :: String, step :: [ cart :: {}, shipping :: {}, payment :: {} ] } -> Maybe { confirmed :: Boolean }
placeAtPayment { step } = if step == .payment {} then Just { confirmed: true } else Nothing

recordPlaced :: { confirmed :: Boolean } -> { confirmed :: Boolean } -> { confirmed :: Boolean }
recordPlaced { confirmed } o = o { confirmed = confirmed }

placedOrder :: { item :: String, address :: String, card :: String, confirmed :: Boolean } -> Maybe { item :: String, address :: String, card :: String }
placedOrder { item, address, card, confirmed } =
  if confirmed then Just { item, address, card } else Nothing

freshOrder :: { item :: String, address :: String, card :: String, confirmed :: Boolean }
freshOrder =
  { item: "Wireless Headphones"
  , address: "221B Baker Street"
  , card: "•••• 4242"
  , confirmed: false
  }
