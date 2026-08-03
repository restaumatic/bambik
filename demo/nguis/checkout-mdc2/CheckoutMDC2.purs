module CheckoutMDC2 (checkoutMDC2) where

import Prelude (identity, (#), ($), Unit, const)

import CheckoutLogic (atCart, atPayment, atShipping, backAtPayment, backAtShipping, cartStep, freshOrder, nextAtCart, nextAtShipping, orderPlaced, placeAtPayment, placedOrder)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, displayed, forField, mvu, updated)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC2 (body2, button, card, elevation20)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC2 :: Effect Unit
checkoutMDC2 =
  body $
    elevation20 $
      card { caption: "Checkout" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              body2 ( RecordToRecord.do
                  staticText "Step 1 of 3 — Cart: "
                  text # forField @"item" identity ) # provided atCart # displayed
              body2 ( RecordToRecord.do
                  staticText "Step 2 of 3 — Shipping to "
                  text # forField @"address" identity ) # provided atShipping # displayed
              body2 ( RecordToRecord.do
                  staticText "Step 3 of 3 — Pay with card "
                  text # forField @"card" identity ) # provided atPayment # displayed
              RecordToVariant.do
                button { label: "Next" } # asCase @"next" # provided nextAtCart
                button { label: "Next" } # asCase @"next" # provided nextAtShipping
                button { label: "Back" } # asCase @"next" # provided backAtShipping
                button { label: "Back" } # asCase @"next" # provided backAtPayment
                button { label: "Place order", icon: "shopping_cart_checkout" } # asCase @"placed" # provided placeAtPayment) # folding @"next" cartStep # updated (match { placed: const (const orderPlaced) })
          body2 ( RecordToRecord.do
              staticText "Order placed: "
              text # forField @"item" identity
              staticText " → "
              text # forField @"address" identity
              staticText " (card "
              text # forField @"card" identity
              staticText ")" ) # provided placedOrder # displayed
      ) # mvu freshOrder
