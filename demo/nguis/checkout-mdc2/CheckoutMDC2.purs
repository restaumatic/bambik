module CheckoutMDC2 (checkoutMDC2) where

import Prelude (identity, (#), ($), Unit, const)

import CheckoutLogic (atCart, atPayment, atShipping, backAtPayment, backAtShipping, cartStep, freshOrder, nextAtCart, nextAtShipping, orderPlaced, placeAtPayment, placedOrder)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (displayed, mvu, updated)
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
                  text @"item" ) # provided atCart # displayed
              body2 ( RecordToRecord.do
                  staticText "Step 2 of 3 — Shipping to "
                  text @"address" ) # provided atShipping # displayed
              body2 ( RecordToRecord.do
                  staticText "Step 3 of 3 — Pay with card "
                  text @"card" ) # provided atPayment # displayed
              RecordToVariant.do
                button @"next" { label: "Next" } # provided nextAtCart
                button @"next" { label: "Next" } # provided nextAtShipping
                button @"next" { label: "Back" } # provided backAtShipping
                button @"next" { label: "Back" } # provided backAtPayment
                button @"placed" { label: "Place order", icon: "shopping_cart_checkout" } # provided placeAtPayment) # folding @"next" cartStep # updated (match { placed: const (const orderPlaced) })
          body2 ( RecordToRecord.do
              staticText "Order placed: "
              text @"item"
              staticText " → "
              text @"address"
              staticText " (card "
              text @"card"
              staticText ")" ) # provided placedOrder # displayed
      ) # mvu freshOrder
