module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit, const)

import CheckoutLogic (atCart, atPayment, atShipping, cartStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, placeAtPayment, placedOrder, previousOf)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (tapped, mvu, toCases, updated)
import PUI.Web.HTML (body, provided, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          ( Semigroupoid.do
              bodyMedium ( RecordToRecord.do
                  staticText "Step 1 of 3 — Cart: "
                  text @"item" ) # provided atCart # tapped
              bodyMedium ( RecordToRecord.do
                  staticText "Step 2 of 3 — Shipping to "
                  text @"address" ) # provided atShipping # tapped
              bodyMedium ( RecordToRecord.do
                  staticText "Step 3 of 3 — Pay with card "
                  text @"card" ) # provided atPayment # tapped
              RecordToVariant.do
                button @"Next" {} # toCases goneOn # provided onwardFrom
                button @"Back" {} # toCases goneBack # provided previousOf
                button @"Place order" { icon: "shopping_cart_checkout" } # provided placeAtPayment) # folding @"next" cartStep # updated (match { "Place order": const (const orderPlaced) })
          bodyMedium ( RecordToRecord.do
              staticText "Order placed: "
              text @"item"
              staticText " → "
              text @"address"
              staticText " (card "
              text @"card"
              staticText ")" ) # provided placedOrder # tapped
      ) # mvu freshOrder
