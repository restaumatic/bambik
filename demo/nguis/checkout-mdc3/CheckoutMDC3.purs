module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit, const)

import CheckoutLogic (atCart, atPayment, atShipping, cartStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, placeAtPayment, placedOrder, previousOf)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCases, updated)
import PUI.Web.HTML (shownWhen, body, provided, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    elevation5 $
      card $ ( Semigroupoid.do
          ( Semigroupoid.do
              shownWhen atCart ( bodyMedium $ RecordToRecord.do
                  staticText "Step 1 of 3 — Cart: "
                  text @"item" )
              shownWhen atShipping ( bodyMedium $ RecordToRecord.do
                  staticText "Step 2 of 3 — Shipping to "
                  text @"address" )
              shownWhen atPayment ( bodyMedium $ RecordToRecord.do
                  staticText "Step 3 of 3 — Pay with card "
                  text @"card" )
              RecordToVariant.do
                button @"Next" {} # toCases goneOn # provided onwardFrom
                button @"Back" {} # toCases goneBack # provided previousOf
                button @"Place order" { icon: "shopping_cart_checkout" } # provided placeAtPayment) # folding @"next" cartStep # updated (match { "Place order": const (const orderPlaced) })
          shownWhen placedOrder ( bodyMedium $ RecordToRecord.do
              staticText "Order placed: "
              text @"item"
              staticText " → "
              text @"address"
              staticText " (card "
              text @"card"
              staticText ")" )
      ) # mvu freshOrder
