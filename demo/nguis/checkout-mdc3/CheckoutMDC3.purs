module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit, const)

import CheckoutLogic (cartLine, cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, paymentLine, placedLine, previousOf, shippingLine)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCase, updated)
import PUI.Web (provided, shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, button)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        ( bodyMedium $ text cartLine ) # shownWhen @"cart" checkoutStep
        ( bodyMedium $ text shippingLine ) # shownWhen @"shipping" checkoutStep
        ( bodyMedium $ text paymentLine ) # shownWhen @"payment" checkoutStep
        RecordToVariant.do
          button @"Next" {} # toCase @"next" goneOn # provided @"onward" onwardFrom
          button @"Back" {} # toCase @"next" goneBack # provided @"back" previousOf
          button @"Place order" { icon: "shopping_cart_checkout" } # provided @"payment" checkoutStep ) # folding @"next" @"step" cartStep # updated (match { "Place order": const orderPlaced })
      ( bodyMedium $ text placedLine ) # shownWhen @"placed" orderStatus
    ) # mvu freshOrder
