module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit, const)

import CheckoutLogic (cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, previousOf)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCases, updated)
import PUI.Web.HTML (shownWhen, body, provided, text)
import PUI.Web.MDC3 (bodyMedium, button, card, elevation5)
import QualifiedDo.Category as Category

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    elevation5 $
      card $ ( Category.do
          ( Category.do
              ( bodyMedium $ text @"cartLine" ) # shownWhen @"cart" checkoutStep
              ( bodyMedium $ text @"shippingLine" ) # shownWhen @"shipping" checkoutStep
              ( bodyMedium $ text @"paymentLine" ) # shownWhen @"payment" checkoutStep
              RecordToVariant.do
                button @"Next" {} # toCases goneOn # provided @"onward" onwardFrom
                button @"Back" {} # toCases goneBack # provided @"back" previousOf
                button @"Place order" { icon: "shopping_cart_checkout" } # provided @"payment" checkoutStep ) # folding @"next" cartStep # updated (match { "Place order": const (const orderPlaced) })
          ( bodyMedium $ text @"placedLine" ) # shownWhen @"placed" orderStatus
      ) # mvu freshOrder
