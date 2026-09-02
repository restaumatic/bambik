module CheckoutMDC2 (checkoutMDC2) where

import Prelude ((#), ($), Unit, const)

import CheckoutLogic (cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, previousOf)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCases, updated)
import PUI.Web.HTML (shownWhen, body, provided, text)
import PUI.Web.MDC2 (body2, button, card, elevation20)
import QualifiedDo.Category as Category

checkoutMDC2 :: Effect Unit
checkoutMDC2 =
  body $
    elevation20 $
      card $ ( Category.do
          ( Category.do
              ( body2 $ text @"cartLine" ) # shownWhen @"cart" checkoutStep
              ( body2 $ text @"shippingLine" ) # shownWhen @"shipping" checkoutStep
              ( body2 $ text @"paymentLine" ) # shownWhen @"payment" checkoutStep
              RecordToVariant.do
                button @"Next" {} # toCases goneOn # provided @"onward" onwardFrom
                button @"Back" {} # toCases goneBack # provided @"back" previousOf
                button @"Place order" { icon: "shopping_cart_checkout" } # provided @"payment" checkoutStep ) # folding @"next" cartStep # updated (match { "Place order": const (const orderPlaced) })
          ( body2 $ text @"placedLine" ) # shownWhen @"placed" orderStatus
      ) # mvu freshOrder
