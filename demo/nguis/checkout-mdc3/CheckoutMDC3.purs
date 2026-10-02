module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit, const)

import CheckoutViewModel (cartLine, cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, paymentLine, placedLine, previousOf, shippingLine)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, state, toCase, updated)
import PUI.Web (provided, shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, button)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    ( Semigroupoid.do
      state @"item" @String
      state @"address" @String
      state @"card" @String
      state @"status" @[ pending :: {}, placed :: {} ]
      ( Semigroupoid.do
        ( Semigroupoid.do
          state @"item" @String
          bodyMedium $ text cartLine ) # shownWhen @"cart" checkoutStep
        ( Semigroupoid.do
          state @"address" @String
          bodyMedium $ text shippingLine ) # shownWhen @"shipping" checkoutStep
        ( Semigroupoid.do
          state @"card" @String
          bodyMedium $ text paymentLine ) # shownWhen @"payment" checkoutStep
        RecordToVariant.do
          ( Semigroupoid.do
            state @"step" @[ cart :: {}, shipping :: {}, payment :: {} ]
            button @"Next" {} # toCase @"next" goneOn ) # provided @"onward" onwardFrom
          ( Semigroupoid.do
            state @"step" @[ cart :: {}, shipping :: {}, payment :: {} ]
            button @"Back" {} # toCase @"next" goneBack ) # provided @"back" previousOf
          ( Semigroupoid.do
            state @"card" @String
            button @"Place order" { icon: "shopping_cart_checkout" } ) # provided @"payment" checkoutStep ) # folding @"next" @"step" @[ cart :: {}, shipping :: {}, payment :: {} ] cartStep # updated (match { "Place order": const orderPlaced })
      ( Semigroupoid.do
        state @"item" @String
        state @"address" @String
        state @"card" @String
        bodyMedium $ text placedLine ) # shownWhen @"placed" orderStatus
    ) # mvu freshOrder
