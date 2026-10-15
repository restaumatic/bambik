module Checkout.MDC3 (checkout) where

import Prelude ((#), ($), Unit)

import Checkout (cartLine, freshOrder, onwardFrom, orderPlaced, paymentLine, placedLine, previousOf, shippingLine, stepBack, stepOnward)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (inCaseAt, provided, providedAt, text)
import PUI.Web.MDC3 (body, bodyMedium, button, button')
import QualifiedDo.Semigroupoid as Semigroupoid

checkout :: Effect Unit
checkout =
  body $ Semigroupoid.do
    ( bodyMedium $ text cartLine ) # inCaseAt @"cart" @"step"
    ( bodyMedium $ text shippingLine ) # inCaseAt @"shipping" @"step"
    ( bodyMedium $ text paymentLine ) # inCaseAt @"payment" @"step"
    RecordToVariant.do
      button @"Next" # provided @"onward" onwardFrom
      button @"Back" # provided @"back" previousOf
      button' @"Place order" { icon: "shopping_cart_checkout" } # providedAt @"payment" @"step"
    VariantToRecord.do
      fold @"Next" stepOnward
      fold @"Back" stepBack
      fold @"Place order" orderPlaced
    ( bodyMedium $ text placedLine ) # inCaseAt @"placed" @"status"
  # looped # with freshOrder
