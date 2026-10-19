module Checkout.MDC3 (checkout) where

import Prelude ((#), ($), Unit)

import Checkout (cartLine, freshOrder, orderPlaced, paymentLine, placedLine, shippingLine)
import Checkout.StepperMDC3 (step, stepper)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (inCaseAt, providedAt, text)
import PUI.Web.MDC3 (body, bodyMedium, button')
import QualifiedDo.Semigroupoid as Semigroupoid

checkout :: Effect Unit
checkout =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      ( bodyMedium $ text cartLine ) # inCaseAt @"Cart" @"step"
      ( bodyMedium $ text shippingLine ) # inCaseAt @"Shipping" @"step"
      ( bodyMedium $ text paymentLine ) # inCaseAt @"Payment" @"step"
      stepper @"step" $ VariantToVariant.do
        step @"Cart"
        step @"Shipping"
        step @"Payment"
    ) # looped
    button' @"Place order" { icon: "shopping_cart_checkout" } # providedAt @"Payment" @"step"
    fold @"Place order" orderPlaced
    ( bodyMedium $ text placedLine ) # inCaseAt @"placed" @"status"
  # looped # with freshOrder
