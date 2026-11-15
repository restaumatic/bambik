module Checkout.MDC2 (checkout) where

import Prelude ((#), ($), Unit)

import Checkout (cartLine, checkoutStage, freshOrder, orderPlaced, paymentLine, placedLine, shippingLine)
import Checkout.StepperMDC2 (step, stepper)
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import PUI (fold, looped, stepped, with)
import PUI.Web (inCaseAt, provided, text)
import PUI.Web.MDC2 (body, body2, button')
import QualifiedDo.Semigroupoid as Semigroupoid

checkout :: Effect Unit
checkout =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      ( body2 $ text cartLine ) # inCaseAt @"Cart" @"step"
      ( body2 $ text shippingLine ) # inCaseAt @"Shipping" @"step"
      ( body2 $ text paymentLine ) # inCaseAt @"Payment" @"step"
      stepper @"step" $ VariantToVariant.do
        step @"Cart"
        step @"Shipping"
        step @"Payment" ) # inCaseAt @"pending" @"status"
    ( Semigroupoid.do
      button' @"Place order" { icon: "shopping_cart_checkout" } # provided @"paying" checkoutStage
      fold @"Place order" orderPlaced ) # stepped
    ( body2 $ text placedLine ) # inCaseAt @"placed" @"status"
  # looped # with freshOrder
