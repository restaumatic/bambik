module Checkout.MDC2 (checkout) where

import Prelude ((#), ($), Unit)

import Checkout (cartLine, freshOrder, orderPlaced, paymentLine, placedLine, shippingLine)
import Checkout.StepperMDC2 (stepper)
import Effect (Effect)
import PUI (fold, looped, with)
import PUI.Web (choice, inCaseAt, providedAt, text, (<+>))
import PUI.Web.MDC2 (body, body2, button')
import QualifiedDo.Semigroupoid as Semigroupoid

checkout :: Effect Unit
checkout =
  body $ Semigroupoid.do
    ( Semigroupoid.do
      ( body2 $ text cartLine ) # inCaseAt @"Cart" @"step"
      ( body2 $ text shippingLine ) # inCaseAt @"Shipping" @"step"
      ( body2 $ text paymentLine ) # inCaseAt @"Payment" @"step"
      stepper @"step" (choice @"Cart" <+> choice @"Shipping" <+> choice @"Payment")
    ) # looped
    button' @"Place order" { icon: "shopping_cart_checkout" } # providedAt @"Payment" @"step"
    fold @"Place order" orderPlaced
    ( body2 $ text placedLine ) # inCaseAt @"placed" @"status"
  # looped # with freshOrder
