module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit)

import CheckoutViewModel (cartLine, freshOrder, onwardFrom, orderPlaced, paymentLine, placedLine, previousOf, shippingLine, stepBack, stepOnward)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, looped, unshown, with)
import PUI.Web (inCaseAt, provided, providedAt, text)
import PUI.Web.MDC3 (body, bodyMedium, button, button')
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $ Semigroupoid.do
    ( bodyMedium $ text cartLine ) # inCaseAt @"cart" @"step"
    ( bodyMedium $ text shippingLine ) # inCaseAt @"shipping" @"step"
    ( bodyMedium $ text paymentLine ) # inCaseAt @"payment" @"step"
    RecordToVariant.do
      button @"Next" # provided @"onward" @( onward :: {}, last :: {} ) onwardFrom
      button @"Back" # provided @"back" @( back :: {}, first :: {} ) previousOf
      button' @"Place order" { icon: "shopping_cart_checkout" } # providedAt @"payment" @"step"
    VariantToRecord.do
      unshown @"Next" # fold stepOnward
      unshown @"Back" # fold stepBack
      unshown @"Place order" # fold orderPlaced
    bodyMedium (text placedLine) # inCaseAt @"placed" @"status"
  # looped
    @( item :: String
     , address :: String
     , card :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, shipping :: {}, payment :: {} ]
     ) # with freshOrder
