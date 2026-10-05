module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit)

import CheckoutViewModel (cartLine, checkoutStep, freshOrder, onwardFrom, orderPlaced, orderStatus, paymentLine, placedLine, previousOf, shippingLine, stepTo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (fold, joined, looped, with)
import PUI.Web (provided, shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, button)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    ( Semigroupoid.do
      ( bodyMedium $ text cartLine ) # shownWhen @"cart" @( cart :: { item :: String }, shipping :: { address :: String }, payment :: { card :: String } ) checkoutStep
      ( bodyMedium $ text shippingLine ) # shownWhen @"shipping" checkoutStep
      ( bodyMedium $ text paymentLine ) # shownWhen @"payment" checkoutStep
      RecordToVariant.do
        button @"Next" {} # provided @"onward" @( onward :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, last :: {} ) onwardFrom # joined @"Next"
        button @"Back" {} # provided @"back" @( back :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, first :: {} ) previousOf # joined @"Back"
        button @"Place order" { icon: "shopping_cart_checkout" } # provided @"payment" checkoutStep # joined @"Place order"
      VariantToRecord.do
        fold @"Next" stepTo
        fold @"Back" stepTo
        fold @"Place order" orderPlaced
      ( bodyMedium $ text placedLine ) # shownWhen @"placed" @( pending :: {}, placed :: { item :: String, address :: String, card :: String } ) orderStatus
    ) # looped @( item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ], step :: [ cart :: {}, shipping :: {}, payment :: {} ] ) # with freshOrder
