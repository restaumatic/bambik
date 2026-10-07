module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit)

import CheckoutViewModel (cartLine, checkoutStep, freshOrder, onwardFrom, orderPlaced, orderPlacedLine, orderStatus, paymentLine, placedLine, previousOf, shippingLine, stepTo)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (blankStatus, fold, joined, looped, with)
import PUI.Web (provided, shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, button, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    Semigroupoid.do
      ( bodyMedium $ text cartLine ) # shownWhen @"cart"
        @( cart :: { item :: String }
         , shipping :: { address :: String }
         , payment :: { card :: String }
         ) checkoutStep
      ( bodyMedium $ text shippingLine ) # shownWhen @"shipping" checkoutStep
      ( bodyMedium $ text paymentLine ) # shownWhen @"payment" checkoutStep
      RecordToVariant.do
        button @"Next" {} # provided @"onward"
          @( onward :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
           , last :: {}
           ) onwardFrom # joined @"Next"
        button @"Back" {} # provided @"back"
          @( back :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
           , first :: {}
           ) previousOf # joined @"Back"
        button @"Place order" { icon: "shopping_cart_checkout" } # provided @"payment" checkoutStep # joined @"Place order"
      VariantToRecord.do
        blankStatus @"Next" # fold stepTo
        blankStatus @"Back" # fold stepTo
        snackbar @"Place order" orderPlacedLine # fold orderPlaced
      ( bodyMedium $ text placedLine ) # shownWhen @"placed"
        @( pending :: {}
         , placed :: { item :: String, address :: String, card :: String }
         ) orderStatus
    # looped
      @( item :: String
       , address :: String
       , card :: String
       , status :: [ pending :: {}, placed :: {} ]
       , step :: [ cart :: {}, shipping :: {}, payment :: {} ]
       ) # with freshOrder
