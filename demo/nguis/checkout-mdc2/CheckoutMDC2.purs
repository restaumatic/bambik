module CheckoutMDC2 (checkoutMDC2) where

import Prelude ((#), ($), Unit)

import CheckoutViewModel (cartLine, checkoutStep, freshOrder, onwardFrom, orderPlaced, orderPlacedLine, orderStatus, paymentLine, placedLine, previousOf, shippingLine, stepTo)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import PUI (blankStatus, fold, joined, looped, prefixed, suffixed, with)
import PUI.Web (pane, provided, text)
import PUI.Web.MDC2 (body, body2, button, snackbar)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC2 :: Effect Unit
checkoutMDC2 =
  body $ Semigroupoid.do
    prefixed ( RecordToRecord.do
      ( body2 $ text cartLine ) # pane @"cart"
        @( cart :: { item :: String }
         , shipping :: { address :: String }
         , payment :: { card :: String }
         ) checkoutStep
      ( body2 $ text shippingLine ) # pane @"shipping" checkoutStep
      ( body2 $ text paymentLine ) # pane @"payment" checkoutStep ) $ RecordToVariant.do
      button @"Next" {} # provided @"onward"
        @( onward :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
         , last :: {}
         ) onwardFrom # joined @"Next"
      button @"Back" {} # provided @"back"
        @( back :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }
         , first :: {}
         ) previousOf # joined @"Back"
      button @"Place order" { icon: "shopping_cart_checkout" } # provided @"payment" checkoutStep # joined @"Place order"
    ( VariantToRecord.do
      blankStatus @"Next" # fold stepTo
      blankStatus @"Back" # fold stepTo
      snackbar @"Place order" orderPlacedLine # fold orderPlaced ) # suffixed ( body2 (text placedLine) # pane @"placed"
        @( pending :: {}
         , placed :: { item :: String, address :: String, card :: String }
         ) orderStatus )
  # looped
    @( item :: String
     , address :: String
     , card :: String
     , status :: [ pending :: {}, placed :: {} ]
     , step :: [ cart :: {}, shipping :: {}, payment :: {} ]
     ) # with freshOrder
