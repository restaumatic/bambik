module CheckoutMDC2 (checkoutMDC2) where

import Prelude ((#), ($), Unit, const)

import CheckoutViewModel (cartLine, cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, paymentLine, placedLine, previousOf, shippingLine)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import PUI (mvu, toCase, updated)
import PUI.Web (provided, shownWhen, text)
import PUI.Web.MDC2 (body, body2, button)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC2 :: Effect Unit
checkoutMDC2 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        ( body2 $ text cartLine ) # shownWhen @"cart" @( cart :: { item :: String }, shipping :: { address :: String }, payment :: { card :: String } ) checkoutStep
        ( body2 $ text shippingLine ) # shownWhen @"shipping" checkoutStep
        ( body2 $ text paymentLine ) # shownWhen @"payment" checkoutStep
        RecordToVariant.do
          button @"Next" {} # toCase @"next" @{ step :: [ cart :: {}, shipping :: {}, payment :: {} ] } goneOn # provided @"onward" @( onward :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, last :: {} ) onwardFrom
          button @"Back" {} # toCase @"next" @{ step :: [ cart :: {}, shipping :: {}, payment :: {} ] } goneBack # provided @"back" @( back :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, first :: {} ) previousOf
          button @"Place order" { icon: "shopping_cart_checkout" } # provided @"payment" checkoutStep ) # folding @"next" @"step" @[ cart :: {}, shipping :: {}, payment :: {} ] cartStep # updated (match { "Place order": const orderPlaced })
      ( body2 $ text placedLine ) # shownWhen @"placed" @( pending :: {}, placed :: { item :: String, address :: String, card :: String } ) orderStatus
    ) # mvu @( item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] ) freshOrder
