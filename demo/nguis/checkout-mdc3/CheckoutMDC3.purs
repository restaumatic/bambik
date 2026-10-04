module CheckoutMDC3 (checkoutMDC3) where

import Prelude ((#), ($), Unit)

import CheckoutViewModel (cartLine, cartStep, checkoutStep, freshOrder, goneBack, goneOn, onwardFrom, orderPlaced, orderStatus, paymentLine, placedLine, previousOf, shippingLine)
import Data.Profunctor.Row.RecordToVariant (folding)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Effect (Effect)
import PUI (fold, joined, mvu, toCase)
import PUI.Web (provided, shownWhen, text)
import PUI.Web.MDC3 (body, bodyMedium, button)
import QualifiedDo.Semigroupoid as Semigroupoid

checkoutMDC3 :: Effect Unit
checkoutMDC3 =
  body $
    ( Semigroupoid.do
      ( Semigroupoid.do
        ( bodyMedium $ text cartLine ) # shownWhen @"cart" @( cart :: { item :: String }, shipping :: { address :: String }, payment :: { card :: String } ) checkoutStep
        ( bodyMedium $ text shippingLine ) # shownWhen @"shipping" checkoutStep
        ( bodyMedium $ text paymentLine ) # shownWhen @"payment" checkoutStep
        RecordToVariant.do
          button @"Next" {} # toCase @"next" @{ step :: [ cart :: {}, shipping :: {}, payment :: {} ] } goneOn # provided @"onward" @( onward :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, last :: {} ) onwardFrom
          button @"Back" {} # toCase @"next" @{ step :: [ cart :: {}, shipping :: {}, payment :: {} ] } goneBack # provided @"back" @( back :: { step :: [ cart :: {}, shipping :: {}, payment :: {} ] }, first :: {} ) previousOf
          button @"Place order" { icon: "shopping_cart_checkout" } # provided @"payment" checkoutStep # joined @"Place order" ) # folding @"next" @"step" @[ cart :: {}, shipping :: {}, payment :: {} ] cartStep
      fold @"Place order" orderPlaced
      ( bodyMedium $ text placedLine ) # shownWhen @"placed" @( pending :: {}, placed :: { item :: String, address :: String, card :: String } ) orderStatus
    ) # mvu @( item :: String, address :: String, card :: String, status :: [ pending :: {}, placed :: {} ] ) freshOrder
