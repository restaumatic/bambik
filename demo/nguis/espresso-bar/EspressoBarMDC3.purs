module EspressoBarMDC3 (espressoBarMDC3) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import EspressoBarViewModel (caffeineFraction, cupLine, espressoNoFrills, orderPlacedLine, placeOrder, theUsual, usualOrder)
import PUI (action, atCase, fold, looped, reset, unshown, with)
import PUI.Web ((<+>), choice, staticText, text)
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, bodyMedium, button', checkbox, chipSet, divider, filledTextField, filterChip, iconToggle, indeterminateLinearProgress, labelMedium, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBarMDC3 :: Effect Unit
espressoBarMDC3 =
  body $
    topAppBar "Espresso Bar" $ Semigroupoid.do
      tabBar @"Drink"
        (choice @"Espresso" <+> choice @"Cappuccino" <+> choice @"Latte")
      filledTextField @"Your name"
      segmentedButton @"Size"
        (choice @"Small" <+> choice @"Medium" <+> choice @"Large")
      select @"Milk"
        (choice @"with whole milk" <+> choice @"with oat milk" <+> choice @"with almond milk" <+> choice @"no milk")
      radioButton @"Roast"
        (choice @"Light" <+> choice @"Medium" <+> choice @"Dark")
      sliderLive @"Sugar"
      chipSet Semigroupoid.do
        filterChip @"Extra shot"
        filterChip @"Decaf"
      toggleSwitch @"Takeaway cup"
      iconToggle @"Mark as favorite" { onIcon: "favorite", offIcon: "heart_plus" }
      checkbox @"Loyalty member" @"member" @"guest" # tooltip "Members get 10% off"
      divider $ Semigroupoid.do
        bodyMedium (text cupLine)
        div $ Semigroupoid.do
          labelMedium $ staticText "Caffeine"
          linearProgress caffeineFraction
      RecordToVariant.do
        menu "Presets" ( RecordToVariant.do
          menuItem @"The usual"
          menuItem @"Espresso, no frills" )
        button' @"Place order" { icon: "local_cafe" }
      VariantToRecord.do
        unshown @"The usual" # fold theUsual
        unshown @"Espresso, no frills" # fold espressoNoFrills
        ( Semigroupoid.do
          indeterminateLinearProgress # action @( "Order placed" :: { number :: Int, "Your name" :: String } ) placeOrder
          snackbar @"Order placed" orderPlacedLine # reset usualOrder ) # atCase @"Place order"
    # looped
      @( "Your name" :: String
       , "Drink" :: [ "Espresso" :: {}, "Cappuccino" :: {}, "Latte" :: {} ]
       , "Size" :: [ "Small" :: {}, "Medium" :: {}, "Large" :: {} ]
       , "Milk" :: [ "with whole milk" :: {}, "with oat milk" :: {}, "with almond milk" :: {}, "no milk" :: {} ]
       , "Roast" :: [ "Light" :: {}, "Medium" :: {}, "Dark" :: {} ]
       , "Sugar" :: { current :: Number, min :: Number, max :: Number, step :: [ discrete :: Number, continuous :: {} ] }
       , "Extra shot" :: Boolean
       , "Decaf" :: Boolean
       , "Takeaway cup" :: Boolean
       , "Mark as favorite" :: Boolean
       , "Loyalty member" :: [ member :: {}, guest :: {} ]
       ) # with usualOrder
