module EspressoBarMDC2 (espressoBarMDC2) where

import Prelude ((#), ($), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import EspressoBarViewModel (brewedLine, caffeineFraction, cupLine, espressoNoFrills, espressoNoFrillsLine, loyaltyNote, theUsual, theUsualLine, usualOrder)
import PUI (fold, looped, with)
import PUI.Web ((<+>), choice, shown, staticText, text)
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, body2, button, caption, checkbox, chipSet, divider, filledTextField, filterChip, iconToggle, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltipWith, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBarMDC2 :: Effect Unit
espressoBarMDC2 =
  body $
    topAppBar "Espresso Bar" $ Semigroupoid.do
        tabBar @"Drink"
          (choice @"Espresso" <+> choice @"Cappuccino" <+> choice @"Latte")
        filledTextField @"Your name" {}
        segmentedButton @"Size"
          (choice @"Small" <+> choice @"Medium" <+> choice @"Large")
        select @"Milk" {}
          (choice @"with whole milk" <+> choice @"with oat milk" <+> choice @"with almond milk" <+> choice @"no milk")
        radioButton @"Roast"
          (choice @"Light" <+> choice @"Medium" <+> choice @"Dark")
        sliderLive @"Sugar" {}
        chipSet Semigroupoid.do
          filterChip @"Extra shot" {}
          filterChip @"Decaf" {}
        toggleSwitch @"Takeaway cup" {}
        iconToggle @"Mark as favorite" { onIcon: "favorite", offIcon: "favorite_border" }
        checkbox @"Loyalty" @"member" @"guest" {} (staticText "Loyalty member") # tooltipWith loyaltyNote
        divider # shown
        body2 (text cupLine) # shown
        ( div $ RecordToRecord.do
          caption $ staticText "Caffeine"
          linearProgress caffeineFraction ) # shown
        RecordToVariant.do
          menu "Presets" ( RecordToVariant.do
            menuItem @"The usual" {}
            menuItem @"Espresso, no frills" {} )
          button @"Place order" { icon: "local_cafe" }
        VariantToRecord.do
          snackbar @"The usual" theUsualLine # fold theUsual
          snackbar @"Espresso, no frills" espressoNoFrillsLine # fold espressoNoFrills
          snackbar @"Place order" brewedLine
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
         , "Loyalty" :: [ member :: {}, guest :: {} ]
         ) # with usualOrder
