module EspressoBarMDC2 (espressoBarMDC2) where

import Prelude (Unit, const, (#), ($), (<<<))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import EspressoBarViewModel (brewedLine, caffeineFraction, cupLine, espressoNoFrills, loyaltyNote, theUsual, usualOrder)
import PUI (armed, mvu, updated)
import PUI.Web ((<+>), choice, shown, staticText, text)
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, body2, button, caption, checkbox, chipSet, divider, filledTextField, filterChip, iconToggle, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltipWith, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBarMDC2 :: Effect Unit
espressoBarMDC2 =
  body $
    topAppBar @"Espresso Bar" $ Semigroupoid.do
      ( Semigroupoid.do
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
        checkbox @"Loyalty" @"member" @"guest" {} (staticText @"Loyalty member") # tooltipWith loyaltyNote
        divider # shown
        menu @"Presets" ( RecordToVariant.do
          menuItem @"The usual" {}
          menuItem @"Espresso, no frills" {} ) # updated (match { "The usual": const <<< theUsual, "Espresso, no frills": const <<< espressoNoFrills })
      ) # mvu usualOrder
      body2 (text cupLine) # shown
      ( div $ RecordToRecord.do
        caption $ staticText @"Caffeine"
        linearProgress @"Caffeine" caffeineFraction ) # shown
      button @"Place order" { icon: "local_cafe" } # armed
      snackbar @"Place order" brewedLine
