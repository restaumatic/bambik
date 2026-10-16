module EspressoBar.MDC3 (espressoBar) where

import Prelude ((#), ($), const, Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Effect (Effect)
import EspressoBar (caffeineFraction, cupLine, espressoNoFrills, placeOrder, theUsual, usualOrder)
import PUI (action, fold, looped, with)
import PUI.Web ((<+>), choice, staticText, text)
import PUI.Web.HTML (div)
import PUI.Web.MDC3 (body, bodyMedium, button', checkbox, chipSet, divider, filledTextField, filterChip, iconToggle, indeterminateLinearProgress, labelMedium, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBar :: Effect Unit
espressoBar =
  body $
    topAppBar "Espresso Bar" $ Semigroupoid.do
      tabBar @"Drink"
        (choice @"Espresso" <+> choice @"Cappuccino" <+> choice @"Latte")
      filledTextField @"Name"
      segmentedButton @"Size"
        (choice @"Small" <+> choice @"Medium" <+> choice @"Large")
      select @"Milk"
        (choice @"Whole milk" <+> choice @"Oat milk" <+> choice @"Almond milk" <+> choice @"No milk")
      radioButton @"Roast"
        (choice @"Light" <+> choice @"Medium" <+> choice @"Dark")
      sliderLive @"Sugar"
      chipSet $ Semigroupoid.do
        filterChip @"Extra shot"
        filterChip @"Decaf"
      toggleSwitch @"Takeaway cup"
      iconToggle @"Mark as favorite" { onIcon: "favorite", offIcon: "heart_plus" }
      checkbox @"Loyalty member" @"member" @"guest" # tooltip "Members get 10% off"
      divider $ Semigroupoid.do
        bodyMedium $ text cupLine
        div $ Semigroupoid.do
          labelMedium $ staticText "Caffeine"
          linearProgress caffeineFraction
      RecordToVariant.do
        menu "Presets" $ RecordToVariant.do
          menuItem @"The usual"
          menuItem @"Plain espresso"
        button' @"Place order" { icon: "local_cafe" }
      VariantToRecord.do
        fold @"The usual" theUsual
        fold @"Plain espresso" espressoNoFrills
        ( Semigroupoid.do
          indeterminateLinearProgress # action @"Place order" placeOrder
          snackbar @"Order placed" (const usualOrder) )
    # looped # with usualOrder
