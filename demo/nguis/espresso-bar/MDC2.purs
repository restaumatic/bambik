module EspressoBar.MDC2 (espressoBar) where

import Prelude ((#), ($), const, Unit)

import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToRecord as VariantToRecord
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Effect (Effect)
import EspressoBar (caffeineFraction, cupLine, espressoNoFrills, placeOrder, theUsual, usualOrder)
import PUI (action, fold, looped, with)
import PUI.Web (staticText, text)
import PUI.Web.HTML (div)
import PUI.Web.MDC2 (body, body2, button', caption, checkbox, chipSet, divider, filledTextField, filterChip, iconToggle, indeterminateLinearProgress, linearProgress, menu, menuItem, option, radio, radioButton, segment, segmentedButton, select, sliderLive, snackbar, tab, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBar :: Effect Unit
espressoBar =
  body $
    topAppBar "Espresso Bar" $ Semigroupoid.do
      ( Semigroupoid.do
        tabBar @"Drink" $ VariantToVariant.do
          tab @"Espresso"
          tab @"Cappuccino"
          tab @"Latte"
        filledTextField @"Name"
        segmentedButton @"Size" $ VariantToVariant.do
          segment @"Small"
          segment @"Medium"
          segment @"Large"
        select @"Milk" $ VariantToVariant.do
          option @"Whole milk"
          option @"Oat milk"
          option @"Almond milk"
          option @"No milk"
        radioButton @"Roast" $ VariantToVariant.do
          radio @"Light"
          radio @"Medium"
          radio @"Dark"
        sliderLive @"Sugar"
        chipSet $ Semigroupoid.do
          filterChip @"Extra shot"
          filterChip @"Decaf"
        toggleSwitch @"Takeaway cup"
        iconToggle @"Mark as favorite" { onIcon: "favorite", offIcon: "favorite_border" }
        checkbox @"Loyalty member" @"member" @"guest" # tooltip "Members get 10% off"
      ) # looped
      divider $ Semigroupoid.do
        body2 $ text cupLine
        div $ Semigroupoid.do
          caption $ staticText "Caffeine"
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
