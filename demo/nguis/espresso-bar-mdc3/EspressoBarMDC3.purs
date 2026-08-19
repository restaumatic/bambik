module EspressoBarMDC3 (espressoBarMDC3) where

import Prelude (Unit, const, (#), ($), (<<<))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import EspressoBarLogic (brewedLine, caffeineFraction, espressoNoFrills, summaryText, theUsual, usualOrder)
import PUI (forCase, mvu, projected, required, tapped, updated, with)
import PUI.Web (choice)
import PUI.Web.HTML (body, div, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, checkbox, chipSet, divider, elevation5, filledTextField, filterChip, iconToggle, labelMedium, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBarMDC3 :: Effect Unit
espressoBarMDC3 =
  body $
    elevation5 $
      topAppBar { title: "Espresso Bar" } $
        card $ Semigroupoid.do
          ( Semigroupoid.do
              RecordToRecord.do
                tabBar @"drink"
                  [ choice @"Espresso", choice @"Cappuccino", choice @"Latte" ]
                filledTextField @"Your name" {}
                segmentedButton @"Size"
                  [ choice @"Small", choice @"Medium", choice @"Large" ] # required
                select @"Milk" {}
                  [ choice @"with whole milk", choice @"with oat milk", choice @"with almond milk", choice @"no milk" ] # required
                radioButton @"Roast"
                  [ choice @"Light", choice @"Medium", choice @"Dark" ] # required
                sliderLive @"Sugar" {}
                chipSet RecordToRecord.do
                  filterChip @"Extra shot" {}
                  filterChip @"Decaf" {}
                toggleSwitch @"Takeaway cup" {}
                iconToggle @"Mark as favorite" { onIcon: "favorite", offIcon: "heart_plus" }
                checkbox @"Loyalty" { ticked: {} } (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }
                divider
              menu { label: "Presets" } ( RecordToVariant.do
                  menuItem @"The usual" {} # with theUsual
                  menuItem @"Espresso, no frills" {} ) # updated (match { "The usual": const, "Espresso, no frills": const <<< espressoNoFrills })
          ) # mvu usualOrder
          bodyMedium ( RecordToRecord.do
              staticText "Your cup: "
              text @"summary" # projected summaryText ) # tapped
          ( div $ RecordToRecord.do
              labelMedium $ staticText "Caffeine"
              linearProgress @"caffeine" ) # projected caffeineFraction # tapped
          button @"Place order" { icon: "local_cafe" }
          snackbar # forCase @"Place order" brewedLine
