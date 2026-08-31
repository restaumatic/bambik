module EspressoBarMDC3 (espressoBarMDC3) where

import Prelude (Unit, const, (#), ($), (<<<))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import EspressoBarLogic (brewedLine, espressoNoFrills, presentEspressoBar, theUsual, usualOrder)
import PUI (armed, forCase, mvu, required, settled, updated, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, div, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, checkbox, chipSet, divider, elevation5, filledTextField, filterChip, iconToggle, labelMedium, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Category as Category

espressoBarMDC3 :: Effect Unit
espressoBarMDC3 =
  body $
    elevation5 $
      topAppBar { title: "Espresso Bar" } $
        card $ Category.do
          ( Category.do
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
              chipSet Category.do
                filterChip @"Extra shot" {}
                filterChip @"Decaf" {}
              toggleSwitch @"Takeaway cup" {}
              iconToggle @"Mark as favorite" { onIcon: "favorite", offIcon: "heart_plus" }
              checkbox @"Loyalty" @"member" @"guest" { ticked: {} } (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }
              divider # shown
              menu { label: "Presets" } ( RecordToVariant.do
                  menuItem @"The usual" {} # with theUsual
                  menuItem @"Espresso, no frills" {} ) # updated (match { "The usual": const, "Espresso, no frills": const <<< espressoNoFrills })
          ) # settled presentEspressoBar # mvu usualOrder
          ( bodyMedium $ RecordToRecord.do
              staticText "Your cup: "
              text @"summary" ) # shown
          ( div $ RecordToRecord.do
              labelMedium $ staticText "Caffeine"
              linearProgress @"caffeine" ) # shown
          button @"Place order" { icon: "local_cafe" } # armed
          snackbar # forCase @"Place order" brewedLine
