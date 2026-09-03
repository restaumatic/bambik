module EspressoBarMDC2 (espressoBarMDC2) where

import Prelude (Unit, const, (#), ($), (<<<))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import EspressoBarLogic (brewedLine, caffeineFraction, cupLine, espressoNoFrills, theUsual, usualOrder)
import PUI (armed, forCase, mvu, required, updated, with)
import PUI.Web (choice)
import PUI.Web.HTML (shown, body, div, staticText, text)
import PUI.Web.MDC2 (body2, button, caption, card, checkbox, chipSet, divider, elevation20, filledTextField, filterChip, iconToggle, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Category as Category

espressoBarMDC2 :: Effect Unit
espressoBarMDC2 =
  body $
    elevation20 $
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
              iconToggle @"Mark as favorite" { onIcon: "favorite", offIcon: "favorite_border" }
              checkbox @"Loyalty" @"member" @"guest" { ticked: {} } (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }
              divider # shown
              menu { label: "Presets" } ( RecordToVariant.do
                  menuItem @"The usual" {} # with theUsual
                  menuItem @"Espresso, no frills" {} ) # updated (match { "The usual": const, "Espresso, no frills": const <<< espressoNoFrills })
          ) # mvu usualOrder
          body2 (text cupLine) # shown
          ( div $ RecordToRecord.do
              caption $ staticText "Caffeine"
              linearProgress @"Caffeine" caffeineFraction ) # shown
          button @"Place order" { icon: "local_cafe" } # armed
          snackbar # forCase @"Place order" brewedLine
