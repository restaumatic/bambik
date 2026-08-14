module EspressoBarMDC2 (espressoBarMDC2) where

import Prelude (Unit, const, (#), ($), (<<<))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import EspressoBarLogic (brewedLine, caffeineFraction, espressoNoFrills, summaryText, theUsual, usualOrder)
import PUI (forCase, mvu, projected, required, tapped, updated, with)
import PUI.Web.HTML (body, div, staticText, text)
import PUI.Web.MDC2 (body2, button, caption, card, checkbox, chipSet, divider, elevation20, filledTextField, filterChip, iconToggle, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBarMDC2 :: Effect Unit
espressoBarMDC2 =
  body $
    elevation20 $
      topAppBar { title: "Espresso Bar" } $
        card { caption: "Your order" } $ Semigroupoid.do
          ( Semigroupoid.do
              RecordToRecord.do
                tabBar @"drink"
                  [ { value: .espresso {}, label: "Espresso" }
                  , { value: .cappuccino {}, label: "Cappuccino" }
                  , { value: .latte {}, label: "Latte" }
                  ]
                filledTextField @"customer" { floatingLabel: "Your name" }
                segmentedButton @"size"
                  [ { value: .small {}, label: "Small" }
                  , { value: .medium {}, label: "Medium" }
                  , { value: .large {}, label: "Large" }
                  ] # required
                select @"milk" {}
                  [ { value: .whole {}, label: "Whole" }
                  , { value: .oat {}, label: "Oat" }
                  , { value: .almond {}, label: "Almond" }
                  , { value: .none {}, label: "None" }
                  ] # required
                radioButton @"roast"
                  [ { value: .light {}, label: "Light roast" }
                  , { value: .medium {}, label: "Medium roast" }
                  , { value: .dark {}, label: "Dark roast" }
                  ] # required
                sliderLive @"sugar" {}
                chipSet RecordToRecord.do
                  filterChip @"extraShot" {}
                  filterChip @"decaf" {}
                toggleSwitch @"takeaway" { label: "Takeaway cup" }
                iconToggle @"favorite" { onIcon: "favorite", offIcon: "favorite_border", label: "Mark as favorite" }
                checkbox @"loyalty" { ticked: {} } (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }
                divider
              menu { label: "Presets" } ( RecordToVariant.do
                  menuItem @"The usual" {} # with theUsual
                  menuItem @"Espresso, no frills" {} ) # updated (match { "The usual": const, "Espresso, no frills": const <<< espressoNoFrills })
          ) # mvu usualOrder
          body2 ( RecordToRecord.do
              staticText "Your cup: "
              text @"summary" # projected summaryText ) # tapped
          ( div $ RecordToRecord.do
              caption $ staticText "Caffeine"
              linearProgress @"caffeine" ) # projected caffeineFraction # tapped
          button @"Place order" { icon: "local_cafe" }
          snackbar # forCase @"Place order" brewedLine
