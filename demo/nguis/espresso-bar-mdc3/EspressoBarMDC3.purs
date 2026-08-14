module EspressoBarMDC3 (espressoBarMDC3) where

import Prelude ((>>>), Unit, const, (#), ($), (<<<))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import EspressoBarLogic (brewedLine, caffeineFraction, espressoNoFrills, summaryText, theUsual, usualOrder)
import PUI (announce, asCase, forCase, mvu, projected, required, tapped, updated)
import PUI.Web.HTML (body, div, staticText, text)
import PUI.Web.MDC3 (bodyMedium, button, card, checkbox, chipSet, divider, elevation5, filledTextField, filterChip, iconToggle, labelMedium, linearProgress, menu, menuItem, radioButton, segmentedButton, select, sliderLive, snackbar, tabBar, toggleSwitch, tooltip, topAppBar)
import QualifiedDo.Semigroupoid as Semigroupoid

espressoBarMDC3 :: Effect Unit
espressoBarMDC3 =
  body $
    elevation5 $
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
                  ] # required @"size"
                select @"milk" { floatingLabel: "Milk" }
                  [ { value: .whole {}, label: "Whole" }
                  , { value: .oat {}, label: "Oat" }
                  , { value: .almond {}, label: "Almond" }
                  , { value: .none {}, label: "None" }
                  ] # required @"milk"
                radioButton @"roast"
                  [ { value: .light {}, label: "Light roast" }
                  , { value: .medium {}, label: "Medium roast" }
                  , { value: .dark {}, label: "Dark roast" }
                  ] # required @"roast"
                sliderLive @"sugar" { label: "Sugar" }
                chipSet RecordToRecord.do
                  filterChip @"extraShot" { label: "Extra shot" }
                  filterChip @"decaf" { label: "Decaf" }
                toggleSwitch @"takeaway" { label: "Takeaway cup" }
                iconToggle @"favorite" { onIcon: "favorite", offIcon: "heart_plus", label: "Mark as favorite" }
                checkbox @"loyalty" { ticked: {} } (staticText "Loyalty member") # tooltip { text: "Members get 10% off" }
                divider
              menu { label: "Presets" } ( RecordToVariant.do
                  announce theUsual >>> menuItem { label: "The usual" } # asCase @"clicked" @"theUsual"
                  menuItem { label: "Espresso, no frills" } # asCase @"clicked" @"espressoNoFrills" ) # updated (match { theUsual: const, espressoNoFrills: const <<< espressoNoFrills })
          ) # mvu usualOrder
          bodyMedium ( RecordToRecord.do
              staticText "Your cup: "
              text @"value" # projected @"value" summaryText ) # tapped
          ( div $ RecordToRecord.do
              labelMedium $ staticText "Caffeine"
              linearProgress @"value" ) # projected @"value" caffeineFraction # tapped
          button { label: "Place order", icon: "local_cafe" } # asCase @"clicked" @"brewed"
          snackbar # forCase @"event" @"brewed" brewedLine
