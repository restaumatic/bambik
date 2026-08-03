module EspressoBarMDC2 (espressoBarMDC2) where

import Prelude (Unit, const, (#), ($), (<<<))

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import EspressoBarLogic (brewedLine, caffeineFraction, espressoNoFrills, summaryText, theUsual, usualOrder)
import PUI (asCase, asField, forCase, mvu, projected, required, tapped, updated, with)
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
                tabBar
                  [ { value: .espresso {}, label: "Espresso" }
                  , { value: .cappuccino {}, label: "Cappuccino" }
                  , { value: .latte {}, label: "Latte" }
                  ] # asField @"drink"
                filledTextField { floatingLabel: "Your name" } # asField @"customer"
                segmentedButton
                  [ { value: .small {}, label: "Small" }
                  , { value: .medium {}, label: "Medium" }
                  , { value: .large {}, label: "Large" }
                  ] # required # asField @"size"
                select { floatingLabel: "Milk" }
                  [ { value: .whole {}, label: "Whole" }
                  , { value: .oat {}, label: "Oat" }
                  , { value: .almond {}, label: "Almond" }
                  , { value: .none {}, label: "None" }
                  ] # required # asField @"milk"
                radioButton
                  [ { value: .light {}, label: "Light roast" }
                  , { value: .medium {}, label: "Medium roast" }
                  , { value: .dark {}, label: "Dark roast" }
                  ] # required # asField @"roast"
                sliderLive { label: "Sugar" } # asField @"sugar"
                chipSet RecordToRecord.do
                  filterChip { label: "Extra shot" } # asField @"extraShot"
                  filterChip { label: "Decaf" } # asField @"decaf"
                toggleSwitch { label: "Takeaway cup" } # asField @"takeaway"
                iconToggle { onIcon: "favorite", offIcon: "favorite_border", label: "Mark as favorite" } # asField @"favorite"
                checkbox (staticText "Loyalty member") # tooltip { text: "Members get 10% off" } # asField @"loyalty"
                divider
              menu { label: "Presets" } ( RecordToVariant.do
                  menuItem { label: "The usual" } # asCase @"theUsual" # with theUsual
                  menuItem { label: "Espresso, no frills" } # asCase @"espressoNoFrills" ) # updated (match { theUsual: const, espressoNoFrills: const <<< espressoNoFrills })
          ) # mvu usualOrder
          body2 ( RecordToRecord.do
              staticText "Your cup: "
              text # projected summaryText ) # tapped
          ( div $ RecordToRecord.do
              caption $ staticText "Caffeine"
              linearProgress ) # projected caffeineFraction # tapped
          button { label: "Place order", icon: "local_cafe" } # asCase @"brewed"
          snackbar # forCase @"brewed" brewedLine
