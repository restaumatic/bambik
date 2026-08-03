module EspressoBarMDC3 (espressoBarMDC3) where

import Prelude (Unit, const, (#), ($), (<<<))

import Data.Maybe (Maybe)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Variant (match)
import Effect (Effect)
import EspressoBarLogic (brewedLine, caffeineFraction, espressoNoFrills, summaryText, theUsual, usualOrder)
import PUI (PUI, asCase, asField, forCase, mvu, projected, required, tapped, updated, with)
import PUI.Web.HTML (body, div, staticText, text)
import PUI.Web (Web)
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
                iconToggle { onIcon: "favorite", offIcon: "heart_plus", label: "Mark as favorite" } # asField @"favorite"
                checkbox (staticText "Loyalty member") # tooltip { text: "Members get 10% off" } # asField @"loyalty"
                divider
              menu { label: "Presets" } ( RecordToVariant.do
                  menuItem { label: "The usual" } # asCase @"theUsual" # with theUsual
                  menuItem { label: "Espresso, no frills" } # asCase @"espressoNoFrills" ) # updated (match { theUsual: const, espressoNoFrills: const <<< espressoNoFrills })
          ) # mvu usualOrder
          bodyMedium ( RecordToRecord.do
              staticText "Your cup: "
              text # projected summaryText ) # tapped
          ( div $ RecordToRecord.do
              labelMedium $ staticText "Caffeine"
              linearProgress ) # projected caffeineFraction # tapped
          button { label: "Place order", icon: "local_cafe" } # asCase @"brewed"
          brewedToast

brewedToast :: PUI Web [ brewed :: { customer :: String, drink :: [ espresso :: {}, cappuccino :: {}, latte :: {} ], size :: [ small :: {}, medium :: {}, large :: {} ], milk :: [ whole :: {}, oat :: {}, almond :: {}, none :: {} ], roast :: [ light :: {}, medium :: {}, dark :: {} ], sugar :: { current :: Number, min :: Number, max :: Number, step :: Maybe Number }, extraShot :: Boolean, decaf :: Boolean, takeaway :: Boolean, favorite :: Boolean, loyalty :: Maybe {} } ] {}
brewedToast = snackbar # forCase @"brewed" brewedLine
