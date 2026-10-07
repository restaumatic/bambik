module OrderFormViewModelTest (orderFormClaims) where

import Prelude ((==))

import OrderFormViewModel (distanceLine, distanceOf, payingLine, setDistance, staleDistanceForgotten)

orderFormClaims :: Array { claim :: String, holds :: Boolean }
orderFormClaims =
  [ { claim: "an estimate is recorded with the address it was made for", holds: estimated.distance == .estimated { km: 4, to: "Main St 1" } }
  , { claim: "an estimate survives while its address stays", holds: staleDistanceForgotten estimated == estimated }
  , { claim: "an address edit forgets the stale estimate", holds: (staleDistanceForgotten (estimated { "Address" = "Main St 2" })).distance == .unknown {} }
  , { claim: "the distance pane shows only the kilometres", holds: distanceOf estimated == .estimated { km: 4 } }
  , { claim: "the distance line names the kilometres", holds: distanceLine { km: 4 } == "Distance 4 km" }
  , { claim: "the paying line reads the method's label back", holds: payingLine { "Method": .cash {}, "Paid": "20", "Total": "18" } == "Paying by cash" }
  ]
  where
  delivery = { "Address": "Main St 1", "Table": "", "Time": "", distance: .unknown {}, selected: ."Delivery" {} }
  estimated = setDistance { km: 4, to: "Main St 1" } delivery
