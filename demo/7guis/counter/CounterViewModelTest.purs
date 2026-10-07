module CounterViewModelTest (counterClaims) where

import Prelude ((==))

import CounterViewModel (countedLine, countLine, freshCount, increment)

counterClaims :: Array { claim :: String, holds :: Boolean }
counterClaims =
  [ { claim: "a fresh count shows 0", holds: countLine freshCount == "0" }
  , { claim: "counting adds one", holds: increment freshCount == { counted: 1 } }
  , { claim: "the snackbar names the count reached", holds: countedLine (increment (increment freshCount)) == "Counted to 2" }
  ]
