module Main (main) where

import Prelude ((#), ($), Unit)

import Effect (Effect)
import PUI (asField, focusRecord, silence, with)
import PUI.HTML (body)
import PUI.MDC (filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  body $ ( Semigroupoid.do
      filledTextField { floatingLabel: "Foo" } # asField @"foo" # focusRecord
      filledTextField { floatingLabel: "Day" } # asField @"day" # focusRecord
      filledTextField { floatingLabel: "Quantity" } # asField @"quantity" # focusRecord
      filledTextField { floatingLabel: "Price" } # asField @"price" # focusRecord
      silence
  ) # with
      { foo: "foo"
      , day: "1"
      , quantity: "1"
      , price: "10"
      }
