module Main (main) where

import Prelude

import Effect (Effect)
import PUI (asField, focusRecord, silence, with)
import PUI.HTML (body) as HTML
import PUI.MDC (filledTextField) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  HTML.body $ ( Semigroupoid.do
      MDC.filledTextField { floatingLabel: "Foo" } # asField @"foo" # focusRecord
      MDC.filledTextField { floatingLabel: "Day" } # asField @"day" # focusRecord
      MDC.filledTextField { floatingLabel: "Quantity" } # asField @"quantity" # focusRecord
      MDC.filledTextField { floatingLabel: "Price" } # asField @"price" # focusRecord
      silence
  ) # with
      { foo: "foo"
      , day: "1"
      , quantity: "1"
      , price: "10"
      }
