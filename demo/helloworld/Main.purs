-- | Illustrative only — this demo is NOT part of `spago build` (sources are `src`/`test`).
-- |
-- | MDC components are row-typed at their labels (`filledTextField @l` is a
-- | singleton-record editor), so a stage of this pipeline is `focusRecord`
-- | of a labeled leaf: the focused field is edited, the background fields
-- | pass through, and each stage hands the whole seeded record to the next.
module Main (main) where

import Prelude

import Data.Profunctor (lcmap)
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
