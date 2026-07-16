-- | Illustrative only — this demo is NOT part of `spago build` (sources are `src`/`test`).
-- |
-- | MDC components are row-typed at their labels (`filledTextField @l` is a
-- | singleton-record editor), so a stage of this pipeline is `focusRecord`
-- | of a labeled leaf: the focused field is edited, the background fields
-- | pass through, and each stage hands the whole seeded record to the next.
module Main (main) where

import Prelude

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (focusRecord)
import Effect (Effect)
import PUI (silence, with)
import PUI.MDC (filledTextField) as MDC
import PUI.Web (body) as Web
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main = Web.body
  $ with
    { foo: "foo"
    , day: "1"
    , quantity: "1"
    , price: "10"
    }
  $ Semigroupoid.do
      focusRecord $ MDC.filledTextField @"foo" { floatingLabel: "Foo" }
      focusRecord $ MDC.filledTextField @"day" { floatingLabel: "Day" }
      focusRecord $ MDC.filledTextField @"quantity" { floatingLabel: "Quantity" }
      focusRecord $ MDC.filledTextField @"price" { floatingLabel: "Price" }
      silence
