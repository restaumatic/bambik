-- | Illustrative only — this demo is NOT part of `spago build` (sources are `src`/`test`).
-- |
-- | The product single-field combinators rest on `Strong` (the row-typed
-- | `Strong`), and `UI` is `Strong`, so `property` (the field lens) works directly on `UI`.
-- | Field-filling is `property` over a seeded record. To introduce a brand-new field from a
-- | source, use `Data.Profunctor.Row.RecordToRecord.recordToProperty`.
module Main (main) where

import Prelude

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (property)
import Effect (Effect)
import UI (silence)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Web (body)

main :: Effect Unit
main = body @({}) $ lcmap (const seed) $ Semigroupoid.do
  property @"foo" $ MDC.filledTextField { floatingLabel: "Foo" }
  property @"day" $ MDC.filledTextField { floatingLabel: "Day" }
  property @"quantity" $ MDC.filledTextField { floatingLabel: "Quantity" }
  property @"price" $ MDC.filledTextField { floatingLabel: "Price" }
  silence
  where
  seed =
    { foo: "foo"
    , day: "1"
    , quantity: "1"
    , price: "10"
    }
