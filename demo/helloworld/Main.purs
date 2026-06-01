-- | Illustrative only — this demo is NOT part of `spago build` (sources are `src`/`test`).
-- |
-- | The product single-field combinators rest on `StrongRecordToRecord` (the row-typed
-- | `Strong`), and `UI` is `Strong`, so `editProperty` (the field lens) works directly on `UI`.
-- | Field-filling is `editProperty` over a seeded record. To introduce a brand-new field from a
-- | source, use `Data.Profunctor.Row.RecordToRecord.introduceProperty`.
module Main (main) where

import Prelude

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (editProperty)
import Data.Profunctor.Zero (pzero)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Web (body)

main :: Effect Unit
main = body @(Record ()) $ lcmap (const seed) $ Semigroupoid.do
  editProperty @"foo" $ MDC.filledTextField { floatingLabel: "Foo" }
  editProperty @"day" $ MDC.filledTextField { floatingLabel: "Day" }
  editProperty @"quantity" $ MDC.filledTextField { floatingLabel: "Quantity" }
  editProperty @"price" $ MDC.filledTextField { floatingLabel: "Price" }
  pzero
  where
  seed = { foo: "foo", day: "1", quantity: "1", price: "10" }
