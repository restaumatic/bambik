-- | Illustrative only — this demo is NOT part of `spago build` (sources are `src`/`test`).
-- |
-- | Since the product half-optics now fold onto `Strong` and `UI` is `Strong`, `edit`
-- | (the field lens, formerly `EditPropP`) works directly on `UI`. The old `input`/`output`
-- | (`ReadP`/`WriteP`) combinators were removed; field-filling is now `edit` over a seeded
-- | record. To introduce a brand-new field from a source, use
-- | `Data.Profunctor.HalfOptic.Property.introduceProperty`.
module Main (main) where

import Prelude

import Data.Profunctor (lcmap)
import Data.Profunctor.HalfOptic.Property (edit)
import Data.Profunctor.Zero (pzero)
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Web (body)

main :: Effect Unit
main = body @(Record ()) $ lcmap (const seed) $ Semigroupoid.do
  edit @"foo" $ MDC.filledTextField { floatingLabel: "Foo" }
  edit @"day" $ MDC.filledTextField { floatingLabel: "Day" }
  edit @"quantity" $ MDC.filledTextField { floatingLabel: "Quantity" }
  edit @"price" $ MDC.filledTextField { floatingLabel: "Price" }
  pzero
  where
  seed = { foo: "foo", day: "1", quantity: "1", price: "10" }
