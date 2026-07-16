-- | Plain-HTML demo — no design system, just `Web` primitives. The whole
-- | page is one `×→×` merge (`RecordToRecord.do`): static content is
-- | announcing chrome (`staticText`/`staticHTML :: … {} {}` — units with a
-- | face, so gates never starve on them), and the live part — a `×→×`
-- | record merge of two plain `input`s feeding a `text` display — is just
-- | one more operand; the page's `{}` output drains into the `silence`
-- | sink. Code order = DOM order.
module Main (main) where

import Prelude hiding (div)

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (field)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid
import UI (UI, silence, with)
import Web (Web, a, body, div, input, li, p, staticHTML, staticText, text, ul, (:=))

main :: Effect Unit
main = body $ with {} $ div $ Semigroupoid.do
  RecordToRecord.do
    p $ staticText "Hello World!"
    ul $ RecordToRecord.do
      li $ staticText "One"
      li $ staticText "Two"
      li $ staticText "Three"
    a >>> "href" := "https://www.google.com" $ staticText "Search for me!"
    staticHTML "<hr/>"
    lcmap seed Semigroupoid.do
      RecordToRecord.do
        field @"greeting" $ input "text"
        field @"name" $ input "text"
      p $ lcmap (\r -> r.greeting <> ", " <> r.name <> "!") text
  silence

-- model seed: its closed signature pins the live operand's input row
seed :: {} -> { greeting :: String, name :: String }
seed _ =
  { greeting: "Hello"
  , name: "World"
  }
