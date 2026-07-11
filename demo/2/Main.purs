-- | Plain-HTML demo — no design system, just `Web` primitives. The whole
-- | page is one `×→+` merge (`RecordToVariant.do`, the ungated direction):
-- | static content rides as `pempty`-terminated operands (the merge's own
-- | unit doubles as the row-pinning silent terminal), and the live part —
-- | the smallest possible row pipeline, a `×→×` record merge of two plain
-- | `input`s feeding a `text` display — is just one more operand. Code
-- | order = DOM order.
module Main (main) where

import Prelude hiding (div)

import Data.Profunctor (lcmap)
import Data.Profunctor.Row (backdrop)
import Data.Profunctor.Row.RecordToRecord (property)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant (pempty)
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Flow
import UI (UI)
import Web (Web, a, body, div, input, li, p, staticHTML, staticText, text, ul, (:=))

main :: Effect Unit
main = body @({}) $ div $ backdrop $ RecordToVariant.do
  p $ staticText "Hello World!"
  ul $ RecordToVariant.do
    li $ staticText "One"
    li $ staticText "Two"
    li $ staticText "Three"
  a >>> "href" := "https://www.google.com" $ staticText "Search for me!"
  staticHTML "<hr/>"
  lcmap seed Flow.do
    RecordToRecord.do
      field @"greeting" $ input "text"
      field @"name" $ input "text"
    p $ lcmap (\r -> r.greeting <> ", " <> r.name <> "!") text
    pempty

-- model seed: its closed signature pins the live operand's input row
seed :: {} -> { greeting :: String, name :: String }
seed _ =
  { greeting: "Hello"
  , name: "World"
  }

-- the same single-field pinning helper as demo/1 (a library candidate):
-- `property` over a closed singleton row, so the merge operands resolve by
-- label with no inline annotations
field :: forall @l v r. IsSymbol l => Cons l v () r => UI Web v v -> UI Web { | r } { | r }
field = property @l
