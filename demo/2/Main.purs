-- | Plain-HTML demo — no design system, just `Web` primitives. Static
-- | layout is composed with UI's `Monoid` (`fold` — every element a silent
-- | sibling), and the live part is the smallest possible row-profunctor
-- | pipeline: a `×→×` record merge of two plain `input`s feeding a `text`
-- | display of the merged record.
module Main (main) where

import Prelude hiding (div)

import Data.Foldable (fold)
import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (property)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Flow
import UI (UI)
import Web (Web, a, body, div, input, li, p, staticHTML, staticText, text, ul, (:=))

main :: Effect Unit
main = body @Unit $ div $ fold
  [ p $ staticText "Hello World!"
  , ul $ fold
      [ li $ staticText "One"
      , li $ staticText "Two"
      , li $ staticText "Three"
      ]
  , a >>> "href" := "https://www.google.com" $ staticText "Search for me!"
  , staticHTML "<hr/>"
  , lcmap
      ( const
          { greeting: "Hello"
          , name: "World"
          }
      )
      Flow.do
        RecordToRecord.do
          field @"greeting" $ input "text"
          field @"name" $ input "text"
        p $ lcmap (\r -> r.greeting <> ", " <> r.name <> "!") text
        staticText ""
  ]

-- the same single-field pinning helper as demo/1 (a library candidate):
-- `property` over a closed singleton row, so the merge operands resolve by
-- label with no inline annotations
field :: forall @l v r. IsSymbol l => Cons l v () r => UI Web v v -> UI Web { | r } { | r }
field = property @l
