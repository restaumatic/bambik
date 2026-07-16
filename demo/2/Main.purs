-- | Plain-HTML demo — no design system, just `Web` primitives. The whole
-- | page is one `×→×` merge (`RecordToRecord.do`): static content is
-- | announcing chrome (`staticText`/`staticHTML :: … {} {}` — units with a
-- | face, so gates never starve on them), and the live part — a `×→×`
-- | record merge of two plain `input`s feeding a `text` display — is just
-- | one more operand; the page's `{}` output drains into the `silence`
-- | sink. Code order = DOM order.
module Main (main) where

import Prelude

import Data.Profunctor (lcmap)
import Data.Profunctor.Row.RecordToRecord (field, forValue, projection)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import PUI (PUI, silence, with)
import PUI.HTML (a, body, div, input, li, p, staticHTML, staticText, text, ul) as HTML
import PUI.HTML ((:=))
import Prim.Row (class Cons)
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  HTML.body $ ( HTML.div $ Semigroupoid.do
      RecordToRecord.do
        HTML.p $ HTML.staticText "Hello World!"
        HTML.ul $ RecordToRecord.do
          HTML.li $ HTML.staticText "One"
          HTML.li $ HTML.staticText "Two"
          HTML.li $ HTML.staticText "Three"
        HTML.a >>> "href" := "https://www.google.com" $ HTML.staticText "Search for me!"
        HTML.staticHTML "<hr/>"
        Semigroupoid.do
          RecordToRecord.do
            HTML.input "text" # field @"greeting"
            HTML.input "text" # field @"name"
          HTML.p (HTML.text # projection (\r -> r.greeting <> ", " <> r.name <> "!") # forValue)
      silence
  ) # with
      { greeting: "Hello"
      , name: "World"
      }
