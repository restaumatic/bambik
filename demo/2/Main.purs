module Main (main) where

import Prelude ((#), ($), (<>), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (field, forValue, projection, silence, with)
import PUI.HTML (a, body, div, input, li, p, staticHTML, staticText, text, ul) as HTML
import PUI.HTML ((:=))
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
