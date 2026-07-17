module Main (main) where

import Prelude ((#), ($), (<>), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (field, forValue, projection, silence, with)
import PUI.HTML (a, body, div, input, li, p, staticHTML, staticText, text, ul)
import PUI.HTML ((:=))
import QualifiedDo.Semigroupoid as Semigroupoid

main :: Effect Unit
main =
  body $ ( div $ Semigroupoid.do
      RecordToRecord.do
        p $ staticText "Hello World!"
        ul $ RecordToRecord.do
          li $ staticText "One"
          li $ staticText "Two"
          li $ staticText "Three"
        a >>> "href" := "https://www.google.com" $ staticText "Search for me!"
        staticHTML "<hr/>"
        Semigroupoid.do
          RecordToRecord.do
            input "text" # field @"greeting"
            input "text" # field @"name"
          p (text # projection (\r -> r.greeting <> ", " <> r.name <> "!") # forValue)
      silence
  ) # with
      { greeting: "Hello"
      , name: "World"
      }
