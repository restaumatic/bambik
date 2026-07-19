module Main (main) where

import Prelude ((#), ($), (<>), (>>>), Unit)

import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Effect (Effect)
import PUI (field, projection, silence, with)
import PUI.HTML (a, body, div, hr, input, li, p, staticText, text, ul, (:=))
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
        hr
        Semigroupoid.do
          RecordToRecord.do
            input "text" # field @"greeting"
            input "text" # field @"name"
          p (text # projection greetingLine)
      silence
  ) # with helloWorld

greetingLine :: { greeting :: String, name :: String } -> String
greetingLine r = r.greeting <> ", " <> r.name <> "!"

helloWorld :: { greeting :: String, name :: String }
helloWorld = { greeting: "Hello", name: "World" }
