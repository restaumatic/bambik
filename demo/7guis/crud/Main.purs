module Main (main) where

import Prelude ((#), ($), (<>), (==), (>>>), Unit)

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (Pattern(..)) as String
import Data.String (stripPrefix)
import Data.Variant (match) as Variant
import Effect (Effect)
import PUI (asCase, asField, completed, forValue, mvu, projection, updates)
import PUI.HTML (attr, body, div, text) as HTML
import PUI.MDC (button, card, elevation20, filledTextField, listOf) as MDC
import QualifiedDo.Semigroupoid as Semigroupoid

type Person = { name :: String, surname :: String }

type Model =
  { prefix :: String
  , name :: String
  , surname :: String
  , people :: Array Person
  , selected :: Maybe Int
  }

main :: Effect Unit
main =
  HTML.body $ MDC.elevation20 $ MDC.card { caption: Just "CRUD" } $ ( Semigroupoid.do
      ( RecordToRecord.do
          MDC.filledTextField { floatingLabel: "Filter prefix (surname)" } # asField @"prefix"
          MDC.filledTextField { floatingLabel: "Name" } # asField @"name"
          MDC.filledTextField { floatingLabel: "Surname" } # asField @"surname"
      ) # completed
      ( RecordToVariant.do
          HTML.attr "style" "border: 1px solid #ccc; min-height: 120px; max-height: 200px;"
            ( MDC.listOf { selected: _.selected } (HTML.text # projection _.label # forValue)
            ) # rmap picked # lcmap entries
          HTML.div >>> HTML.attr "style" "display: flex; gap: 8px; margin-top: 8px;" $ RecordToVariant.do
            MDC.button { label: Just "Create", icon: Nothing } # asCase @"create"
            MDC.button { label: Just "Update", icon: Nothing } # asCase @"update"
            MDC.button { label: Just "Delete", icon: Nothing } # asCase @"delete"
      ) # updates handle
  ) # mvu
      { prefix: ""
      , name: ""
      , surname: ""
      , people:
          [ { name: "Hans", surname: "Emil" }
          , { name: "Max", surname: "Mustermann" }
          , { name: "Roman", surname: "Tisch" }
          ]
      , selected: Nothing
      }

handle ::
  [ picked :: Int
  , create :: Model
  , update :: Model
  , delete :: Model
  ]
  -> Model -> Model
handle e m = Variant.match
  { picked: \i -> case index m.people i of
      Just p -> m { selected = Just i, name = p.name, surname = p.surname }
      Nothing -> m
  , create: \_ ->
      m { people = snoc m.people { name: m.name, surname: m.surname } }
  , update: \_ -> case m.selected of
      Just i -> m { people = fromMaybe m.people (updateAt i { name: m.name, surname: m.surname } m.people) }
      Nothing -> m
  , delete: \_ -> case m.selected of
      Just i -> m { people = fromMaybe m.people (deleteAt i m.people), selected = Nothing }
      Nothing -> m
  } e

type Entry = { key :: Int, label :: String, surname :: String, selected :: Boolean }

picked :: Entry -> [ picked :: Int ]
picked e = .picked e.key

entries :: Model -> Array Entry
entries m = filter (\e -> hasPrefix m.prefix e.surname)
  (mapWithIndex (\i p -> { key: i, label: p.surname <> ", " <> p.name, surname: p.surname, selected: m.selected == Just i }) m.people)
  where
  hasPrefix p s = case stripPrefix (String.Pattern p) s of
    Just _ -> true
    Nothing -> false
