module Crud (crud) where

import Prelude ((#), ($), (<>), (==), (>>>), Unit)

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (Pattern(..))
import Data.String (stripPrefix)
import Data.Variant (match)
import Effect (Effect)
import PUI (asCase, asField, completed, forValue, mvu, projection, updates)
import PUI.HTML (attr, body, div, text)
import PUI.MDC (button, card, elevation20, filledTextField, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

type Person = { name :: String, surname :: String }

type Model =
  { prefix :: String
  , name :: String
  , surname :: String
  , people :: Array Person
  , selected :: Maybe Int
  }

crud :: Effect Unit
crud =
  body $
    elevation20 $
      card { caption: "CRUD" } $ ( Semigroupoid.do
          ( RecordToRecord.do
              filledTextField { floatingLabel: "Filter prefix (surname)" } # asField @"prefix"
              filledTextField { floatingLabel: "Name" } # asField @"name"
              filledTextField { floatingLabel: "Surname" } # asField @"surname"
          ) # completed
          ( RecordToVariant.do
              attr "style" "border: 1px solid #ccc; min-height: 120px; max-height: 200px;"
                ( listOf { selected: _.selected } (text # projection _.label # forValue)
                ) # rmap (\e -> .picked e.key :: [ picked :: Int ]) # lcmap entries
              div >>> attr "style" "display: flex; gap: 8px; margin-top: 8px;" $ RecordToVariant.do
                button { label: "Create" } # asCase @"create"
                button { label: "Update" } # asCase @"update"
                button { label: "Delete" } # asCase @"delete"
          ) # updates (match { picked: pick, create: \m _ -> createPerson m, update: \m _ -> updatePerson m, delete: \m _ -> deletePerson m })
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

pick :: Int -> Model -> Model
pick i m = case index m.people i of
  Just p -> m { selected = Just i, name = p.name, surname = p.surname }
  Nothing -> m

createPerson :: Model -> Model
createPerson m = m { people = snoc m.people { name: m.name, surname: m.surname } }

updatePerson :: Model -> Model
updatePerson m = case m.selected of
  Just i -> m { people = fromMaybe m.people (updateAt i { name: m.name, surname: m.surname } m.people) }
  Nothing -> m

deletePerson :: Model -> Model
deletePerson m = case m.selected of
  Just i -> m { people = fromMaybe m.people (deleteAt i m.people), selected = Nothing }
  Nothing -> m

type Entry = { key :: Int, label :: String, surname :: String, selected :: Boolean }

entries :: Model -> Array Entry
entries m = filter (\e -> hasPrefix m.prefix e.surname)
  (mapWithIndex (\i p -> { key: i, label: p.surname <> ", " <> p.name, surname: p.surname, selected: m.selected == Just i }) m.people)
  where
  hasPrefix p s = case stripPrefix (Pattern p) s of
    Just _ -> true
    Nothing -> false
