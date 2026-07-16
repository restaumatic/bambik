module Main (main) where

import Prelude

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord (completed)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (stripPrefix)
import Data.String (Pattern(..)) as String
import Data.Variant (match) as Variant
import Effect (Effect)
import PUI (looped, updates, with)
import PUI.HTML (attr, body, cl, clWhen, clicked, div, foreach, li, text, ul) as HTML
import PUI.MDC (button, card, elevation20, filledTextField) as MDC
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
          MDC.filledTextField @"prefix" { floatingLabel: "Filter prefix (surname)" }
          MDC.filledTextField @"name" { floatingLabel: "Name" }
          MDC.filledTextField @"surname" { floatingLabel: "Surname" }
      ) # completed
      ( RecordToVariant.do
          HTML.ul >>> HTML.cl "mdc-deprecated-list" >>> HTML.attr "style" "border: 1px solid #ccc; min-height: 120px; max-height: 200px; overflow-y: auto;" $ HTML.foreach
            ( HTML.clicked $ HTML.clWhen _.selected "mdc-deprecated-list-item--selected"
                $ HTML.li >>> HTML.cl "mdc-deprecated-list-item" >>> HTML.attr "style" "cursor: pointer;"
                $ HTML.text # lcmap _.label
            ) # rmap picked # lcmap entries
          HTML.div >>> HTML.attr "style" "display: flex; gap: 8px; margin-top: 8px;" $ RecordToVariant.do
            MDC.button @"create" { label: Just "Create", icon: Nothing }
            MDC.button @"update" { label: Just "Update", icon: Nothing }
            MDC.button @"delete" { label: Just "Delete", icon: Nothing }
      ) # updates handle
  ) # with
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
    # looped

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
