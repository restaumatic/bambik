module Main (main) where

import Prelude

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Foldable (for_)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Row.RecordToRecord (completed)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.String (joinWith, stripPrefix)
import Data.String (Pattern(..)) as String
import Data.Variant (case_, on) as Variant
import Effect (Effect)
import MDC as MDC
import QualifiedDo.Semigroupoid as Semigroupoid
import Type.Proxy (Proxy(..))
import UI (UI, looped, updates)
import Web (Web, bodyWith, escapeHtml, onKeyClick, viewEvents)

type Person = { name :: String, surname :: String }

type Model =
  { prefix :: String
  , name :: String
  , surname :: String
  , people :: Array Person
  , selected :: Maybe Int
  }

main :: Effect Unit
main = bodyWith initial $ MDC.elevation20 $ MDC.card { caption: Just "CRUD" } $ looped Semigroupoid.do
  completed RecordToRecord.do
    MDC.filledTextField @"prefix" { floatingLabel: "Filter prefix (surname)" }
    MDC.filledTextField @"name" { floatingLabel: "Name" }
    MDC.filledTextField @"surname" { floatingLabel: "Surname" }
  updates handle RecordToVariant.do
    listBox
    MDC.button @"create" { label: Just "Create", icon: Nothing }
    MDC.button @"update" { label: Just "Update", icon: Nothing }
    MDC.button @"delete" { label: Just "Delete", icon: Nothing }

initial :: Model
initial =
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
handle e m = e # (Variant.case_
  -- selection is an event: it prefills the fields from the picked person
  # Variant.on (Proxy @"picked") (\i -> case index m.people i of
      Just p -> m { selected = Just i, name = p.name, surname = p.surname }
      Nothing -> m)
  # Variant.on (Proxy @"create") (\_ ->
      m { people = snoc m.people { name: m.name, surname: m.surname } })
  # Variant.on (Proxy @"update") (\_ -> case m.selected of
      Just i -> m { people = fromMaybe m.people (updateAt i { name: m.name, surname: m.surname } m.people) }
      Nothing -> m)
  # Variant.on (Proxy @"delete") (\_ -> case m.selected of
      Just i -> m { people = fromMaybe m.people (deleteAt i m.people), selected = Nothing }
      Nothing -> m))

-- | The list: a `×→+` view-with-events leaf — the model in (rendered as an
-- | MDC list of filtered entries, absolute keys surviving the filter), a
-- | bare `picked` index out.
listBox :: UI Web Model [ picked :: Int ]
listBox = viewEvents
  """<ul class="mdc-deprecated-list" style="border: 1px solid #ccc; min-height: 120px; max-height: 200px; overflow-y: auto;"></ul>"""
  render
  (\node emit -> onKeyClick node \key -> for_ (Int.fromString key) \i -> emit (.picked i))
  where
  render m = joinWith "" (entries m <#> \e ->
    "<li class=\"mdc-deprecated-list-item" <> (if m.selected == Just e.key then " mdc-deprecated-list-item--selected" else "") <> "\" style=\"cursor: pointer;\" data-key=\"" <> show e.key <> "\">"
      <> escapeHtml e.label <> "</li>")
  entries m = m.people
    # mapWithIndex (\i p -> { key: i, label: p.surname <> ", " <> p.name, surname: p.surname })
    # filter (\e -> hasPrefix m.prefix e.surname)
  hasPrefix p s = case stripPrefix (String.Pattern p) s of
    Just _ -> true
    Nothing -> false
