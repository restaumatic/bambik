module Crud (Catalogue, Entry, Person, createPerson, deletePerson, entries, filterChanged, freshCatalogue, isSelected, personLine, pick, updatePerson) where

import Prelude ((<$>), (<>), (==))

import Data.Array (any, deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Variant (match)

type Person = { "Name" :: String, "Surname" :: String }

type Catalogue =
  { "Surname prefix" :: String
  , "Name" :: String
  , "Surname" :: String
  , people :: Array Person
  , selected :: [ none :: {}, picked :: { index :: Int } ]
  }

type Entry =
  { "Name" :: String
  , "Surname" :: String
  , key :: Int
  , status :: [ selected :: {}, unselected :: {} ]
  }

freshCatalogue :: Catalogue
freshCatalogue =
  { "Surname prefix": ""
  , "Name": ""
  , "Surname": ""
  , people:
      [ { "Name": "Hans", "Surname": "Emil" }
      , { "Name": "Max", "Surname": "Mustermann" }
      , { "Name": "Roman", "Surname": "Tisch" }
      ]
  , selected: .none {}
  }

pick :: { event :: Int, model :: Catalogue } -> Catalogue
pick { event: i, model: m@{ people } } = case index people i of
  Just p -> m { selected = .picked { index: i }, "Name" = p."Name", "Surname" = p."Surname" }
  Nothing -> m

filterChanged :: Catalogue -> Catalogue
filterChanged m@{ selected } = match
  { picked: \{ index: i } ->
      if any (\e -> e.key == i) (entries m) then m else m { selected = .none {} }
  , none: \_ -> m
  } selected

createPerson :: Catalogue -> Catalogue
createPerson m@{ "Name": name, "Surname": surname, people } =
  m { people = snoc people { "Name": name, "Surname": surname } }

updatePerson :: Catalogue -> Catalogue
updatePerson m@{ "Name": name, "Surname": surname, people, selected } = match
  { picked: \{ index: i } ->
      m { people = fromMaybe people (updateAt i { "Name": name, "Surname": surname } people) }
  , none: \_ -> m
  } selected

deletePerson :: Catalogue -> Catalogue
deletePerson m@{ people, selected } = match
  { picked: \{ index: i } -> m { people = fromMaybe people (deleteAt i people), selected = .none {} }
  , none: \_ -> m
  } selected

entries :: Catalogue -> Array Entry
entries { "Surname prefix": prefix, selected, people } =
  (\{ i, p } -> { key: i, "Name": p."Name", "Surname": p."Surname", status: statusOf i })
    <$> filter (\{ p } -> hasPrefix prefix p."Surname") (mapWithIndex (\i p -> { i, p }) people)
  where
  statusOf i = match
    { picked: \p -> if p.index == i then .selected {} else .unselected {}
    , none: \_ -> .unselected {}
    } selected
  hasPrefix start s = isJust (stripPrefix (Pattern start) s)

personLine :: Entry -> String
personLine { "Name": name, "Surname": surname } = surname <> ", " <> name

isSelected :: Entry -> Boolean
isSelected { status } = match { selected: \_ -> true, unselected: \_ -> false } status
