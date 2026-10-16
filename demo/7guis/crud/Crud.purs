module Crud (Catalogue, Entry, createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personLine, pick, updatePerson) where

import Prelude (bind, otherwise, pure, (<$>), (<>), (==), (||))

import Crud.PeopleServer (Person, fetchPeople, storePeople)
import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Variant (match)
import Effect.Aff (Aff)

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

loadPeopleCatalogue :: {} -> Aff [ "People loaded" :: Catalogue ]
loadPeopleCatalogue _ = do
  people <- fetchPeople
  pure (."People loaded" { "Surname prefix": "", "Name": "", "Surname": "", people, selected: .none {} })

pick :: { event :: Int, model :: Catalogue } -> Catalogue
pick { event: i, model: m@{ people } } = case index people i of
  Just p -> m { selected = .picked { index: i }, "Name" = p."Name", "Surname" = p."Surname" }
  Nothing -> m

createPerson
  :: Catalogue
  -> Aff [ "Person created" :: Catalogue, "Person not created" :: Catalogue ]
createPerson m@{ "Name": name, "Surname": surname, people }
  | name == "" || surname == "" = pure (."Person not created" m)
  | otherwise = (\ps -> ."Person created" (refreshPeople ps m)) <$> storePeople (snoc people { "Name": name, "Surname": surname })

updatePerson
  :: Catalogue
  -> Aff [ "Person not updated" :: Catalogue, "Person updated" :: Catalogue ]
updatePerson m@{ "Name": name, "Surname": surname, people, selected } = match
  { picked: \p -> (\ps -> ."Person updated" (refreshPeople ps m)) <$> storePeople (fromMaybe people (updateAt p.index { "Name": name, "Surname": surname } people))
  , none: \_ -> pure (."Person not updated" m)
  } selected

deletePerson
  :: Catalogue
  -> Aff [ "Person deleted" :: Catalogue, "Person not deleted" :: Catalogue ]
deletePerson m@{ people, selected } = match
  { picked: \p -> (\ps -> ."Person deleted" (peopleDeleted ps m)) <$> storePeople (fromMaybe people (deleteAt p.index people))
  , none: \_ -> pure (."Person not deleted" m)
  } selected

refreshPeople :: Array Person -> Catalogue -> Catalogue
refreshPeople people m = m { people = people }

peopleDeleted :: Array Person -> Catalogue -> Catalogue
peopleDeleted people m = m { people = people, selected = .none {} }

entries :: Catalogue -> Array Entry
entries { "Surname prefix": prefix, selected, people } =
  (\{ i, p } -> { key: i, "Name": p."Name", "Surname": p."Surname", status: statusOf i })
    <$> filter (\{ p } -> hasPrefix prefix p."Surname") (mapWithIndex (\i p -> { i, p }) people)
  where
  statusOf i = match { picked: \p -> if p.index == i then .selected {} else .unselected {}, none: \_ -> .unselected {} } selected
  hasPrefix start s = isJust (stripPrefix (Pattern start) s)

personLine :: Entry -> String
personLine { "Name": name, "Surname": surname } = surname <> ", " <> name

isSelected :: Entry -> Boolean
isSelected { status } = match { selected: \_ -> true, unselected: \_ -> false } status

