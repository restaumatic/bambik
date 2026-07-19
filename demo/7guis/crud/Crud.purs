module Crud (crud) where

import Prelude ((#), ($), (<$>), (<>), (==), (>>>), Unit, bind, discard, pure, unit)

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap, rmap)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.String (Pattern(..), stripPrefix)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import PUI (action, asCase, asField, completed, forValue, looped, onCase, projection, toCase, updates, with)
import PUI.HTML (body, text)
import PUI.MDC (button, card, elevation20, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

type Person = { name :: String, surname :: String }

type SharedPeopleCatalogue = Ref (Array Person)

type PeopleCatalogue =
  { prefix :: String
  , name :: String
  , surname :: String
  , people :: Array Person
  , selected :: Maybe Int
  }

crud :: Effect Unit
crud = do
  catalogue <- sharedPeopleCatalogue
  body $
    elevation20 $
      card { caption: "CRUD" } $ ( Semigroupoid.do
          indeterminateLinearProgress # action (loadPeopleCatalogue catalogue)
          ( Semigroupoid.do
              ( RecordToRecord.do
                  filledTextField { floatingLabel: "Filter prefix (surname)" } # asField @"prefix"
                  filledTextField { floatingLabel: "Name" } # asField @"name"
                  filledTextField { floatingLabel: "Surname" } # asField @"surname"
              ) # completed
              ( listOf { selected: _.selected } (text # projection _.label # forValue)
              ) # rmap _.key # toCase @"picked" # lcmap entries # updates (match { picked: pick })
              ( Semigroupoid.do
                  RecordToVariant.do
                    button { label: "Create" } # asCase @"create"
                    button { label: "Update" } # asCase @"update"
                    button { label: "Delete" } # asCase @"delete"
                  VariantToVariant.do
                    indeterminateLinearProgress # action (createPerson catalogue) # onCase @"create"
                    indeterminateLinearProgress # action (updatePerson catalogue) # onCase @"update"
                    indeterminateLinearProgress # action (deletePerson catalogue) # onCase @"delete"
              ) # updates (match { created: refreshPeople, updated: refreshPeople, deleted: peopleDeleted })
          ) # looped
      ) # with unit

pick :: Int -> PeopleCatalogue -> PeopleCatalogue
pick i m = case index m.people i of
  Just p -> m { selected = Just i, name = p.name, surname = p.surname }
  Nothing -> m

createPerson :: SharedPeopleCatalogue -> PeopleCatalogue -> Aff [ created :: Array Person ]
createPerson catalogue m = .created <$> writePeople catalogue (snoc m.people { name: m.name, surname: m.surname })

updatePerson :: SharedPeopleCatalogue -> PeopleCatalogue -> Aff [ updated :: Array Person ]
updatePerson catalogue m = case m.selected of
  Just i -> .updated <$> writePeople catalogue (fromMaybe m.people (updateAt i { name: m.name, surname: m.surname } m.people))
  Nothing -> pure (.updated m.people)

deletePerson :: SharedPeopleCatalogue -> PeopleCatalogue -> Aff [ deleted :: Array Person ]
deletePerson catalogue m = case m.selected of
  Just i -> .deleted <$> writePeople catalogue (fromMaybe m.people (deleteAt i m.people))
  Nothing -> pure (.deleted m.people)

refreshPeople :: Array Person -> PeopleCatalogue -> PeopleCatalogue
refreshPeople people m = m { people = people }

deselect :: PeopleCatalogue -> PeopleCatalogue
deselect m = m { selected = Nothing }

peopleDeleted :: Array Person -> PeopleCatalogue -> PeopleCatalogue
peopleDeleted people = refreshPeople people >>> deselect

loadPeopleCatalogue :: SharedPeopleCatalogue -> Unit -> Aff PeopleCatalogue
loadPeopleCatalogue catalogue _ = do
  people <- readPeople catalogue
  pure { prefix: "", name: "", surname: "", people, selected: Nothing }

readPeople :: SharedPeopleCatalogue -> Aff (Array Person)
readPeople catalogue = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.read catalogue)

writePeople :: SharedPeopleCatalogue -> Array Person -> Aff (Array Person)
writePeople catalogue people = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.write people catalogue)
  readPeople catalogue

sharedPeopleCatalogue :: Effect SharedPeopleCatalogue
sharedPeopleCatalogue = Ref.new
  [ { name: "Hans", surname: "Emil" }
  , { name: "Max", surname: "Mustermann" }
  , { name: "Roman", surname: "Tisch" }
  ]

type Entry = { key :: Int, label :: String, surname :: String, selected :: Boolean }

entries :: PeopleCatalogue -> Array Entry
entries m = filter (\e -> hasPrefix m.prefix e.surname)
  (mapWithIndex (\i p -> { key: i, label: p.surname <> ", " <> p.name, surname: p.surname, selected: m.selected == Just i }) m.people)
  where
  hasPrefix p s = case stripPrefix (Pattern p) s of
    Just _ -> true
    Nothing -> false
