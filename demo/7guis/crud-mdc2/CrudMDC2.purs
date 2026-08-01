module CrudMDC2 (crudMDC2) where

import Prelude ((#), ($), (<$>), (<<<), (==), Unit, bind, const, discard, pure, unit)

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
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
import PUI (action, asCase, asField, completed, displayed, forField, forValue, looped, onCase, toCase, updates, with)
import PUI.HTML (body, staticText, text)
import PUI.MDC2 (button, card, cardActions, elevation20, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

crudMDC2 :: Effect Unit
crudMDC2 = do
  catalogue <- sharedPeopleCatalogue
  body $
    elevation20 $
      card { caption: "CRUD" } $ ( Semigroupoid.do
          indeterminateLinearProgress # action (loadPeopleCatalogue catalogue)
          ( Semigroupoid.do
              ( RecordToRecord.do
                  filledTextField { floatingLabel: "Filter prefix (surname)" } # asField @"prefix"
                  filledTextField { floatingLabel: "Name" } # asField @"name"
                  filledTextField { floatingLabel: "Surname" } # asField @"surname") # completed
              listOf { selected: _.selected } entries ( displayed $ RecordToRecord.do
                  text # forValue # forField @"surname"
                  staticText ", "
                  text # forValue # forField @"name" ) # toCase @"picked" _.key # updates (match { picked: pick })
              ( Semigroupoid.do
                  cardActions $ RecordToVariant.do
                    button { label: "Create" } # asCase @"create"
                    button { label: "Update" } # asCase @"update"
                    button { label: "Delete" } # asCase @"delete"
                  VariantToVariant.do
                    indeterminateLinearProgress # action (createPerson catalogue) # onCase @"create"
                    indeterminateLinearProgress # action (updatePerson catalogue) # onCase @"update"
                    indeterminateLinearProgress # action (deletePerson catalogue) # onCase @"delete") # updates (match { created: refreshPeople, updated: refreshPeople, deleted: const <<< peopleDeleted })) # looped
      ) # with unit

pick :: Int -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int, name :: String, surname :: String } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int, name :: String, surname :: String }
pick i m@{ people } = case index people i of
  Just p -> m { selected = Just i, name = p.name, surname = p.surname }
  Nothing -> m

createPerson :: Ref (Array { name :: String, surname :: String }) -> { name :: String, surname :: String, people :: Array { name :: String, surname :: String } } -> Aff [ created :: Array { name :: String, surname :: String } ]
createPerson catalogue { name, surname, people } = .created <$> writePeople catalogue (snoc people { name, surname })

updatePerson :: Ref (Array { name :: String, surname :: String }) -> { name :: String, surname :: String, people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> Aff [ updated :: Array { name :: String, surname :: String } ]
updatePerson catalogue { name, surname, people, selected } = case selected of
  Just i -> .updated <$> writePeople catalogue (fromMaybe people (updateAt i { name, surname } people))
  Nothing -> pure (.updated people)

deletePerson :: Ref (Array { name :: String, surname :: String }) -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> Aff [ deleted :: Array { name :: String, surname :: String } ]
deletePerson catalogue { people, selected } = case selected of
  Just i -> .deleted <$> writePeople catalogue (fromMaybe people (deleteAt i people))
  Nothing -> pure (.deleted people)

refreshPeople :: Array { name :: String, surname :: String } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int }
refreshPeople people m = m { people = people }

peopleDeleted :: Array { name :: String, surname :: String } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int }
peopleDeleted people = { people, selected: Nothing }

loadPeopleCatalogue :: Ref (Array { name :: String, surname :: String }) -> Unit -> Aff { prefix :: String, name :: String, surname :: String, people :: Array { name :: String, surname :: String }, selected :: Maybe Int }
loadPeopleCatalogue catalogue _ = do
  people <- readPeople catalogue
  pure { prefix: "", name: "", surname: "", people, selected: Nothing }

readPeople :: Ref (Array { name :: String, surname :: String }) -> Aff (Array { name :: String, surname :: String })
readPeople catalogue = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.read catalogue)

writePeople :: Ref (Array { name :: String, surname :: String }) -> Array { name :: String, surname :: String } -> Aff (Array { name :: String, surname :: String })
writePeople catalogue people = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.write people catalogue)
  readPeople catalogue

sharedPeopleCatalogue :: Effect (Ref (Array { name :: String, surname :: String }))
sharedPeopleCatalogue = Ref.new
  [ { name: "Hans", surname: "Emil" }
  , { name: "Max", surname: "Mustermann" }
  , { name: "Roman", surname: "Tisch" }
  ]

entries :: { prefix :: String, people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> Array { key :: Int, name :: String, surname :: String, selected :: Boolean }
entries { prefix, selected, people } =
  (\{ i, p } -> { key: i, name: p.name, surname: p.surname, selected: selected == Just i })
    <$> filter (\{ p } -> hasPrefix prefix p.surname) (mapWithIndex (\i p -> { i, p }) people)
  where
  hasPrefix p s = case stripPrefix (Pattern p) s of
    Just _ -> true
    Nothing -> false
