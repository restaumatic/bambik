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
import PUI (action, asCase, asField, completed, looped, onCase, projection, toCase, updatesOn, widenRecordInput, with)
import PUI.HTML (body, text)
import PUI.MDC (button, card, cardActions, elevation20, filledTextField, indeterminateLinearProgress, listOf)
import QualifiedDo.Semigroupoid as Semigroupoid

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
                  filledTextField { floatingLabel: "Surname" } # asField @"surname") # completed
              listOf { selected: _.selected } (text # projection _.label) # rmap _.key # toCase @"picked" # lcmap entries # widenRecordInput # updatesOn (match { picked: pick })
              ( Semigroupoid.do
                  cardActions $ RecordToVariant.do
                    button { label: "Create" } # asCase @"create"
                    button { label: "Update" } # asCase @"update"
                    button { label: "Delete" } # asCase @"delete"
                  VariantToVariant.do
                    indeterminateLinearProgress # action (createPerson catalogue) # onCase @"create"
                    indeterminateLinearProgress # action (updatePerson catalogue) # onCase @"update"
                    indeterminateLinearProgress # action (deletePerson catalogue) # onCase @"delete") # widenRecordInput # updatesOn (match { created: refreshPeople, updated: refreshPeople, deleted: peopleDeleted })) # looped
      ) # with unit

pick :: Int -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int, name :: String, surname :: String } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int, name :: String, surname :: String }
pick i m = case index m.people i of
  Just p -> m { selected = Just i, name = p.name, surname = p.surname }
  Nothing -> m

createPerson :: Ref (Array { name :: String, surname :: String }) -> { name :: String, surname :: String, people :: Array { name :: String, surname :: String } } -> Aff [ created :: Array { name :: String, surname :: String } ]
createPerson catalogue m = .created <$> writePeople catalogue (snoc m.people { name: m.name, surname: m.surname })

updatePerson :: Ref (Array { name :: String, surname :: String }) -> { name :: String, surname :: String, people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> Aff [ updated :: Array { name :: String, surname :: String } ]
updatePerson catalogue m = case m.selected of
  Just i -> .updated <$> writePeople catalogue (fromMaybe m.people (updateAt i { name: m.name, surname: m.surname } m.people))
  Nothing -> pure (.updated m.people)

deletePerson :: Ref (Array { name :: String, surname :: String }) -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> Aff [ deleted :: Array { name :: String, surname :: String } ]
deletePerson catalogue m = case m.selected of
  Just i -> .deleted <$> writePeople catalogue (fromMaybe m.people (deleteAt i m.people))
  Nothing -> pure (.deleted m.people)

refreshPeople :: Array { name :: String, surname :: String } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int }
refreshPeople people m = m { people = people }

deselect :: { people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int }
deselect m = m { selected = Nothing }

peopleDeleted :: Array { name :: String, surname :: String } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> { people :: Array { name :: String, surname :: String }, selected :: Maybe Int }
peopleDeleted people = refreshPeople people >>> deselect

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

entries :: { prefix :: String, people :: Array { name :: String, surname :: String }, selected :: Maybe Int } -> Array { key :: Int, label :: String, selected :: Boolean }
entries m =
  (\{ i, p } -> { key: i, label: p.surname <> ", " <> p.name, selected: m.selected == Just i })
    <$> filter (\{ p } -> hasPrefix m.prefix p.surname) (mapWithIndex (\i p -> { i, p }) m.people)
  where
  hasPrefix p s = case stripPrefix (Pattern p) s of
    Just _ -> true
    Nothing -> false
