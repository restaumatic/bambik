module CrudLogic (createPerson, deletePerson, entries, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson) where

import Prelude ((<$>), (==), bind, discard, pure)

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), stripPrefix)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

pick :: Int -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int, "Name" :: String, "Surname" :: String } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int, "Name" :: String, "Surname" :: String }
pick i m@{ people } = case index people i of
  Just p -> m { selected = Just i, "Name" = p."Name", "Surname" = p."Surname" }
  Nothing -> m

createPerson :: Ref (Array { "Name" :: String, "Surname" :: String }) -> { "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String } } -> Aff [ created :: Array { "Name" :: String, "Surname" :: String } ]
createPerson catalogue { "Name": name, "Surname": surname, people } = .created <$> writePeople catalogue (snoc people { "Name": name, "Surname": surname })

updatePerson :: Ref (Array { "Name" :: String, "Surname" :: String }) -> { "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int } -> Aff [ updated :: Array { "Name" :: String, "Surname" :: String } ]
updatePerson catalogue { "Name": name, "Surname": surname, people, selected } = case selected of
  Just i -> .updated <$> writePeople catalogue (fromMaybe people (updateAt i { "Name": name, "Surname": surname } people))
  Nothing -> pure (.updated people)

deletePerson :: Ref (Array { "Name" :: String, "Surname" :: String }) -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int } -> Aff [ deleted :: Array { "Name" :: String, "Surname" :: String } ]
deletePerson catalogue { people, selected } = case selected of
  Just i -> .deleted <$> writePeople catalogue (fromMaybe people (deleteAt i people))
  Nothing -> pure (.deleted people)

refreshPeople :: Array { "Name" :: String, "Surname" :: String } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int }
refreshPeople people m = m { people = people }

peopleDeleted :: Array { "Name" :: String, "Surname" :: String } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int }
peopleDeleted people = { people, selected: Nothing }

loadPeopleCatalogue :: Ref (Array { "Name" :: String, "Surname" :: String }) -> {} -> Aff { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int }
loadPeopleCatalogue catalogue _ = do
  people <- readPeople catalogue
  pure { "Filter prefix (surname)": "", "Name": "", "Surname": "", people, selected: Nothing }

readPeople :: Ref (Array { "Name" :: String, "Surname" :: String }) -> Aff (Array { "Name" :: String, "Surname" :: String })
readPeople catalogue = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.read catalogue)

writePeople :: Ref (Array { "Name" :: String, "Surname" :: String }) -> Array { "Name" :: String, "Surname" :: String } -> Aff (Array { "Name" :: String, "Surname" :: String })
writePeople catalogue people = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.write people catalogue)
  readPeople catalogue

sharedPeopleCatalogue :: Effect (Ref (Array { "Name" :: String, "Surname" :: String }))
sharedPeopleCatalogue = Ref.new
  [ { "Name": "Hans", "Surname": "Emil" }
  , { "Name": "Max", "Surname": "Mustermann" }
  , { "Name": "Roman", "Surname": "Tisch" }
  ]

entries :: { "Filter prefix (surname)" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: Maybe Int } -> Array { key :: Int, "Name" :: String, "Surname" :: String, selected :: Boolean }
entries { "Filter prefix (surname)": prefix, selected, people } =
  (\{ i, p } -> { key: i, "Name": p."Name", "Surname": p."Surname", selected: selected == Just i })
    <$> filter (\{ p } -> hasPrefix prefix p."Surname") (mapWithIndex (\i p -> { i, p }) people)
  where
  hasPrefix p s = case stripPrefix (Pattern p) s of
    Just _ -> true
    Nothing -> false
