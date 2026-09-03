module CrudLogic (createPerson, deletePerson, entries, isSelected, personLine, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, sharedPeopleCatalogue, updatePerson) where

import Prelude ((<$>), (<>), (==), bind, discard, pure)

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), stripPrefix)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.Variant (match)

pick :: Int -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ], "Name" :: String, "Surname" :: String } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ], "Name" :: String, "Surname" :: String }
pick i m@{ people } = case index people i of
  Just p -> m { selected = .picked { index: i }, "Name" = p."Name", "Surname" = p."Surname" }
  Nothing -> m

createPerson :: Ref (Array { "Name" :: String, "Surname" :: String }) -> { "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String } } -> Aff [ created :: Array { "Name" :: String, "Surname" :: String } ]
createPerson catalogue { "Name": name, "Surname": surname, people } = .created <$> writePeople catalogue (snoc people { "Name": name, "Surname": surname })

updatePerson :: Ref (Array { "Name" :: String, "Surname" :: String }) -> { "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] } -> Aff [ updated :: Array { "Name" :: String, "Surname" :: String } ]
updatePerson catalogue { "Name": name, "Surname": surname, people, selected } = match
  { picked: \p -> .updated <$> writePeople catalogue (fromMaybe people (updateAt p.index { "Name": name, "Surname": surname } people))
  , none: \_ -> pure (.updated people)
  } selected

deletePerson :: Ref (Array { "Name" :: String, "Surname" :: String }) -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] } -> Aff [ deleted :: Array { "Name" :: String, "Surname" :: String } ]
deletePerson catalogue { people, selected } = match
  { picked: \p -> .deleted <$> writePeople catalogue (fromMaybe people (deleteAt p.index people))
  , none: \_ -> pure (.deleted people)
  } selected

refreshPeople :: Array { "Name" :: String, "Surname" :: String } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] }
refreshPeople people m = m { people = people }

peopleDeleted :: Array { "Name" :: String, "Surname" :: String } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] }
peopleDeleted people = { people, selected: .none {} }

loadPeopleCatalogue :: Ref (Array { "Name" :: String, "Surname" :: String }) -> {} -> Aff { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] }
loadPeopleCatalogue catalogue _ = do
  people <- readPeople catalogue
  pure { "Filter prefix (surname)": "", "Name": "", "Surname": "", people, selected: .none {} }

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

entries :: { "Filter prefix (surname)" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] } -> Array { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] }
entries { "Filter prefix (surname)": prefix, selected, people } =
  (\{ i, p } -> { key: i, "Name": p."Name", "Surname": p."Surname", status: statusOf i })
    <$> filter (\{ p } -> hasPrefix prefix p."Surname") (mapWithIndex (\i p -> { i, p }) people)
  where
  statusOf i = match { picked: \p -> if p.index == i then .selected {} else .unselected {}, none: \_ -> .unselected {} } selected
  hasPrefix p s = case stripPrefix (Pattern p) s of
    Just _ -> true
    Nothing -> false

personLine :: { "Name" :: String, "Surname" :: String } -> String
personLine { "Name": name, "Surname": surname } = surname <> ", " <> name

isSelected :: { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] } -> Boolean
isSelected { status } = match { selected: \_ -> true, unselected: \_ -> false } status
