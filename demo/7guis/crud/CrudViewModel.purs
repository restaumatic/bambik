module CrudViewModel (createPerson, deletePerson, entries, isSelected, personLine, loadPeopleCatalogue, pick, updatePerson) where

import Prelude ((<$>), (<>), (==), ($), bind, discard, pure)

import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Variant (match)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

catalogue :: Ref (Array { "Name" :: String, "Surname" :: String })
catalogue = unsafePerformEffect $ Ref.new
  [ { "Name": "Hans", "Surname": "Emil" }
  , { "Name": "Max", "Surname": "Mustermann" }
  , { "Name": "Roman", "Surname": "Tisch" }
  ]

loadPeopleCatalogue :: {} -> Aff { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }
loadPeopleCatalogue _ = do
  people <- readPeople catalogue
  pure { "Filter prefix (surname)": "", "Name": "", "Surname": "", people, selected: .none {} }

pick :: { event :: Int, model :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } } -> { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }
pick { event: i, model: m@{ people } } = case index people i of
  Just p -> m { selected = .picked { index: i }, "Name" = p."Name", "Surname" = p."Surname" }
  Nothing -> m

createPerson :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> Aff { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }
createPerson m@{ "Name": name, "Surname": surname, people } = (\ps -> refreshPeople ps m) <$> writePeople catalogue (snoc people { "Name": name, "Surname": surname })

updatePerson :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> Aff { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }
updatePerson m@{ "Name": name, "Surname": surname, people, selected } = (\ps -> refreshPeople ps m) <$> match
  { picked: \p -> writePeople catalogue (fromMaybe people (updateAt p.index { "Name": name, "Surname": surname } people))
  , none: \_ -> pure people
  } selected

deletePerson :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> Aff { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }
deletePerson m@{ people, selected } = (\ps -> peopleDeleted ps m) <$> match
  { picked: \p -> writePeople catalogue (fromMaybe people (deleteAt p.index people))
  , none: \_ -> pure people
  } selected

refreshPeople :: Array { "Name" :: String, "Surname" :: String } -> { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }
refreshPeople people m = m { people = people }

peopleDeleted :: Array { "Name" :: String, "Surname" :: String } -> { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }
peopleDeleted people m = m { people = people, selected = .none {} }

readPeople :: Ref (Array { "Name" :: String, "Surname" :: String }) -> Aff (Array { "Name" :: String, "Surname" :: String })
readPeople store = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.read store)

writePeople :: Ref (Array { "Name" :: String, "Surname" :: String }) -> Array { "Name" :: String, "Surname" :: String } -> Aff (Array { "Name" :: String, "Surname" :: String })
writePeople store people = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.write people store)
  readPeople store

entries :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> Array { "Name" :: String, "Surname" :: String, key :: Int, status :: [ selected :: {}, unselected :: {} ] }
entries { "Filter prefix (surname)": prefix, selected, people } =
  (\{ i, p } -> { key: i, "Name": p."Name", "Surname": p."Surname", status: statusOf i })
    <$> filter (\{ p } -> hasPrefix prefix p."Surname") (mapWithIndex (\i p -> { i, p }) people)
  where
  statusOf i = match { picked: \p -> if p.index == i then .selected {} else .unselected {}, none: \_ -> .unselected {} } selected
  hasPrefix start s = isJust (stripPrefix (Pattern start) s)

personLine :: { "Name" :: String, "Surname" :: String, key :: Int, status :: [ selected :: {}, unselected :: {} ] } -> String
personLine { "Name": name, "Surname": surname } = surname <> ", " <> name

isSelected :: { "Name" :: String, "Surname" :: String, key :: Int, status :: [ selected :: {}, unselected :: {} ] } -> Boolean
isSelected { status } = match { selected: \_ -> true, unselected: \_ -> false } status
