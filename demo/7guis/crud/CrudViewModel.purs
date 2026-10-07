module CrudViewModel (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, peopleLoadedLine, personCreatedLine, personDeletedLine, personLine, personNotCreatedLine, personNotDeletedLine, personNotUpdatedLine, personPickedLine, personUpdatedLine, pick, updatePerson) where

import Prelude ((<$>), (<>), (==), (||), ($), bind, discard, otherwise, pure, show)

import Data.Array (deleteAt, filter, index, length, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
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

loadPeopleCatalogue :: {} -> Aff [ "People loaded" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } ]
loadPeopleCatalogue _ = do
  people <- readPeople catalogue
  pure (."People loaded" { "Filter prefix (surname)": "", "Name": "", "Surname": "", people, selected: .none {} })

pick :: { event :: Int, model :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } } -> { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }
pick { event: i, model: m@{ people } } = case index people i of
  Just p -> m { selected = .picked { index: i }, "Name" = p."Name", "Surname" = p."Surname" }
  Nothing -> m

createPerson :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> Aff [ "Person created" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person deleted" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not created" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not deleted" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not updated" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person updated" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } ]
createPerson m@{ "Name": name, "Surname": surname, people }
  | name == "" || surname == "" = pure (."Person not created" m)
  | otherwise = (\ps -> ."Person created" (refreshPeople ps m)) <$> writePeople catalogue (snoc people { "Name": name, "Surname": surname })

updatePerson :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> Aff [ "Person created" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person deleted" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not created" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not deleted" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not updated" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person updated" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } ]
updatePerson m@{ "Name": name, "Surname": surname, people, selected } = match
  { picked: \p -> (\ps -> ."Person updated" (refreshPeople ps m)) <$> writePeople catalogue (fromMaybe people (updateAt p.index { "Name": name, "Surname": surname } people))
  , none: \_ -> pure (."Person not updated" m)
  } selected

deletePerson :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> Aff [ "Person created" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person deleted" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not created" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not deleted" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person not updated" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] }, "Person updated" :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } ]
deletePerson m@{ people, selected } = match
  { picked: \p -> (\ps -> ."Person deleted" (peopleDeleted ps m)) <$> writePeople catalogue (fromMaybe people (deleteAt p.index people))
  , none: \_ -> pure (."Person not deleted" m)
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

peopleLoadedLine :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> String
peopleLoadedLine { people } = "Loaded " <> show (length people) <> " people"

personPickedLine :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> String
personPickedLine { people, selected } = match
  { picked: \p -> maybe "Picked nobody" (\q -> "Picked " <> q."Name" <> " " <> q."Surname") (index people p.index)
  , none: \_ -> "Picked nobody"
  } selected

personCreatedLine :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> String
personCreatedLine { "Name": name, "Surname": surname } = "Created " <> name <> " " <> surname

personUpdatedLine :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> String
personUpdatedLine { "Name": name, "Surname": surname } = "Updated " <> name <> " " <> surname

personDeletedLine :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> String
personDeletedLine { "Name": name, "Surname": surname } = "Deleted " <> name <> " " <> surname

personNotCreatedLine :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> String
personNotCreatedLine _ = "Not created: a name and a surname are needed"

personNotUpdatedLine :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> String
personNotUpdatedLine _ = "Not updated: nobody is selected"

personNotDeletedLine :: { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ none :: {}, picked :: { index :: Int } ] } -> String
personNotDeletedLine _ = "Not deleted: nobody is selected"
