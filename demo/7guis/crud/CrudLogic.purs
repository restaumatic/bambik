module CrudLogic (createPerson, deletePerson, entries, isSelected, personLine, loadPeopleCatalogue, peopleDeleted, pick, refreshPeople, updatePerson) where

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

loadPeopleCatalogue :: forall r1. { | r1 } -> Aff { "Filter prefix (surname)" :: String, "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] }
loadPeopleCatalogue _ = do
  people <- readPeople catalogue
  pure { "Filter prefix (surname)": "", "Name": "", "Surname": "", people, selected: .none {} }

pick :: forall r1. Int -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ], "Name" :: String, "Surname" :: String | r1 } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ], "Name" :: String, "Surname" :: String | r1 }
pick i m@{ people } = case index people i of
  Just p -> m { selected = .picked { index: i }, "Name" = p."Name", "Surname" = p."Surname" }
  Nothing -> m

createPerson :: forall r1. { "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String } | r1 } -> Aff (Array { "Name" :: String, "Surname" :: String })
createPerson { "Name": name, "Surname": surname, people } = writePeople catalogue (snoc people { "Name": name, "Surname": surname })

updatePerson :: forall r1. { "Name" :: String, "Surname" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] | r1 } -> Aff (Array { "Name" :: String, "Surname" :: String })
updatePerson { "Name": name, "Surname": surname, people, selected } = match
  { picked: \p -> writePeople catalogue (fromMaybe people (updateAt p.index { "Name": name, "Surname": surname } people))
  , none: \_ -> pure people
  } selected

deletePerson :: forall r1. { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] | r1 } -> Aff (Array { "Name" :: String, "Surname" :: String })
deletePerson { people, selected } = match
  { picked: \p -> writePeople catalogue (fromMaybe people (deleteAt p.index people))
  , none: \_ -> pure people
  } selected

refreshPeople :: forall r1. Array { "Name" :: String, "Surname" :: String } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] | r1 } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] | r1 }
refreshPeople people m = m { people = people }

peopleDeleted :: forall r1. Array { "Name" :: String, "Surname" :: String } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] | r1 } -> { people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] | r1 }
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

entries :: forall r1. { "Filter prefix (surname)" :: String, people :: Array { "Name" :: String, "Surname" :: String }, selected :: [ picked :: { index :: Int }, none :: {} ] | r1 } -> Array { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] }
entries { "Filter prefix (surname)": prefix, selected, people } =
  (\{ i, p } -> { key: i, "Name": p."Name", "Surname": p."Surname", status: statusOf i })
    <$> filter (\{ p } -> hasPrefix prefix p."Surname") (mapWithIndex (\i p -> { i, p }) people)
  where
  statusOf i = match { picked: \p -> if p.index == i then .selected {} else .unselected {}, none: \_ -> .unselected {} } selected
  hasPrefix start s = isJust (stripPrefix (Pattern start) s)

personLine :: forall r1. { "Name" :: String, "Surname" :: String | r1 } -> String
personLine { "Name": name, "Surname": surname } = surname <> ", " <> name

isSelected :: forall r1. { key :: Int, "Name" :: String, "Surname" :: String, status :: [ selected :: {}, unselected :: {} ] | r1 } -> Boolean
isSelected { status } = match { selected: \_ -> true, unselected: \_ -> false } status
