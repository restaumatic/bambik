module Crud (createPerson, deletePerson, entries, isSelected, loadPeopleCatalogue, personLine, pick, updatePerson) where

import Prelude (bind, otherwise, pure, (<$>), (<>), (==), (||))

import Crud.PeopleServer (fetchPeople, storePeople)
import Data.Array (deleteAt, filter, index, mapWithIndex, snoc, updateAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Variant (match)
import Effect.Aff (Aff)

loadPeopleCatalogue
  :: {}
  -> Aff [ "People loaded" :: { "Filter" :: String
                              , "Name" :: String
                              , "Surname" :: String
                              , people :: Array { "Name" :: String, "Surname" :: String }
                              , selected :: [ none :: {}, picked :: { index :: Int } ]
                              }
         ]
loadPeopleCatalogue _ = do
  people <- fetchPeople
  pure (."People loaded" { "Filter": "", "Name": "", "Surname": "", people, selected: .none {} })

pick
  :: { event :: Int
     , model :: { "Filter" :: String
                , "Name" :: String
                , "Surname" :: String
                , people :: Array { "Name" :: String, "Surname" :: String }
                , selected :: [ none :: {}, picked :: { index :: Int } ]
                }
     }
  -> { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
pick { event: i, model: m@{ people } } = case index people i of
  Just p -> m { selected = .picked { index: i }, "Name" = p."Name", "Surname" = p."Surname" }
  Nothing -> m

createPerson
  :: { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
  -> Aff [ "Person created" :: { "Filter" :: String
                               , "Name" :: String
                               , "Surname" :: String
                               , people :: Array { "Name" :: String, "Surname" :: String }
                               , selected :: [ none :: {}, picked :: { index :: Int } ]
                               }
         , "Person not created" :: { "Filter" :: String
                                   , "Name" :: String
                                   , "Surname" :: String
                                   , people :: Array { "Name" :: String, "Surname" :: String }
                                   , selected :: [ none :: {}, picked :: { index :: Int } ]
                                   }
         ]
createPerson m@{ "Name": name, "Surname": surname, people }
  | name == "" || surname == "" = pure (."Person not created" m)
  | otherwise = (\ps -> ."Person created" (refreshPeople ps m)) <$> storePeople (snoc people { "Name": name, "Surname": surname })

updatePerson
  :: { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
  -> Aff [ "Person not updated" :: { "Filter" :: String
                                   , "Name" :: String
                                   , "Surname" :: String
                                   , people :: Array { "Name" :: String, "Surname" :: String }
                                   , selected :: [ none :: {}, picked :: { index :: Int } ]
                                   }
         , "Person updated" :: { "Filter" :: String
                               , "Name" :: String
                               , "Surname" :: String
                               , people :: Array { "Name" :: String, "Surname" :: String }
                               , selected :: [ none :: {}, picked :: { index :: Int } ]
                               }
         ]
updatePerson m@{ "Name": name, "Surname": surname, people, selected } = match
  { picked: \p -> (\ps -> ."Person updated" (refreshPeople ps m)) <$> storePeople (fromMaybe people (updateAt p.index { "Name": name, "Surname": surname } people))
  , none: \_ -> pure (."Person not updated" m)
  } selected

deletePerson
  :: { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
  -> Aff [ "Person deleted" :: { "Filter" :: String
                               , "Name" :: String
                               , "Surname" :: String
                               , people :: Array { "Name" :: String, "Surname" :: String }
                               , selected :: [ none :: {}, picked :: { index :: Int } ]
                               }
         , "Person not deleted" :: { "Filter" :: String
                                   , "Name" :: String
                                   , "Surname" :: String
                                   , people :: Array { "Name" :: String, "Surname" :: String }
                                   , selected :: [ none :: {}, picked :: { index :: Int } ]
                                   }
         ]
deletePerson m@{ people, selected } = match
  { picked: \p -> (\ps -> ."Person deleted" (peopleDeleted ps m)) <$> storePeople (fromMaybe people (deleteAt p.index people))
  , none: \_ -> pure (."Person not deleted" m)
  } selected

refreshPeople
  :: Array { "Name" :: String, "Surname" :: String }
  -> { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
  -> { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
refreshPeople people m = m { people = people }

peopleDeleted
  :: Array { "Name" :: String, "Surname" :: String }
  -> { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
  -> { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
peopleDeleted people m = m { people = people, selected = .none {} }

entries
  :: { "Filter" :: String
     , "Name" :: String
     , "Surname" :: String
     , people :: Array { "Name" :: String, "Surname" :: String }
     , selected :: [ none :: {}, picked :: { index :: Int } ]
     }
  -> Array { "Name" :: String
           , "Surname" :: String
           , key :: Int
           , status :: [ selected :: {}, unselected :: {} ]
           }
entries { "Filter": prefix, selected, people } =
  (\{ i, p } -> { key: i, "Name": p."Name", "Surname": p."Surname", status: statusOf i })
    <$> filter (\{ p } -> hasPrefix prefix p."Surname") (mapWithIndex (\i p -> { i, p }) people)
  where
  statusOf i = match { picked: \p -> if p.index == i then .selected {} else .unselected {}, none: \_ -> .unselected {} } selected
  hasPrefix start s = isJust (stripPrefix (Pattern start) s)

personLine
  :: { "Name" :: String
     , "Surname" :: String
     , key :: Int
     , status :: [ selected :: {}, unselected :: {} ]
     }
  -> String
personLine { "Name": name, "Surname": surname } = surname <> ", " <> name

isSelected
  :: { "Name" :: String
     , "Surname" :: String
     , key :: Int
     , status :: [ selected :: {}, unselected :: {} ]
     }
  -> Boolean
isSelected { status } = match { selected: \_ -> true, unselected: \_ -> false } status

