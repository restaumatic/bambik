module PeopleServer (fetchPeople, storePeople) where

import Prelude (discard, ($))

import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

storedPeople :: Ref (Array { "Name" :: String, "Surname" :: String })
storedPeople = unsafePerformEffect $ Ref.new
  [ { "Name": "Hans", "Surname": "Emil" }
  , { "Name": "Max", "Surname": "Mustermann" }
  , { "Name": "Roman", "Surname": "Tisch" }
  ]

fetchPeople :: Aff (Array { "Name" :: String, "Surname" :: String })
fetchPeople = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.read storedPeople)

storePeople :: Array { "Name" :: String, "Surname" :: String } -> Aff (Array { "Name" :: String, "Surname" :: String })
storePeople people = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.write people storedPeople)
  fetchPeople
