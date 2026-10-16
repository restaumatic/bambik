module Crud.PeopleServer (Person, fetchPeople, storePeople) where

import Prelude (discard, ($))

import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)

type Person = { "Name" :: String, "Surname" :: String }

storedPeople :: Ref (Array Person)
storedPeople = unsafePerformEffect $ Ref.new
  [ { "Name": "Hans", "Surname": "Emil" }
  , { "Name": "Max", "Surname": "Mustermann" }
  , { "Name": "Roman", "Surname": "Tisch" }
  ]

fetchPeople :: Aff (Array Person)
fetchPeople = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.read storedPeople)

storePeople :: Array Person -> Aff (Array Person)
storePeople people = do
  delay (Milliseconds 300.0)
  liftEffect (Ref.write people storedPeople)
  fetchPeople
