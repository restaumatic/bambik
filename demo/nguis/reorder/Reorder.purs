module Reorder (reorder) where

import Prelude ((#), ($), (<$>), (>>>), Unit, bind, compare, map, pure)

import Data.Array (snoc, sortBy, uncons)
import Data.Maybe (maybe)
import Data.Profunctor.Row.RecordToRecord (pempty)
import Data.Profunctor.Row.RecordToRecord as RecordToRecord
import Data.Profunctor.Row.RecordToVariant as RecordToVariant
import Data.Profunctor.Row.VariantToVariant as VariantToVariant
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Variant (match)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import PUI (action, asCase, asField, completed, edits, field, mvu, onCase, silence, updates)
import PUI.HTML (body, el, li, ul, (:=))
import PUI.MDC (button, card, cardActions, elevation20, filledTextField)
import QualifiedDo.Semigroupoid as Semigroupoid

type Track = { id :: String, title :: String }
type Playlist = { order :: Array Track }

reorder :: Effect Unit
reorder =
  body $
    elevation20 $
      card { caption: "Reorder" } $ ( Semigroupoid.do
          ( Semigroupoid.do
              cardActions $ RecordToVariant.do
                button { label: "Rotate", icon: "sync" } # asCase @"rotate"
                button { label: "Shuffle", icon: "shuffle" } # asCase @"shuffle"
              VariantToVariant.do
                silence # action rotateAction # onCase @"rotate"
                silence # action shuffleAction # onCase @"shuffle") # updates (match { reordered: setOrder })
          ul
            ( ( li >>> "style" := "display: flex; gap: 10px; align-items: center; list-style: none; margin: 6px 0;" $ ( RecordToRecord.do
                  el "input" >>> "type" := "checkbox" $ pempty
                  filledTextField { floatingLabel: "Title" } # asField @"title") # completed) # edits _.id) # field @"order"
      ) # mvu openingSetlist

rotateAction :: Playlist -> Aff [ reordered :: Array Track ]
rotateAction pl = pure (.reordered (rotate pl))

shuffleAction :: Playlist -> Aff [ reordered :: Array Track ]
shuffleAction pl = liftEffect (.reordered <$> shuffleOrder pl.order)

rotate :: Playlist -> Array Track
rotate pl = maybe pl.order (\{ head, tail } -> snoc tail head) (uncons pl.order)

setOrder :: Array Track -> Playlist -> Playlist
setOrder order pl = pl { order = order }

shuffleOrder :: Array Track -> Effect (Array Track)
shuffleOrder tracks = do
  keyed <- traverse withKey tracks
  pure (map snd (sortBy (\a b -> compare (fst a) (fst b)) keyed))
  where
  withKey t = do
    k <- randomInt 0 1000000
    pure (Tuple k t)

openingSetlist :: Playlist
openingSetlist =
  { order:
      [ { id: "t1", title: "Track 1" }
      , { id: "t2", title: "Track 2" }
      , { id: "t3", title: "Track 3" }
      , { id: "t4", title: "Track 4" }
      ]
  }
