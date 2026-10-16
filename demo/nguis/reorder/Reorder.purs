module Reorder (Setlist, Track, openingSetlist, rotateSetlist, shuffleAction) where

import Prelude (bind, compare, map, pure, (<$>))

import Data.Array (snoc, sortBy, uncons)
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

type Setlist = { "Setlist" :: Array Track }

type Track = { "Title" :: String, id :: String }

openingSetlist :: Setlist
openingSetlist =
  { "Setlist":
    [ { id: "t1", "Title": "Track 1" }
    , { id: "t2", "Title": "Track 2" }
    , { id: "t3", "Title": "Track 3" }
    , { id: "t4", "Title": "Track 4" }
    ]
  }

rotateSetlist :: Setlist -> Setlist
rotateSetlist pl@{ "Setlist": tracks } = pl { "Setlist" = rotate tracks }

shuffleAction :: Setlist -> Aff [ "Setlist shuffled" :: Setlist ]
shuffleAction pl@{ "Setlist": tracks } = (\ts -> ."Setlist shuffled" (pl { "Setlist" = ts })) <$> liftEffect (shuffleOrder tracks)

rotate :: Array Track -> Array Track
rotate tracks = maybe tracks (\{ head, tail } -> snoc tail head) (uncons tracks)

shuffleOrder :: Array Track -> Effect (Array Track)
shuffleOrder tracks = do
  keyed <- traverse withKey tracks
  pure (map snd (sortBy (\a b -> compare (fst a) (fst b)) keyed))
  where
  withKey t = do
    k <- randomInt 0 1000000
    pure (Tuple k t)
