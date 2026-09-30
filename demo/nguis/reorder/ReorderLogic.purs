module ReorderLogic (openingSetlist, rotateAction, setOrder, shuffleAction) where

import Prelude (bind, compare, map, pure)

import Data.Array (snoc, sortBy, uncons)
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

openingSetlist :: { "Setlist" :: Array { id :: String, "Title" :: String } }
openingSetlist =
  { "Setlist":
    [ { id: "t1", "Title": "Track 1" }
    , { id: "t2", "Title": "Track 2" }
    , { id: "t3", "Title": "Track 3" }
    , { id: "t4", "Title": "Track 4" }
    ]
  }

rotateAction :: forall r1. { "Setlist" :: Array { id :: String, "Title" :: String } | r1 } -> Aff (Array { id :: String, "Title" :: String })
rotateAction { "Setlist": tracks } = pure (rotate tracks)

shuffleAction :: forall r1. { "Setlist" :: Array { id :: String, "Title" :: String } | r1 } -> Aff (Array { id :: String, "Title" :: String })
shuffleAction { "Setlist": tracks } = liftEffect (shuffleOrder tracks)

rotate :: Array { id :: String, "Title" :: String } -> Array { id :: String, "Title" :: String }
rotate tracks = maybe tracks (\{ head, tail } -> snoc tail head) (uncons tracks)

setOrder :: forall r1. Array { id :: String, "Title" :: String } -> { "Setlist" :: Array { id :: String, "Title" :: String } | r1 } -> { "Setlist" :: Array { id :: String, "Title" :: String } | r1 }
setOrder tracks pl = pl { "Setlist" = tracks }

shuffleOrder :: Array { id :: String, "Title" :: String } -> Effect (Array { id :: String, "Title" :: String })
shuffleOrder tracks = do
  keyed <- traverse withKey tracks
  pure (map snd (sortBy (\a b -> compare (fst a) (fst b)) keyed))
  where
  withKey t = do
    k <- randomInt 0 1000000
    pure (Tuple k t)
