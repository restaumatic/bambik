module ReorderLogic (openingSetlist, rotateAction, setOrder, shuffleAction) where

import Prelude ((<$>), bind, compare, map, pure)

import Data.Array (snoc, sortBy, uncons)
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

openingSetlist :: { order :: Array { id :: String, "Title" :: String } }
openingSetlist =
  { order:
      [ { id: "t1", "Title": "Track 1" }
      , { id: "t2", "Title": "Track 2" }
      , { id: "t3", "Title": "Track 3" }
      , { id: "t4", "Title": "Track 4" }
      ]
  }

rotateAction
  :: { order :: Array { id :: String, "Title" :: String } }
  -> Aff [ reordered :: Array { id :: String, "Title" :: String } ]
rotateAction { order } = pure (.reordered (rotate { order }))

shuffleAction
  :: { order :: Array { id :: String, "Title" :: String } }
  -> Aff [ reordered :: Array { id :: String, "Title" :: String } ]
shuffleAction { order } = liftEffect (.reordered <$> shuffleOrder order)

rotate :: { order :: Array { id :: String, "Title" :: String } } -> Array { id :: String, "Title" :: String }
rotate { order } = maybe order (\{ head, tail } -> snoc tail head) (uncons order)

setOrder
  :: Array { id :: String, "Title" :: String }
  -> { order :: Array { id :: String, "Title" :: String } }
  -> { order :: Array { id :: String, "Title" :: String } }
setOrder order pl = pl { order = order }

shuffleOrder :: Array { id :: String, "Title" :: String } -> Effect (Array { id :: String, "Title" :: String })
shuffleOrder tracks = do
  keyed <- traverse withKey tracks
  pure (map snd (sortBy (\a b -> compare (fst a) (fst b)) keyed))
  where
  withKey t = do
    k <- randomInt 0 1000000
    pure (Tuple k t)
